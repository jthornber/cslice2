{-# LANGUAGE TemplateHaskell #-}

module C.Translate (
            toHir
    ) where


import Debug.Trace

import qualified C.AST as AST
import C.HIR
import C.Identifier
import C.Int
import C.SymbolTable (SymbolTable, Symbol(..), Scope)
import C.TypeCheck
import qualified C.SymbolTable as ST

import Control.Lens hiding (element, op)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

---------------------------------------------------------------------
-- Monad
data TranslateState = TranslateState {
    _tsSymbolTable :: SymbolTable,
    _tsUIDCount :: Int,
    _tsVarTypes :: Map Symbol Type,
    _tsStructDetails :: Map Symbol [StructEntry]
}

makeLenses ''TranslateState

data TranslateError = TranslateError String
    deriving (Show)

type Translate = ExceptT TranslateError (State TranslateState)

emptyTS :: TranslateState
emptyTS = TranslateState ST.empty 0 M.empty M.empty

runTranslate :: Translate a -> Either String a
runTranslate x = case evalState (runExceptT x) emptyTS of
    (Left (TranslateError msg)) -> Left msg
    (Right x') -> Right x'

----------------------------------
-- Lookup entries in symbol table

mkSym :: Identifier -> Translate Symbol
mkSym nm = do
    ts <- get
    let r = Symbol (view tsUIDCount ts) nm
    put $ over tsUIDCount (+ 1) ts
    pure r

anonSym :: Translate Symbol
anonSym = mkSym $ Identifier ""

mkRef :: (Identifier -> SymbolTable -> Maybe Symbol) -> String ->
         Identifier -> Translate Symbol
mkRef fn namespace nm = do
    s <- view tsSymbolTable <$> get
    case fn nm s of
        Nothing -> barf $
            "Couldn't find reference to '" ++ show nm ++ "' in " ++ namespace ++ " scope"
        (Just sym) -> pure sym

refFun :: Identifier -> Translate Symbol
refFun = mkRef ST.refFun "function"

refStruct :: Identifier -> Translate Symbol
refStruct = mkRef ST.refStruct "struct"

mrefStruct :: Identifier -> Translate (Maybe Symbol)
mrefStruct nm = ST.refStruct nm . view tsSymbolTable <$> get

refEnum :: Identifier -> Translate Symbol
refEnum = mkRef ST.refEnum "enum"

refEnumElt :: Identifier -> Translate Symbol
refEnumElt = mkRef ST.refVar "var (enum elt)"

refLabel :: Identifier -> Translate Symbol
refLabel nm = do
    ts <- get
    case ST.refLabel nm (view tsSymbolTable ts) of
        Nothing -> barf ("couldn't find label: " ++ show nm)
        (Just v) -> pure v

refVar :: Identifier -> Translate Symbol
refVar = mkRef ST.refVar "var"

refTypedef :: Identifier -> Translate Symbol
refTypedef = mkRef ST.refTypedef "typedef"

structName' :: RawType -> Translate Symbol
structName' (TyStruct _ sym _) = pure sym
structName' (TyStructRef _ sym) = pure sym
structName' _ = barf "not a struct"

structField' :: [StructEntry] -> Identifier -> Maybe Symbol
structField' fields nm = firstJust checkEntry fields
    where
        checkEntry (StructEntry _ (Just sym@(Symbol _ nm')) _) | nm == nm' = Just sym
        checkEntry _ = Nothing

        firstJust f = listToMaybe . mapMaybe f

refStructElt :: Type -> Identifier -> Translate Symbol
refStructElt (Type rt _) nm = do
    ts <- get
    structSym <- structName' rt
    let sd = view tsStructDetails ts
    case M.lookup structSym sd of
        Nothing -> barf $ "struct isn't registered: " ++ show structSym
        (Just fields) -> case structField' fields nm of
            Nothing -> barf $ "invalid struct field: " ++ show structSym ++ "." ++ show nm
            (Just fieldSym) -> pure fieldSym

----------------------------------
-- Define entries in symbol table

mkDef :: (Symbol -> SymbolTable -> Maybe SymbolTable) ->
         String -> Identifier -> Translate Symbol
mkDef fn namespace nm = do
    sym <- mkSym nm
    ts <- get
    case fn sym (view tsSymbolTable ts) of
        Just st' -> do
            put $ over tsSymbolTable (const st') ts
            pure sym
        Nothing -> barf $ "identifier already present in " ++ namespace ++ " scope: " ++ show nm

defFun :: Identifier -> Translate Symbol
defFun = mkDef ST.defFun "function"

defStruct' :: Identifier -> Translate Symbol
defStruct' = mkDef ST.defStruct "struct"

defStruct :: Maybe [StructEntry] -> Identifier -> Translate Symbol
defStruct (Just fields) nm = do
    msym <- mrefStruct nm
    case msym of
        Nothing -> do
            sym <- defStruct' nm
            modify $ over tsStructDetails (M.insert sym fields)
            pure sym
        Just sym -> do
            modify $ over tsStructDetails (M.insert sym fields)
            pure sym
defStruct Nothing nm = defStruct' nm

defEnum :: Identifier -> Translate Symbol
defEnum = mkDef ST.defEnum "enum"

defLabel :: Identifier -> Translate Symbol
defLabel nm = do
    sym <- mkSym nm
    ts <- get
    case ST.defLabel sym (view tsSymbolTable ts) of
        Just st' -> do
            put $ over tsSymbolTable (const st') ts
            pure sym
        Nothing -> barf $ "multiply defined label"

defVar :: Identifier -> Type -> Translate Symbol
defVar nm ty = do
    sym <- mkDef ST.defVar "var" nm
    modify $ over tsVarTypes (M.insert sym ty)
    pure sym

rmVar :: Identifier -> Translate ()
rmVar nm = do
    ts <- get
    case ST.rmVar nm (view tsSymbolTable ts) of
        (Just st') -> put $ over tsSymbolTable (const st') ts
        Nothing -> barf $ "asked to remove symbol that is not present: " ++ show nm

defTypedef :: Identifier -> Translate Symbol
defTypedef = mkDef ST.defTypedef "typedef"

----------------------------------
-- Push and pop frames in the symbol table

pushScope :: Scope -> Translate ()
pushScope sc = do
    ts <- get
    put $ over tsSymbolTable (ST.enterScope sc) ts

popScope :: Translate ()
popScope = do
    ts <- get
    case ST.leaveScope (view tsSymbolTable ts) of
        Nothing -> internalError "no scope to leave"
        (Just st') -> put $ over tsSymbolTable (const st') ts

withScope :: Scope -> Translate a -> Translate a
withScope sc m = do
    pushScope sc
    r <- m
    popScope
    pure r

typeVar :: Symbol -> Translate Type
typeVar sym = do
    ts <- get
    case M.lookup sym $ view tsVarTypes ts of
        Nothing -> barf $ "unable to retrieve type for variable: " ++ show sym
        (Just ty) -> pure ty

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn xs = do
    xs' <- mapM fn xs
    pure $ concat xs'

maybeT :: (a -> Translate b) -> Maybe a -> Translate (Maybe b)
maybeT _ Nothing = pure Nothing
maybeT fn (Just x) = Just <$> fn x

barf :: String -> Translate a
barf txt = throwError (TranslateError txt)

internalError :: String -> Translate a
internalError txt = barf $ "internal error: " ++ txt

---------------------------------------------------------------------
-- Expression translation

xBinOp :: AST.BinOp -> Translate BinOp
xBinOp AST.LOGICAL_OR = pure LOGICAL_OR
xBinOp AST.LOGICAL_AND = pure LOGICAL_AND
xBinOp AST.BIT_OR = pure BIT_OR
xBinOp AST.BIT_AND = pure BIT_AND
xBinOp AST.XOR = pure XOR
xBinOp AST.EQ = pure C.HIR.EQ
xBinOp AST.NEQ = pure NEQ
xBinOp AST.LT = pure C.HIR.LT
xBinOp AST.LTE = pure LTE
xBinOp AST.GT = pure C.HIR.GT
xBinOp AST.GTE = pure GTE
xBinOp AST.LSHIFT = pure LSHIFT
xBinOp AST.RSHIFT = pure RSHIFT
xBinOp AST.PLUS = pure PLUS
xBinOp AST.MINUS = pure MINUS
xBinOp AST.MULT = pure MULT
xBinOp AST.DIV = pure DIV
xBinOp AST.MOD = pure MOD

xUnOp :: AST.UnaryOp -> Translate UnaryOp
xUnOp AST.ADDRESS_OF = pure ADDRESS_OF
xUnOp AST.LABEL_ADDRESS = pure LABEL_ADDRESS
xUnOp AST.DEREF = pure DEREF
xUnOp AST.UNARY_PLUS = pure UNARY_PLUS
xUnOp AST.UNARY_MINUS = pure UNARY_MINUS
xUnOp AST.BIT_NOT = pure BIT_NOT
xUnOp AST.LOGICAL_NOT = pure LOGICAL_NOT
xUnOp AST.PRE_INC = pure PRE_INC
xUnOp AST.POST_INC = pure POST_INC
xUnOp AST.PRE_DEC = pure PRE_DEC
xUnOp AST.POST_DEC = pure POST_DEC

binAssign :: BinOp -> AST.Exp -> AST.Exp -> Translate Exp
binAssign op e1 e2 = do
    e1'@(Exp t1 _) <- xExpression e1
    e2'@(Exp t2 _) <- xExpression e2
    let t3 = usualArithmeticConversion t1 t2
    pure $ Exp t1 (AssignExp e1' (Exp t3 (BinaryExp op e1' e2')))

xTypeName :: AST.TypeName -> Translate Type
xTypeName (AST.TypeName specs Nothing) = do
    rty <- xTypeSpecifier ts
    cvr <- scanTypeQualifiers tq
    pure $ Type rty cvr
    where
        (_, _, ts, tq, _, _) = dsSplit $ map toDeclSpec specs

xTypeName (AST.TypeName specs (Just adecl)) = do
    rty <- xTypeSpecifier ts
    cvr <- scanTypeQualifiers tq
    applyAbstractDeclarator adecl (Type rty cvr)
    where
        (_, _, ts, tq, _, _) = dsSplit $ map toDeclSpec specs

applyAbstractDeclarator :: AST.AbstractDeclarator -> Type -> Translate Type
applyAbstractDeclarator (AST.AbstractPointer ptr) ty = mkPtr ptr ty
applyAbstractDeclarator (AST.AbstractDeclarator Nothing dad) ty =
    applyDirectAbstractDeclarator dad ty
applyAbstractDeclarator (AST.AbstractDeclarator (Just ptr) dad) ty = do
    ty' <- applyDirectAbstractDeclarator dad ty
    mkPtr ptr ty'

applyDirectAbstractDeclarator :: AST.DirectAbstractDeclarator -> Type -> Translate Type
applyDirectAbstractDeclarator (AST.DANested ad) ty = applyAbstractDeclarator ad ty
applyDirectAbstractDeclarator (AST.DAArray mdad tq me isStatic) ty = undefined
applyDirectAbstractDeclarator (AST.DAArrayStar _) ty = undefined

applyDirectAbstractDeclarator (AST.DAFun Nothing params) ty = do
    params' <- convertParams params
    pure $ Type (TyFunction (FunType ty params' S.empty)) S.empty

-- FIXME: finish
applyDirectAbstractDeclarator (AST.DAFun (Just _) params) ty = do
    params' <- convertParams params
    pure $ Type (TyFunction (FunType ty params' S.empty)) S.empty

xExpression :: AST.Exp -> Translate Exp
xExpression (AST.VarExp nm) = do
    sym <- refVar nm
    t <- typeVar sym
    pure . Exp t . VarExp $ sym

xExpression (AST.IntConstExp s ty n) = do
    pure . Exp (Type (TyInt s ty) S.empty) . LiteralExp . IntValue $ n

xExpression (AST.StringConstExp str) =
    pure . Exp constCharStar . LiteralExp . StringValue $ str
    where
        -- FIXME: write some combinators to make writing types out easier
        constCharStar = Type (TyPointer (Type (TyInt SIGNED CHAR) (S.fromList [CONST]))) S.empty

xExpression (AST.CharConstExp c) =
    pure . Exp constChar . LiteralExp . CharValue . head $ c
    where
        constChar = Type (TyInt SIGNED CHAR) (S.fromList [CONST])

xExpression (AST.CompoundLiteral tn inits) = do
    ty <- xTypeName tn
    fields <- mapM xInitializerPair inits
    pure . Exp ty $ LiteralExp (CompoundValue ty fields)

xExpression (AST.SubscriptExp e1 e2) = do
    e1'@(Exp t _) <- xExpression e1
    e2' <- xExpression e2
    pure . Exp (arrayEltType t) $ SubscriptExp e1' e2'

xExpression (AST.FuncallExp e params) = do
    fun@(Exp t _) <- xExpression e
    params' <- mapM xExpression params
    pure . Exp (returnType t) $ FuncallExp fun params'

xExpression (AST.StructElt e n) = do
    e'@(Exp t _) <- xExpression e
    sym <- refStructElt t n
    pure . Exp (structEltType t sym) $ StructElt e' sym

xExpression (AST.StructDeref e n) = do
    e'@(Exp t _) <- xExpression (traceIt e)
    sym <- refVar n
    pure . Exp (structEltType (deref t) sym) $ StructDeref e' sym

xExpression (AST.CommaExp e1 e2) = do
    e1' <- xExpression e1
    e2'@(Exp t _) <- xExpression e2
    pure . Exp t $ CommaExp e1' e2'

xExpression (AST.AssignExp AST.ASSIGN e1 e2) = do
    e1'@(Exp t _) <- xExpression e1
    e2' <- xExpression e2
    pure . Exp t $ AssignExp e1' e2'

xExpression (AST.AssignExp AST.MULT_ASSIGN e1 e2) =
    binAssign MULT e1 e2

xExpression (AST.AssignExp AST.DIV_ASSIGN e1 e2) =
    binAssign DIV e1 e2

xExpression (AST.AssignExp AST.MOD_ASSIGN e1 e2) =
    binAssign MOD e1 e2

xExpression (AST.AssignExp AST.PLUS_ASSIGN e1 e2) =
    binAssign PLUS e1 e2

xExpression (AST.AssignExp AST.MINUS_ASSIGN e1 e2) =
    binAssign MINUS e1 e2

xExpression (AST.AssignExp AST.LSHIFT_ASSIGN e1 e2) =
    binAssign LSHIFT e1 e2

xExpression (AST.AssignExp AST.RSHIFT_ASSIGN e1 e2) =
    binAssign RSHIFT e1 e2

xExpression (AST.AssignExp AST.AND_ASSIGN e1 e2) =
    binAssign BIT_AND e1 e2

xExpression (AST.AssignExp AST.XOR_ASSIGN e1 e2) =
    binAssign XOR e1 e2

xExpression (AST.AssignExp AST.OR_ASSIGN e1 e2) =
    binAssign BIT_OR e1 e2

xExpression (AST.ConditionalExp e1 e2 e3) = do
    e1' <- xExpression e1
    e2'@(Exp t _) <- xExpression e2
    e3' <- xExpression e3
    pure . Exp t $ ConditionalExp e1' e2' e3'

xExpression (AST.BinaryExp op e1 e2) = do
    e1'@(Exp t1 _) <- xExpression e1
    e2'@(Exp t2 _) <- xExpression e2
    op' <- xBinOp op
    pure . Exp (usualArithmeticConversion t1 t2) $ BinaryExp op' e1' e2'

xExpression (AST.UnaryExp op e1) = do
    e1'@(Exp t _) <- xExpression e1
    op' <- xUnOp op
    pure . Exp t $ UnaryExp op' e1'

xExpression (AST.CastExp tn e) = do
    t <- xTypeName tn
    e' <- xExpression e
    pure . Exp t $ CastExp t e'

xExpression (AST.SizeofValueExp e) =
    Exp unsigned_long . SizeofValueExp <$> xExpression e
    where
        unsigned_long = Type (TyInt UNSIGNED LONG) S.empty

xExpression (AST.SizeofTypeExp tn) =
    Exp unsigned_long . SizeofTypeExp <$> xTypeName tn
    where
        unsigned_long = Type (TyInt UNSIGNED LONG) S.empty

xExpression (AST.AlignofExp tn) =
    Exp unsigned . AlignofExp <$> xTypeName tn
    where
        unsigned = Type (TyInt UNSIGNED INT) S.empty

xExpression (AST.BlockExp nms items) = do
    nms' <- mapM refVar nms
    items' <- withScope ST.ScopeBlock $ concatMapM xBlockItem items
    -- FIXME: how do we ensure the last block item is an expression with a type
    pure . Exp void' $ BlockExp nms' items'
    where
        void' = Type TyVoid S.empty

xExpression (AST.BuiltinVaArg) =
    pure $ Exp (Type TyVoid S.empty) BuiltinVaArg

xExpression (AST.BuiltinOffsetOf) =
    pure $ Exp (Type TyVoid S.empty) BuiltinOffsetOf

xExpression (AST.BuiltinTypesCompatible) =
    pure $ Exp (Type TyVoid S.empty) BuiltinTypesCompatible

xExpression (AST.BuiltinConvertVector) =
    pure $ Exp (Type TyVoid S.empty) BuiltinConvertVector

xInitializerPair :: AST.InitializerPair -> Translate InitializerPair
xInitializerPair (AST.InitializerPair Nothing i) = InitializerPair Nothing <$> xInitializer i
xInitializerPair (AST.InitializerPair (Just ds) i) = InitializerPair <$> (Just <$> mapM xDesignator ds) <*> xInitializer i

xDesignator :: AST.Designator -> Translate Designator
xDesignator (AST.SubscriptDesignator e) = SubscriptDesignator <$> xExpression e
xDesignator (AST.StructDesignator nm) = StructDesignator <$> refStruct nm

xInitializer :: AST.Initializer -> Translate Initializer
xInitializer (AST.InitAssign e) = InitAssign <$> xExpression e
xInitializer (AST.InitList pairs) = InitList <$> mapM xInitializerPair pairs

--------------------------------------------------------------
-- Declaration translation

{-
In the AST the types are split into two halves.

i) A list of DeclarationSpecifiers, which contain:
   - storage class (static, extern, auto, register)
   - Basic type info
   - const, volatile, restrict qualfiers
   - FunctionSpecifier (inline, noreturn)
   - AlignmentSpecifiers

ii) A declarator; which is a messy combination of optional identifier, pointer,
    param list and arrays.

In addition the fields of structs and unions can have a width specified
if they're a basic type.

Decoding this mess is quite tricky.  eg.  In HIR the cvr qualifiers are
applied after the fundamental type is constructed.  Also there's more info
in here than is represented by the HIR 'Type' (storage class).

Processing an AST.Declaration is going to give us:
  ([(Type, Maybe Identifier)], StorageClass, [FunctionSpecifier], Maybe AlignmentSpecifier)

Processing an AST.StructDeclaration gives us:
  ([(Type, Maybe Identifier, Int)])
-}

extractStorageClass :: [AST.StorageClass] -> Translate (Maybe StorageClass)
extractStorageClass specs = case specs of
    []  -> pure Nothing
    [x] -> getSC x
    _   -> barf $ "Too many storage classes: " ++ (show specs)
    where
        -- Typedef is a storage clas in the AST, but a separate
        -- declaration type in HIR.
        getSC AST.Typedef       = bad
        getSC AST.Extern        = pure $ Just Extern
        getSC AST.Static        = pure $ Just Static
        getSC AST.ThreadLocal   = bad
        getSC AST.Auto          = pure $ Just Auto
        getSC AST.Register      = pure $ Just Register

        bad = barf "unsupported storage class"

dsSplit :: [AST.DeclarationSpecifier] -> (Bool,
                                          [AST.StorageClass], [AST.TypeSpecifier],
                                          [AST.TypeQualifier], [AST.FunctionSpecifier],
                                          [AST.AlignmentSpecifier])
dsSplit = foldr get' (False, [], [], [], [], [])
    where
        get' (AST.DSStorageClass AST.Typedef) (_, sc, ts, tq, fs, align) = (True, sc, ts, tq, fs, align)
        get' (AST.DSStorageClass x) (td, sc, ts, tq, fs, align) = (td, x:sc, ts, tq, fs, align)
        get' (AST.DSTypeSpecifier x) (td, sc, ts, tq, fs, align) = (td, sc, x:ts, tq, fs, align)
        get' (AST.DSTypeQualifier x) (td, sc, ts, tq, fs, align) = (td, sc, ts, x:tq, fs, align)
        get' (AST.DSFunctionSpecifier x) (td, sc, ts, tq, fs, align) = (td, sc, ts, tq, x:fs, align)
        get' (AST.DSAlignmentSpecifier x) (td, sc, ts, tq, fs, align) = (td, sc, ts, tq, fs, x:align)
        get' (AST.DSAttr _) r = r

scanTypeQualifiers :: [AST.TypeQualifier] -> Translate (Set CVR)
scanTypeQualifiers = foldM insert S.empty
    where
        insert s AST.Const    = pure $ S.insert CONST s
        insert s AST.Volatile = pure $ S.insert VOLATILE s
        insert s AST.Restrict = pure $ S.insert RESTRICT s
        insert _ AST.Atomic   = barf "Atomic not supported"

xTypeSpecifier :: [AST.TypeSpecifier] -> Translate RawType
xTypeSpecifier specs = case specs of
    [] -> barf "No type specifier given"
    [x] -> getTS x
    _ -> barf $ "Too many type specifiers: " ++ (show specs)

    where
        getTS :: AST.TypeSpecifier -> Translate RawType
        getTS AST.Void                  = pure TyVoid
        getTS AST.Char                  = pure $ signed CHAR
        getTS AST.UnsignedChar          = pure $ unsigned CHAR
        getTS AST.Short                 = pure $ signed SHORT
        getTS AST.UnsignedShort         = pure $ unsigned SHORT
        getTS AST.Int                   = pure $ signed INT
        getTS AST.UnsignedInt           = pure $ unsigned INT
        getTS AST.Int128                = pure $ signed INT128
        getTS AST.UnsignedInt128        = pure $ unsigned INT128
        getTS AST.Long                  = pure $ signed LONG
        getTS AST.UnsignedLong          = pure $ unsigned LONG
        getTS AST.LongLong              = pure $ signed LONG_LONG
        getTS AST.UnsignedLongLong      = pure $ unsigned LONG_LONG
        getTS AST.Float                 = pure TyFloat
        getTS AST.Double                = pure TyDouble
        getTS AST.Bool                  = pure TyBool
        getTS AST.Complex               = barf "complex nrs not implemented"
        getTS AST.AtomicSpecifier       = barf "atomics not implemented"

        getTS (AST.StructOrUnionSpecifier st mnm Nothing) = do
            case mnm of
                Nothing -> do
                    sym <- anonSym
                    pure $ TyStructRef (xStructType st) sym
                (Just nm) -> do
                    sym <- refStruct nm
                    pure $ TyStructRef (xStructType st) sym

        getTS (AST.StructOrUnionSpecifier st mnm (Just fields)) = do
            entries <- mapM xStructEntry fields
            sym <- maybe anonSym (defStruct (Just $ concat entries)) mnm
            pure $ TyStruct (xStructType st) sym (concat entries)

        getTS (AST.EnumDefSpecifier mnm entries) = do
            entries' <- mapM xEnumEntry entries
            msym <- maybeT defEnum mnm
            pure $ TyEnum msym (Just entries')

        getTS (AST.EnumRefSpecifier nm) = do
            sym <- refEnum nm
            pure $ TyEnum (Just sym) Nothing

        getTS (AST.TSTypedefName nm) = do
            sym <- refTypedef nm
            pure $ TyAlias sym

        getTS (AST.TSTypeofExp e) =
            TyTypeofExp <$> xExpression e

        getTS (AST.TSTypeofDecl ds) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            pure (TyTypeofType $ Type rt cvr)
            where
                (_, _, ts, tq, _, _) = dsSplit ds

        signed x = TyInt SIGNED x
        unsigned x = TyInt UNSIGNED x

xEnumEntry :: AST.Enumerator -> Translate EnumEntry
xEnumEntry (AST.Enumerator nm me) = do
    sym <- defVar nm (Type (TyInt UNSIGNED INT) S.empty)
    me' <- mapM xExpression me
    pure $ EnumEntry sym me'

xStructType :: AST.StructType -> StructType
xStructType AST.Struct = Struct
xStructType AST.Union = Union

xStructDeclarator :: [AST.SpecifierQualifier] -> AST.StructDeclarator -> Translate [StructEntry]
xStructDeclarator specs (AST.StructDeclarator decl me) = do
    decl' <- xVarDeclaration (map toDeclSpec specs) decl
    case decl' of
        (VarDeclaration ty _ sym _) -> do
            width <- mapM xExpression me
            pure $ [StructEntry ty (Just sym) width]
        _ -> barf "bad struct field"
xStructDeclarator _ (AST.StructDeclaratorNoDecl e) = do
    e' <- xExpression e
    pure [StructEntryWidthOnly e']

toDeclSpec :: AST.SpecifierQualifier -> AST.DeclarationSpecifier
toDeclSpec (AST.SQTypeSpecifier ts) = AST.DSTypeSpecifier ts
toDeclSpec (AST.SQTypeQualifier tq) = AST.DSTypeQualifier tq

xStructEntry :: AST.StructDeclaration -> Translate [StructEntry]
xStructEntry (AST.StructDeclaration specs declarators) =
    concat <$> mapM (xStructDeclarator specs) declarators

xStructEntry AST.StructStaticAssert = barf "Struct static assert not supported"

applyDeclarator :: Type -> (Maybe StorageClass) -> AST.Declarator -> Translate Declaration
applyDeclarator ty sc (AST.Declarator Nothing dd) = expandDD ty sc dd
applyDeclarator ty sc (AST.Declarator (Just ptr) dd) = expandDD ty sc dd >>= expandPtr ptr

mkPtr :: AST.Pointer -> Type -> Translate Type
mkPtr (AST.Pointer tq Nothing) ty = do
    cvr <- scanTypeQualifiers tq
    pure $ Type (TyPointer ty) cvr

mkPtr (AST.Pointer tq (Just subptr)) ty = do
    ty' <- mkPtr subptr ty
    cvr <- scanTypeQualifiers tq
    pure $ Type (TyPointer ty') cvr

expandPtr :: AST.Pointer -> Declaration -> Translate Declaration
expandPtr ptr (VarDeclaration ty sc nm ml) = do
    ty' <- mkPtr ptr ty
    pure $ VarDeclaration ty' sc nm ml

expandPtr _ (FunDeclaration _ _ _ _) = undefined
expandPtr _ (TypedefDeclaration _ _) = undefined
expandPtr _ (TypeDeclaration _) = undefined

-- FIXME: check vararg
convertParams :: AST.ParameterTypeList -> Translate [ParamEntry]
convertParams (AST.ParameterTypeList pds vararg) = mapM expand pds
    where
        expand (AST.PDDeclarator ds d) = xVarDeclaration ds d >>= toEntry

        expand (AST.PDAbstract ds Nothing) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            pure $ ParamEntry (Type rt cvr) Nothing
            where
                (_, _, ts, tq, _, _) = dsSplit ds

        expand (AST.PDAbstract ds (Just adecl)) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            ty' <- applyAbstractDeclarator adecl (Type rt cvr)
            pure $ ParamEntry ty' Nothing
            where
                (_, _, ts, tq, _, _) = dsSplit ds

        -- FIXME: handle anonymous params
        toEntry :: Declaration -> Translate ParamEntry
        toEntry (VarDeclaration t _ nm _) = pure $ ParamEntry t (Just nm)
        toEntry (FunDeclaration _ _ _ _) = barf "Fun declaration can't be a param type"
        toEntry (TypedefDeclaration _ _) = undefined
        toEntry (TypeDeclaration _) = undefined

expandDD :: Type -> (Maybe StorageClass) -> AST.DirectDeclarator -> Translate Declaration
expandDD ty sc (AST.DDIdentifier nm) = do
    sym <- defVar nm ty
    pure $ VarDeclaration ty sc sym Nothing
expandDD ty sc (AST.DDNested d) = applyDeclarator ty sc d

-- FIXME: apply the qualifiers
expandDD ty sc (AST.DDArray dd' _ Nothing _ _) =
    expandDD (Type (TyArray ty Nothing) S.empty) sc dd'

-- FIXME: apply the qualifiers
expandDD ty sc (AST.DDArray dd' _ (Just e) _ _) = do
    e' <- xExpression e
    expandDD (Type (TyArray ty (Just e')) S.empty) sc dd'

expandDD ty sc (AST.DDFun dd' params) = do
    ps <- convertParams params
    ed <- applyDeclarator ty sc (AST.Declarator Nothing dd')
    case ed of
        (VarDeclaration ty' sc' nm _) -> pure $ VarDeclaration (Type (TyFunction (FunType ty' ps S.empty)) S.empty) sc' nm Nothing
        (FunDeclaration _ _ _ _)   -> barf "Unexpected function declaration"
        (TypedefDeclaration _ _) -> undefined
        (TypeDeclaration _) -> undefined
expandDD ty sc (AST.DDFunPtr dd' nms) = undefined

initToDecl :: AST.InitDeclarator -> AST.Declarator
initToDecl (AST.InitDeclarator d _) = d

xVarDeclaration :: [AST.DeclarationSpecifier] -> AST.Declarator -> Translate Declaration
xVarDeclaration ds declarator = do
    rt <- xTypeSpecifier ts
    sClass <- extractStorageClass sc
    cvr <- scanTypeQualifiers tq
    decl <- applyDeclarator (Type rt cvr) sClass declarator
    if isTypedef
        then case decl of
            (VarDeclaration ty Nothing (Symbol _ nm) Nothing) -> do
                rmVar nm
                sym <- defTypedef nm
                pure $ TypedefDeclaration ty sym
            _ -> barf "bad typedef"
        else pure decl
    where
        (isTypedef, sc, ts, tq, _, _) = dsSplit ds

xTypeDeclaration :: [AST.DeclarationSpecifier] -> Translate Declaration
xTypeDeclaration ds = do
    rt <- xTypeSpecifier ts
    sClass <- extractStorageClass sc
    cvr <- scanTypeQualifiers tq
    pure $ TypeDeclaration (Type rt cvr)
    where
        (_, sc, ts, tq, _, _) = dsSplit ds

xDeclaration :: AST.Declaration -> Translate [Declaration]
xDeclaration (AST.Declaration ds []) = do
    r <- xTypeDeclaration ds
    pure [r]
xDeclaration (AST.Declaration ds declarators) =
    mapM (xVarDeclaration ds) .
    map initToDecl $
    declarators
xDeclaration AST.StaticAssert = pure []

---------------------------------------------------------------------
-- Statement translation
xStatement :: AST.Statement -> Translate Statement
xStatement (AST.LabelStatement nm s) = do
    sym <- defLabel nm
    LabelStatement sym <$> xStatement s

xStatement (AST.CaseStatement e Nothing s) =
    CaseStatement <$> xExpression e <*> (pure Nothing) <*> xStatement s

xStatement (AST.CaseStatement e (Just e2) s) =
    CaseStatement <$> xExpression e <*> (Just <$> xExpression e2) <*> xStatement s

xStatement (AST.DefaultStatement s) =
    DefaultStatement <$> xStatement s

xStatement (AST.CompoundStatement nms items) = do
    labels <- mapM defLabel nms
    withScope ST.ScopeBlock $ CompoundStatement labels . concat <$> mapM xBlockItem items


xStatement (AST.ExpressionStatement e) =
    ExpressionStatement <$> xExpression e

xStatement (AST.IfStatement cond t Nothing) =
    IfStatement <$> xExpression cond <*> xStatement t <*> (pure Nothing)

xStatement (AST.IfStatement cond t (Just f)) =
    IfStatement <$> xExpression cond <*> xStatement t <*> (Just <$> xStatement f)

xStatement (AST.SwitchStatement e s) =
    SwitchStatement <$> xExpression e <*> xStatement s

xStatement (AST.WhileStatement e s) =
    WhileStatement <$> xExpression e <*> xStatement s

xStatement (AST.DoStatement s e) =
    DoStatement <$> xStatement s <*> xExpression e

xStatement (AST.ForStatement md me1 me2 me3 s) = do
    decls <- maybe (pure []) xDeclaration md
    me1' <- mapM xExpression me1
    me2' <- mapM xExpression me2
    me3' <- mapM xExpression me3
    s' <- xStatement s
    pure $ ForStatement decls me1' me2' me3' s'

xStatement (AST.GotoStatement nm) = do
    sym <- refLabel nm
    pure $ GotoStatement sym

xStatement (AST.ContinueStatement) =
    pure ContinueStatement

xStatement (AST.BreakStatement) =
    pure BreakStatement

xStatement (AST.ReturnStatement Nothing) =
    pure $ ReturnStatement Nothing

xStatement (AST.ReturnStatement (Just e)) =
    ReturnStatement <$> (Just <$> xExpression e)

xStatement (AST.EmptyStatement) =
    pure EmptyStatement

xStatement (AST.AsmStatement) =
    pure AsmStatement

xBlockItem :: AST.BlockItem -> Translate [BlockItem]
xBlockItem (AST.BIDeclaration decl) = map BIDeclaration <$> xDeclaration decl
xBlockItem (AST.BIStatement s) = singleton . BIStatement <$> xStatement s
    where
        singleton x = [x]

xTranslationUnit :: AST.TranslationUnit -> Translate TranslationUnit
xTranslationUnit (AST.TranslationUnit edecls) =
    TranslationUnit <$> (concat <$> mapM xExternalDeclaration edecls)

xExternalDeclaration :: AST.ExternalDeclaration -> Translate [ExternalDeclaration]
xExternalDeclaration (AST.ExternalDeclaration d) = do
    ds <- xDeclaration d
    pure $ map ExternalDeclaration ds

xExternalDeclaration (AST.FunDef specs d decls s) =
    withScope ST.ScopeParam $ do
        d' <- xVarDeclaration specs d
        case traceIt d' of
            (VarDeclaration (Type (TyFunction ft) _) _ nm  _) -> do
                --params' <- xParams decls
                s' <- withScope ST.ScopeFunction $ xStatement s
                pure [FunDef ft nm s'] -- FIXME: missing cvr
            x -> pure [ExternalDeclaration x]

xExternalDeclaration (AST.AsmDeclaration _) =
    pure [AsmDeclaration]

{-
xParams :: [AST.Declaration] -> Translate [ParamEntry]
xParams ds = mapM singleDecl ds
    where
        singleDecl :: AST.Declaration -> Translate ParamEntry
        singleDecl (AST.Declaration specs [AST.InitDeclarator d _]) = do
            d' <- xVarDeclaration specs d
            case d' of
                (Declaration t _ nm Nothing) -> pure $ ParamEntry t (Just nm)
                _ -> barf "bad parameter"
-}

toHir :: AST.TranslationUnit -> Either String TranslationUnit
toHir ast = runTranslate $ xTranslationUnit ast

traceIt :: (Show a) => a -> a
traceIt x = trace (show x) x
