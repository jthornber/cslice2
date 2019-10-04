{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module C.Translate (
            toHir
    ) where


import Debug.Trace

import qualified C.AST as AST
import C.ErrorType
import C.HIR
import C.Identifier
import C.Int
import C.LexerUtils
import C.SourcePos
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
import Data.Text (Text)
import qualified Data.Text as T

---------------------------------------------------------------------
-- Monad
data TranslateState = TranslateState {
    _tsSymbolTable :: SymbolTable,
    _tsUIDCount :: Int,
    _tsVarTypes :: Map Symbol Type,
    _tsStructDetails :: Map Symbol [StructEntry]
}

makeLenses ''TranslateState

type Translate = ExceptT SliceError (State TranslateState)

emptyTS :: TranslateState
emptyTS = TranslateState ST.empty 0 M.empty M.empty

runTranslate :: Translate a -> Either SliceError a
runTranslate x = evalState (runExceptT x) emptyTS

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

mkRef :: (Identifier -> SymbolTable -> Maybe Symbol) -> Text ->
         Identifier -> SourcePos -> Translate Symbol
mkRef fn namespace nm pos = do
    s <- view tsSymbolTable <$> get
    case fn nm s of
        Nothing -> barf pos ["Couldn't find reference to '",
                             showT nm,
                             "' in ",
                             namespace,
                             " scope"]
        (Just sym) -> pure sym

refFun :: Identifier -> SourcePos -> Translate Symbol
refFun = mkRef ST.refFun "function"

refStruct :: Identifier -> SourcePos -> Translate Symbol
refStruct = mkRef ST.refStruct "struct"

mrefStruct :: Identifier -> SourcePos ->Translate (Maybe Symbol)
mrefStruct nm _pos = ST.refStruct nm . view tsSymbolTable <$> get

refEnum :: Identifier -> SourcePos -> Translate Symbol
refEnum = mkRef ST.refEnum "enum"

refEnumElt :: Identifier -> SourcePos -> Translate Symbol
refEnumElt = mkRef ST.refVar "var (enum elt)"

refLabel :: Identifier -> SourcePos -> Translate Symbol
refLabel nm pos = do
    ts <- get
    case ST.refLabel nm (view tsSymbolTable ts) of
        Nothing -> barf pos ["couldn't find label: ",
                         showT nm]
        (Just v) -> pure v

refVar :: Identifier -> SourcePos -> Translate Symbol
refVar = mkRef ST.refVar "var"

refTypedef :: Identifier -> SourcePos -> Translate Symbol
refTypedef = mkRef ST.refTypedef "typedef"

structName' :: RawType -> Translate Symbol
structName' (TyStruct _ sym _) = pure sym
structName' (TyStructRef _ sym) = pure sym
structName' _ = internalError ["not a struct"]

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
        Nothing -> internalError ["struct isn't registered: ",
                                  showT structSym
                                 ]
        (Just fields) -> case structField' fields nm of
            Nothing -> internalError ["invalid struct field: ",
                                      showT structSym,
                                      ".",
                                      showT nm]
            (Just fieldSym) -> pure fieldSym

----------------------------------
-- Define entries in symbol table

mkDef :: (Symbol -> SymbolTable -> Maybe SymbolTable) ->
         Text -> Identifier -> SourcePos -> Translate Symbol
mkDef fn namespace nm pos = do
    sym <- mkSym nm
    ts <- get
    case fn sym (view tsSymbolTable ts) of
        Just st' -> do
            put $ over tsSymbolTable (const st') ts
            pure sym
        Nothing -> barf pos ["identifier already present in ",
                             namespace,
                             " scope: ",
                             showT nm
                             ]

defFun :: Identifier -> SourcePos -> Translate Symbol
defFun = mkDef ST.defFun "function"

defStruct' :: Identifier -> SourcePos -> Translate Symbol
defStruct' = mkDef ST.defStruct "struct"

defStruct :: Maybe [StructEntry] -> Identifier -> SourcePos -> Translate Symbol
defStruct (Just fields) nm pos = do
    msym <- mrefStruct nm pos
    case msym of
        Nothing -> do
            sym <- defStruct' nm pos
            modify $ over tsStructDetails (M.insert sym fields)
            pure sym
        Just sym -> do
            modify $ over tsStructDetails (M.insert sym fields)
            pure sym
defStruct Nothing nm pos = defStruct' nm pos

defEnum :: Identifier -> SourcePos -> Translate Symbol
defEnum = mkDef ST.defEnum "enum"

defLabel :: Identifier -> SourcePos -> Translate Symbol
defLabel nm pos = do
    sym <- mkSym nm
    ts <- get
    case ST.defLabel sym (view tsSymbolTable ts) of
        Just st' -> do
            put $ over tsSymbolTable (const st') ts
            pure sym
        Nothing -> barf pos ["multiply defined label"]

defVar :: Identifier -> Type -> SourcePos -> Translate Symbol
defVar nm ty pos = do
    sym <- mkDef ST.defVar "var" nm pos
    modify $ over tsVarTypes (M.insert sym ty)
    pure sym

rmVar :: Identifier -> SourcePos -> Translate ()
rmVar nm pos = do
    ts <- get
    case ST.rmVar nm (view tsSymbolTable ts) of
        (Just st') -> put $ over tsSymbolTable (const st') ts
        Nothing -> barf pos ["asked to remove symbol that is not present: ",
                             showT nm
                            ]

defTypedef :: Identifier -> SourcePos -> Translate Symbol
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
        Nothing -> internalError ["no scope to leave"]
        (Just st') -> put $ over tsSymbolTable (const st') ts

withScope :: Scope -> Translate a -> Translate a
withScope sc m = do
    pushScope sc
    r <- m
    popScope
    pure r

typeVar :: Symbol -> SourcePos -> Translate Type
typeVar sym pos = do
    ts <- get
    case M.lookup sym $ view tsVarTypes ts of
        Nothing -> barf pos ["unable to retrieve type for variable: ",
                             showT sym
                            ]
        (Just ty) -> pure ty

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn xs = do
    xs' <- mapM fn xs
    pure $ concat xs'

maybeT :: (a -> Translate b) -> Maybe a -> Translate (Maybe b)
maybeT _ Nothing = pure Nothing
maybeT fn (Just x) = Just <$> fn x

barf :: SourcePos -> [Text] -> Translate a
barf pos txt = throwError (SliceError (Just pos) $ T.concat txt)

internalError :: [Text] -> Translate a
internalError txt = throwError (SliceError Nothing (T.concat ("internal error: " : txt)))

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
xTypeName (AST.TypeName specs Nothing _) = do
    rty <- xTypeSpecifier ts
    cvr <- scanTypeQualifiers tq
    pure $ Type rty cvr
    where
        (_, _, ts, tq, _, _) = dsSplit $ map toDeclSpec specs

xTypeName (AST.TypeName specs (Just adecl) _) = do
    rty <- xTypeSpecifier ts
    cvr <- scanTypeQualifiers tq
    applyAbstractDeclarator adecl (Type rty cvr)
    where
        (_, _, ts, tq, _, _) = dsSplit $ map toDeclSpec specs

applyAbstractDeclarator :: AST.AbstractDeclarator -> Type -> Translate Type
applyAbstractDeclarator (AST.AbstractPointer ptr _) ty = mkPtr ptr ty
applyAbstractDeclarator (AST.AbstractDeclarator Nothing dad _) ty =
    applyDirectAbstractDeclarator dad ty
applyAbstractDeclarator (AST.AbstractDeclarator (Just ptr) dad _) ty = do
    ty' <- applyDirectAbstractDeclarator dad ty
    mkPtr ptr ty'

applyDirectAbstractDeclarator :: AST.DirectAbstractDeclarator -> Type -> Translate Type
applyDirectAbstractDeclarator (AST.DANested ad _) ty = applyAbstractDeclarator ad ty
applyDirectAbstractDeclarator (AST.DAArray mdad tq me isStatic _) ty = undefined
applyDirectAbstractDeclarator (AST.DAArrayStar _ _) ty = undefined

applyDirectAbstractDeclarator (AST.DAFun Nothing params _) ty = do
    params' <- convertParams params
    pure $ Type (TyFunction (FunType ty params' S.empty)) S.empty

-- FIXME: finish
applyDirectAbstractDeclarator (AST.DAFun (Just _) params _) ty = do
    params' <- convertParams params
    pure $ Type (TyFunction (FunType ty params' S.empty)) S.empty

xExpression :: AST.Exp -> Translate Exp
xExpression (AST.VarExp nm pos) = do
    sym <- refVar nm pos
    t <- typeVar sym pos
    pure . Exp t . VarExp $ sym

xExpression (AST.IntConstExp s ty n _) = do
    pure . Exp (Type (TyInt s ty) S.empty) . LiteralExp . IntValue $ n

xExpression (AST.StringConstExp str _) =
    pure . Exp constCharStar . LiteralExp . StringValue $ str
    where
        -- FIXME: write some combinators to make writing types out easier
        constCharStar = Type (TyPointer (Type (TyInt SIGNED CHAR) (S.fromList [CONST]))) S.empty

xExpression (AST.CharConstExp c _) =
    pure . Exp constChar . LiteralExp . CharValue  $ c
    where
        constChar = Type (TyInt SIGNED CHAR) (S.fromList [CONST])

xExpression (AST.CompoundLiteral tn inits _) = do
    ty <- xTypeName tn
    fields <- mapM xInitializerPair inits
    pure . Exp ty $ LiteralExp (CompoundValue ty fields)

xExpression (AST.SubscriptExp e1 e2 _) = do
    e1'@(Exp t _) <- xExpression e1
    e2' <- xExpression e2
    pure . Exp (arrayEltType t) $ SubscriptExp e1' e2'

xExpression (AST.FuncallExp e params _) = do
    fun@(Exp t _) <- xExpression e
    params' <- mapM xExpression params
    pure . Exp (returnType t) $ FuncallExp fun params'

xExpression (AST.StructElt e n _) = do
    e'@(Exp t _) <- xExpression e
    sym <- refStructElt t n
    pure . Exp (structEltType t sym) $ StructElt e' sym

xExpression (AST.StructDeref e n pos) = do
    e'@(Exp t _) <- xExpression (traceIt e)
    sym <- refVar n pos
    pure . Exp (structEltType (deref t) sym) $ StructDeref e' sym

xExpression (AST.CommaExp e1 e2 _) = do
    e1' <- xExpression e1
    e2'@(Exp t _) <- xExpression e2
    pure . Exp t $ CommaExp e1' e2'

xExpression (AST.AssignExp AST.ASSIGN e1 e2 _) = do
    e1'@(Exp t _) <- xExpression e1
    e2' <- xExpression e2
    pure . Exp t $ AssignExp e1' e2'

xExpression (AST.AssignExp AST.MULT_ASSIGN e1 e2 _) =
    binAssign MULT e1 e2

xExpression (AST.AssignExp AST.DIV_ASSIGN e1 e2 _) =
    binAssign DIV e1 e2

xExpression (AST.AssignExp AST.MOD_ASSIGN e1 e2 _) =
    binAssign MOD e1 e2

xExpression (AST.AssignExp AST.PLUS_ASSIGN e1 e2 _) =
    binAssign PLUS e1 e2

xExpression (AST.AssignExp AST.MINUS_ASSIGN e1 e2 _) =
    binAssign MINUS e1 e2

xExpression (AST.AssignExp AST.LSHIFT_ASSIGN e1 e2 _) =
    binAssign LSHIFT e1 e2

xExpression (AST.AssignExp AST.RSHIFT_ASSIGN e1 e2 _) =
    binAssign RSHIFT e1 e2

xExpression (AST.AssignExp AST.AND_ASSIGN e1 e2 _) =
    binAssign BIT_AND e1 e2

xExpression (AST.AssignExp AST.XOR_ASSIGN e1 e2 _) =
    binAssign XOR e1 e2

xExpression (AST.AssignExp AST.OR_ASSIGN e1 e2 _) =
    binAssign BIT_OR e1 e2

xExpression (AST.ConditionalExp e1 e2 e3 _) = do
    e1' <- xExpression e1
    e2'@(Exp t _) <- xExpression e2
    e3' <- xExpression e3
    pure . Exp t $ ConditionalExp e1' e2' e3'

xExpression (AST.BinaryExp op e1 e2 _) = do
    e1'@(Exp t1 _) <- xExpression e1
    e2'@(Exp t2 _) <- xExpression e2
    op' <- xBinOp op
    pure . Exp (usualArithmeticConversion t1 t2) $ BinaryExp op' e1' e2'

xExpression (AST.UnaryExp op e1 _) = do
    e1'@(Exp t _) <- xExpression e1
    op' <- xUnOp op
    pure . Exp t $ UnaryExp op' e1'

xExpression (AST.CastExp tn e _) = do
    t <- xTypeName tn
    e' <- xExpression e
    pure . Exp t $ CastExp t e'

xExpression (AST.SizeofValueExp e _) =
    Exp unsigned_long . SizeofValueExp <$> xExpression e
    where
        unsigned_long = Type (TyInt UNSIGNED LONG) S.empty

xExpression (AST.SizeofTypeExp tn _) =
    Exp unsigned_long . SizeofTypeExp <$> xTypeName tn
    where
        unsigned_long = Type (TyInt UNSIGNED LONG) S.empty

xExpression (AST.AlignofExp tn _) =
    Exp unsigned . AlignofExp <$> xTypeName tn
    where
        unsigned = Type (TyInt UNSIGNED INT) S.empty

xExpression (AST.BlockExp nms items pos) = do
    nms' <- forM nms $ \nm -> refVar nm pos
    items' <- withScope ST.ScopeBlock $ concatMapM xBlockItem items
    -- FIXME: how do we ensure the last block item is an expression with a type
    pure . Exp void' $ BlockExp nms' items'
    where
        void' = Type TyVoid S.empty

xExpression (AST.BuiltinVaArg _) =
    pure $ Exp (Type TyVoid S.empty) BuiltinVaArg

xExpression (AST.BuiltinOffsetOf _) =
    pure $ Exp (Type TyVoid S.empty) BuiltinOffsetOf

xExpression (AST.BuiltinTypesCompatible _) =
    pure $ Exp (Type TyVoid S.empty) BuiltinTypesCompatible

xExpression (AST.BuiltinConvertVector _) =
    pure $ Exp (Type TyVoid S.empty) BuiltinConvertVector

xInitializerPair :: AST.InitializerPair -> Translate InitializerPair
xInitializerPair (AST.InitializerPair Nothing i _) = InitializerPair Nothing <$> xInitializer i
xInitializerPair (AST.InitializerPair (Just ds) i _) = InitializerPair <$> (Just <$> mapM xDesignator ds) <*> xInitializer i

xDesignator :: AST.Designator -> Translate Designator
xDesignator (AST.SubscriptDesignator e _) = SubscriptDesignator <$> xExpression e
xDesignator (AST.StructDesignator nm pos) = StructDesignator <$> refStruct nm pos

xInitializer :: AST.Initializer -> Translate Initializer
xInitializer (AST.InitAssign e _) = InitAssign <$> xExpression e
xInitializer (AST.InitList pairs _) = InitList <$> mapM xInitializerPair pairs

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
    _   -> internalError ["Too many storage classes: ",
                          showT specs
                         ]
    where
        -- Typedef is a storage clas in the AST, but a separate
        -- declaration type in HIR.
        getSC AST.Typedef       = bad
        getSC AST.Extern        = pure $ Just Extern
        getSC AST.Static        = pure $ Just Static
        getSC AST.ThreadLocal   = bad
        getSC AST.Auto          = pure $ Just Auto
        getSC AST.Register      = pure $ Just Register

        bad = internalError ["unsupported storage class"]

dsSplit :: [AST.DeclarationSpecifier] -> (Bool,
                                          [AST.StorageClass], [AST.TypeSpecifier],
                                          [AST.TypeQualifier], [AST.FunctionSpecifier],
                                          [AST.AlignmentSpecifier])
dsSplit = foldr get' (False, [], [], [], [], [])
    where
        get' (AST.DSStorageClass AST.Typedef _) (_, sc, ts, tq, fs, align) = (True, sc, ts, tq, fs, align)
        get' (AST.DSStorageClass x _) (td, sc, ts, tq, fs, align) = (td, x:sc, ts, tq, fs, align)
        get' (AST.DSTypeSpecifier x _) (td, sc, ts, tq, fs, align) = (td, sc, x:ts, tq, fs, align)
        get' (AST.DSTypeQualifier x _) (td, sc, ts, tq, fs, align) = (td, sc, ts, x:tq, fs, align)
        get' (AST.DSFunctionSpecifier x _) (td, sc, ts, tq, fs, align) = (td, sc, ts, tq, x:fs, align)
        get' (AST.DSAlignmentSpecifier x _) (td, sc, ts, tq, fs, align) = (td, sc, ts, tq, fs, x:align)
        get' (AST.DSAttr _ _) r = r

scanTypeQualifiers :: [AST.TypeQualifier] -> Translate (Set CVR)
scanTypeQualifiers = foldM insert S.empty
    where
        insert s AST.Const    = pure $ S.insert CONST s
        insert s AST.Volatile = pure $ S.insert VOLATILE s
        insert s AST.Restrict = pure $ S.insert RESTRICT s
        insert _ AST.Atomic   = internalError ["Atomic not supported"]

xTypeSpecifier :: [AST.TypeSpecifier] -> Translate RawType
xTypeSpecifier specs = case specs of
    [] -> internalError ["No type specifier given"]
    [x] -> getTS x
    _ -> internalError ["Too many type specifiers: ", showT specs]

    where
        getTS :: AST.TypeSpecifier -> Translate RawType
        getTS (AST.Void _)                 = pure TyVoid
        getTS (AST.Char _)                 = pure $ signed CHAR
        getTS (AST.UnsignedChar _)         = pure $ unsigned CHAR
        getTS (AST.Short _)                = pure $ signed SHORT
        getTS (AST.UnsignedShort _)        = pure $ unsigned SHORT
        getTS (AST.Int _)                  = pure $ signed INT
        getTS (AST.UnsignedInt _)          = pure $ unsigned INT
        getTS (AST.Int128 _)               = pure $ signed INT128
        getTS (AST.UnsignedInt128 _)       = pure $ unsigned INT128
        getTS (AST.Long _)                 = pure $ signed LONG
        getTS (AST.UnsignedLong _)         = pure $ unsigned LONG
        getTS (AST.LongLong _)             = pure $ signed LONG_LONG
        getTS (AST.UnsignedLongLong _)     = pure $ unsigned LONG_LONG
        getTS (AST.Float _)                = pure TyFloat
        getTS (AST.Double _)               = pure TyDouble
        getTS (AST.Bool _)                 = pure TyBool
        getTS (AST.Complex pos)            = barf pos ["complex nrs not implemented"]
        getTS (AST.AtomicSpecifier pos)    = barf pos ["atomics not implemented"]

        getTS (AST.StructOrUnionSpecifier st mnm Nothing pos) = do
            case mnm of
                Nothing -> do
                    sym <- anonSym
                    pure $ TyStructRef (xStructType st) sym
                (Just nm) -> do
                    sym <- refStruct nm pos
                    pure $ TyStructRef (xStructType st) sym

        getTS (AST.StructOrUnionSpecifier st mnm (Just fields) pos) = do
            entries <- mapM xStructEntry fields
            sym <- maybe anonSym (\nm -> defStruct (Just $ concat entries) nm pos) mnm
            pure $ TyStruct (xStructType st) sym (concat entries)

        getTS (AST.EnumDefSpecifier mnm entries pos) = do
            entries' <- mapM xEnumEntry entries
            msym <- maybeT (\nm -> defEnum nm pos) mnm
            pure $ TyEnum msym (Just entries')

        getTS (AST.EnumRefSpecifier nm pos) = do
            sym <- refEnum nm pos
            pure $ TyEnum (Just sym) Nothing

        getTS (AST.TSTypedefName nm pos) = do
            sym <- refTypedef nm pos
            pure $ TyAlias sym

        getTS (AST.TSTypeofExp e _) =
            TyTypeofExp <$> xExpression e

        getTS (AST.TSTypeofDecl ds _) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            pure (TyTypeofType $ Type rt cvr)
            where
                (_, _, ts, tq, _, _) = dsSplit ds

        signed x = TyInt SIGNED x
        unsigned x = TyInt UNSIGNED x

xEnumEntry :: AST.Enumerator -> Translate EnumEntry
xEnumEntry (AST.Enumerator nm me pos) = do
    sym <- defVar nm (Type (TyInt UNSIGNED INT) S.empty) pos
    me' <- mapM xExpression me
    pure $ EnumEntry sym me'

xStructType :: AST.StructType -> StructType
xStructType AST.Struct = Struct
xStructType AST.Union = Union

xStructDeclarator :: [AST.SpecifierQualifier] -> AST.StructDeclarator -> Translate [StructEntry]
xStructDeclarator specs (AST.StructDeclarator decl me pos) = do
    decl' <- xVarDeclaration (map toDeclSpec specs) decl
    case decl' of
        (VarDeclaration ty _ sym _) -> do
            width <- mapM xExpression me
            pure $ [StructEntry ty (Just sym) width]
        _ -> barf pos ["bad struct field"]
xStructDeclarator _ (AST.StructDeclaratorNoDecl e _) = do
    e' <- xExpression e
    pure [StructEntryWidthOnly e']

toDeclSpec :: AST.SpecifierQualifier -> AST.DeclarationSpecifier
toDeclSpec (AST.SQTypeSpecifier ts pos) = AST.DSTypeSpecifier ts pos
toDeclSpec (AST.SQTypeQualifier tq pos) = AST.DSTypeQualifier tq pos

xStructEntry :: AST.StructDeclaration -> Translate [StructEntry]
xStructEntry (AST.StructDeclaration specs declarators _) =
    concat <$> mapM (xStructDeclarator specs) declarators

xStructEntry (AST.StructStaticAssert pos) = barf pos ["Struct static assert not supported"]

applyDeclarator :: Type -> (Maybe StorageClass) -> AST.Declarator -> Translate Declaration
applyDeclarator ty sc (AST.Declarator Nothing dd _) = expandDD ty sc dd
applyDeclarator ty sc (AST.Declarator (Just ptr) dd _) = expandDD ty sc dd >>= expandPtr ptr

mkPtr :: AST.Pointer -> Type -> Translate Type
mkPtr (AST.Pointer tq Nothing _) ty = do
    cvr <- scanTypeQualifiers tq
    pure $ Type (TyPointer ty) cvr

mkPtr (AST.Pointer tq (Just subptr) _) ty = do
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
convertParams (AST.ParameterTypeList pds vararg _) = mapM expand pds
    where
        expand (AST.PDDeclarator ds d _) = xVarDeclaration ds d >>= toEntry

        expand (AST.PDAbstract ds Nothing _) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            pure $ ParamEntry (Type rt cvr) Nothing
            where
                (_, _, ts, tq, _, _) = dsSplit ds

        expand (AST.PDAbstract ds (Just adecl) _) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            ty' <- applyAbstractDeclarator adecl (Type rt cvr)
            pure $ ParamEntry ty' Nothing
            where
                (_, _, ts, tq, _, _) = dsSplit ds

        -- FIXME: handle anonymous params
        toEntry :: Declaration -> Translate ParamEntry
        toEntry (VarDeclaration t _ nm _) = pure $ ParamEntry t (Just nm)
        toEntry (FunDeclaration _ _ _ _) = internalError ["Fun declaration can't be a param type"]
        toEntry (TypedefDeclaration _ _) = undefined
        toEntry (TypeDeclaration _) = undefined

expandDD :: Type -> (Maybe StorageClass) -> AST.DirectDeclarator -> Translate Declaration
expandDD ty sc (AST.DDIdentifier nm pos) = do
    sym <- defVar nm ty pos
    pure $ VarDeclaration ty sc sym Nothing
expandDD ty sc (AST.DDNested d _) = applyDeclarator ty sc d

-- FIXME: apply the qualifiers
expandDD ty sc (AST.DDArray dd' _ Nothing _ _ _) =
    expandDD (Type (TyArray ty Nothing) S.empty) sc dd'

-- FIXME: apply the qualifiers
expandDD ty sc (AST.DDArray dd' _ (Just e) _ _ _) = do
    e' <- xExpression e
    expandDD (Type (TyArray ty (Just e')) S.empty) sc dd'

expandDD ty sc (AST.DDFun dd' params pos) = do
    ps <- convertParams params
    ed <- applyDeclarator ty sc (AST.Declarator Nothing dd' pos)
    case ed of
        (VarDeclaration ty' sc' nm _) -> pure $ VarDeclaration (Type (TyFunction (FunType ty' ps S.empty)) S.empty) sc' nm Nothing
        (FunDeclaration _ _ _ _)   -> internalError ["Unexpected function declaration"]
        (TypedefDeclaration _ _) -> undefined
        (TypeDeclaration _) -> undefined
expandDD ty sc (AST.DDFunPtr dd' nms _) = undefined

initToDecl :: AST.InitDeclarator -> AST.Declarator
initToDecl (AST.InitDeclarator d _ _) = d

xVarDeclaration :: [AST.DeclarationSpecifier] -> AST.Declarator -> Translate Declaration
xVarDeclaration ds declarator = do
    let pos = getPos declarator
    rt <- xTypeSpecifier ts
    sClass <- extractStorageClass sc
    cvr <- scanTypeQualifiers tq
    decl <- applyDeclarator (Type rt cvr) sClass declarator
    if isTypedef
        then case decl of
            (VarDeclaration ty Nothing (Symbol _ nm) Nothing) -> do
                rmVar nm pos
                sym <- defTypedef nm pos
                pure $ TypedefDeclaration ty sym
            _ -> barf pos ["bad typedef"]
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
xDeclaration (AST.Declaration ds [] _) = do
    r <- xTypeDeclaration ds
    pure [r]
xDeclaration (AST.Declaration ds declarators _) =
    mapM (xVarDeclaration ds) .
    map initToDecl $
    declarators
xDeclaration (AST.StaticAssert _) = pure []

---------------------------------------------------------------------
-- Statement translation
xStatement :: AST.Statement -> Translate Statement
xStatement (AST.LabelStatement nm s pos) = do
    sym <- defLabel nm pos
    LabelStatement sym <$> xStatement s

xStatement (AST.CaseStatement e Nothing s _) =
    CaseStatement <$> xExpression e <*> (pure Nothing) <*> xStatement s

xStatement (AST.CaseStatement e (Just e2) s _) =
    CaseStatement <$> xExpression e <*> (Just <$> xExpression e2) <*> xStatement s

xStatement (AST.DefaultStatement s _) =
    DefaultStatement <$> xStatement s

xStatement (AST.CompoundStatement nms items pos) = do
    labels <- mapM (\nm -> defLabel nm pos) nms
    withScope ST.ScopeBlock $ CompoundStatement labels . concat <$> mapM xBlockItem items

xStatement (AST.ExpressionStatement e _) =
    ExpressionStatement <$> xExpression e

xStatement (AST.IfStatement cond t Nothing _) =
    IfStatement <$> xExpression cond <*> xStatement t <*> (pure Nothing)

xStatement (AST.IfStatement cond t (Just f) _) =
    IfStatement <$> xExpression cond <*> xStatement t <*> (Just <$> xStatement f)

xStatement (AST.SwitchStatement e s _) =
    SwitchStatement <$> xExpression e <*> xStatement s

xStatement (AST.WhileStatement e s _) =
    WhileStatement <$> xExpression e <*> xStatement s

xStatement (AST.DoStatement s e _) =
    DoStatement <$> xStatement s <*> xExpression e

xStatement (AST.ForStatement md me1 me2 me3 s _) = do
    decls <- maybe (pure []) xDeclaration md
    me1' <- mapM xExpression me1
    me2' <- mapM xExpression me2
    me3' <- mapM xExpression me3
    s' <- xStatement s
    pure $ ForStatement decls me1' me2' me3' s'

xStatement (AST.GotoStatement nm pos) = do
    sym <- refLabel nm pos
    pure $ GotoStatement sym

xStatement (AST.ContinueStatement _) =
    pure ContinueStatement

xStatement (AST.BreakStatement _) =
    pure BreakStatement

xStatement (AST.ReturnStatement Nothing _) =
    pure $ ReturnStatement Nothing

xStatement (AST.ReturnStatement (Just e) _) =
    ReturnStatement <$> (Just <$> xExpression e)

xStatement (AST.EmptyStatement _) =
    pure EmptyStatement

xStatement (AST.AsmStatement _) =
    pure AsmStatement

xBlockItem :: AST.BlockItem -> Translate [BlockItem]
xBlockItem (AST.BIDeclaration decl _) = map BIDeclaration <$> xDeclaration decl
xBlockItem (AST.BIStatement s _) = singleton . BIStatement <$> xStatement s
    where
        singleton x = [x]

xTranslationUnit :: AST.TranslationUnit -> Translate TranslationUnit
xTranslationUnit (AST.TranslationUnit edecls _) =
    TranslationUnit <$> (concat <$> mapM xExternalDeclaration edecls)

xExternalDeclaration :: AST.ExternalDeclaration -> Translate [ExternalDeclaration]
xExternalDeclaration (AST.ExternalDeclaration d _) = do
    ds <- xDeclaration d
    pure $ map ExternalDeclaration ds

xExternalDeclaration (AST.FunDef specs d decls s _) =
    withScope ST.ScopeParam $ do
        d' <- xVarDeclaration specs d
        case traceIt d' of
            (VarDeclaration (Type (TyFunction ft) _) _ nm  _) -> do
                --params' <- xParams decls
                s' <- withScope ST.ScopeFunction $ xStatement s
                pure [FunDef ft nm s'] -- FIXME: missing cvr
            x -> pure [ExternalDeclaration x]

xExternalDeclaration (AST.AsmDeclaration _ _) =
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

toHir :: AST.TranslationUnit -> Either SliceError TranslationUnit
toHir ast = runTranslate $ xTranslationUnit ast

showT :: (Show a) => a -> Text
showT = T.pack . show

traceIt :: (Show a) => a -> a
traceIt x = trace (show x) x
