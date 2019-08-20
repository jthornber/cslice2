module C.Translate (
    toHir
    ) where

import qualified C.AST as AST
import C.HIR
import C.Identifier
import C.Lexer
import C.Parser
import C.SymbolTable (SymbolTable, Symbol)
import qualified C.SymbolTable as ST

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.Set (Set)
import qualified Data.Set as S

---------------------------------------------------------------------
-- Monad
data TranslateState = TranslateState {
    tsSymbolTable :: SymbolTable
}

data TranslateError = TranslateError String
    deriving (Show)

type Translate = ExceptT TranslateError (State TranslateState)

runTranslate :: Translate a -> Either String a
runTranslate x = case evalState (runExceptT x) (TranslateState ST.empty) of
    (Left (TranslateError msg)) -> Left msg
    (Right x') -> Right x'

mkDef :: (Identifier -> SymbolTable -> (SymbolTable, Symbol)) -> Identifier -> Translate Symbol
mkDef fn nm = do
    (TranslateState st) <- get
    let (st', sym) = fn nm st
    put $ TranslateState st'
    pure sym

mkRef :: (Identifier -> SymbolTable -> Symbol) -> Identifier -> Translate Symbol
mkRef fn nm = gets $ fn nm . tsSymbolTable

defFun, defStruct, defStructElt, defEnum, defEnumElt, defLabel, defVar, defTypedef
    :: Identifier -> Translate Symbol
defFun = mkDef ST.defFun
defStruct = mkDef ST.defStruct
defStructElt = mkDef ST.defStructElt
defEnum = mkDef ST.defEnum
defEnumElt = mkDef ST.defEnumElt
defLabel = mkDef ST.defLabel
defVar = mkDef ST.defVar
defTypedef = mkDef ST.defTypedef

refFun, refStruct, refStructElt, refEnum, refEnumElt, refLabel, refVar, refTypedef
    :: Identifier -> Translate Symbol
refFun = mkRef ST.refFun
refStruct = mkRef ST.refStruct
refStructElt = mkRef ST.refStructElt
refEnum = mkRef ST.refEnum
refEnumElt = mkRef ST.refEnumElt
refLabel = mkRef ST.refLabel
refVar = mkRef ST.refVar
refTypedef = mkRef ST.refTypedef

concatMapM fn xs = do
    xs' <- mapM fn xs
    pure $ concat xs'

maybeT :: (a -> Translate b) -> Maybe a -> Translate (Maybe b)
maybeT fn Nothing = pure Nothing
maybeT fn (Just x) = Just <$> fn x

barf :: String -> Translate a
barf txt = throwError (TranslateError txt)

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
    e1' <- xExpression e1
    e2' <- xExpression e2
    pure $ AssignExp e1' (BinaryExp op e1' e2')

xTypeName :: AST.TypeName -> Translate Type
xTypeName (AST.TypeName specs Nothing) = do
    rty <- xTypeSpecifier ts
    cvr <- scanTypeQualifiers tq
    pure $ Type rty cvr
    where
        (isTypedef, sc, ts, tq,fs, align) = dsSplit $ map toDeclSpec specs

xTypeName (AST.TypeName specs (Just adecl)) = do
    rty <- xTypeSpecifier ts
    cvr <- scanTypeQualifiers tq
    applyAbstractDeclarator adecl (Type rty cvr)
    where
        (isTypedef, sc, ts, tq,fs, align) = dsSplit $ map toDeclSpec specs

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
xExpression (AST.VarExp i) =
    VarExp <$> refVar i

xExpression (AST.IntConstExp n) =
    pure $ LiteralExp $ IntValue n

xExpression (AST.StringConstExp str) =
    pure $ LiteralExp $ StringValue str

xExpression (AST.CharConstExp c) =
    pure $ LiteralExp $ CharValue (head c)

xExpression (AST.CompoundLiteral tn inits) = do
    ty <- xTypeName tn
    fields <- mapM xInitializerPair inits
    pure $ LiteralExp (CompoundValue ty fields)

xExpression (AST.SubscriptExp e1 e2) =
    SubscriptExp <$> xExpression e1 <*> xExpression e2

xExpression (AST.FuncallExp e params) =
    FuncallExp <$> xExpression e <*> mapM xExpression params

xExpression (AST.StructElt e n) =
    StructElt <$> xExpression e <*> refVar n

xExpression (AST.StructDeref e n) =
    StructDeref <$> xExpression e <*> refVar n

xExpression (AST.CommaExp e1 e2) =
    CommaExp <$> xExpression e1 <*> xExpression e2

xExpression (AST.AssignExp AST.ASSIGN e1 e2) =
    AssignExp <$> xExpression e1 <*> xExpression e2

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

xExpression (AST.ConditionalExp e1 e2 e3) =
    ConditionalExp <$> xExpression e1 <*> xExpression e2 <*> xExpression e3

xExpression (AST.BinaryExp op e1 e2) =
    BinaryExp <$> xBinOp op <*> xExpression e1 <*> xExpression e2

xExpression (AST.UnaryExp op e1) =
    UnaryExp <$> xUnOp op <*> xExpression e1

xExpression (AST.CastExp tn e) =
    CastExp <$> xTypeName tn <*> xExpression e

xExpression (AST.SizeofValueExp e) =
    SizeofValueExp <$> xExpression e

xExpression (AST.SizeofTypeExp tn) =
    SizeofTypeExp <$> xTypeName tn

xExpression (AST.AlignofExp tn) =
    AlignofExp <$> xTypeName tn

xExpression (AST.BlockExp nms items) =
    BlockExp <$> mapM refVar nms <*> concatMapM xBlockItem items

xExpression (AST.BuiltinVaArg) =
    pure BuiltinVaArg

xExpression (AST.BuiltinOffsetOf) =
    pure BuiltinOffsetOf

xExpression (AST.BuiltinTypesCompatible) =
    pure BuiltinTypesCompatible

xExpression (AST.BuiltinConvertVector) =
    pure BuiltinConvertVector

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
dsSplit = foldr get (False, [], [], [], [], [])
    where
        get (AST.DSStorageClass AST.Typedef) (_, sc, ts, tq, fs, align) = (True, sc, ts, tq, fs, align)
        get (AST.DSStorageClass x) (td, sc, ts, tq, fs, align) = (td, x:sc, ts, tq, fs, align)
        get (AST.DSTypeSpecifier x) (td, sc, ts, tq, fs, align) = (td, sc, x:ts, tq, fs, align)
        get (AST.DSTypeQualifier x) (td, sc, ts, tq, fs, align) = (td, sc, ts, x:tq, fs, align)
        get (AST.DSFunctionSpecifier x) (td, sc, ts, tq, fs, align) = (td, sc, ts, tq, x:fs, align)
        get (AST.DSAlignmentSpecifier x) (td, sc, ts, tq, fs, align) = (td, sc, ts, tq, fs, x:align)
        get (AST.DSAttr _) r = r

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
                Nothing -> pure $ TyStruct (xStructType st) Nothing Nothing
                (Just nm) -> do
                    sym <- defStruct nm
                    pure $ TyStruct (xStructType st) (Just sym) Nothing

        getTS (AST.StructOrUnionSpecifier st mnm (Just fields)) = do
            entries <- mapM xStructEntry fields
            msym <- maybeT defStruct mnm
            pure $ TyStruct (xStructType st) msym (Just . concat $ entries)

        getTS (AST.EnumDefSpecifier mnm entries) = do
            entries' <- mapM xEnumEntry entries
            msym <- maybeT defEnum mnm
            pure $ TyEnum msym (Just entries')

        getTS (AST.EnumRefSpecifier nm)  = do
            sym <- refEnum nm
            pure $ TyEnum (Just sym) Nothing

        getTS (AST.TSTypedefName nm)     = do
            sym <- refTypedef nm
            pure $ TyAlias sym

        getTS (AST.TSTypeofExp e)        = TyTypeofExp <$> xExpression e
        getTS (AST.TSTypeofDecl ds)   = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            pure (TyTypeofType $ Type rt cvr)
            where
                (isTypedef, sc, ts, tq, fs, align) = dsSplit ds

        signed x = TyInt SIGNED x
        unsigned x = TyInt UNSIGNED x

xEnumEntry :: AST.Enumerator -> Translate EnumEntry
xEnumEntry (AST.Enumerator nm me) = do
    sym <- defEnumElt nm
    me' <- mapM xExpression me
    pure $ EnumEntry sym me'

xStructType AST.Struct = Struct
xStructType AST.Union = Union

xStructDeclarator :: [AST.SpecifierQualifier] -> AST.StructDeclarator -> Translate [StructEntry]
xStructDeclarator specs (AST.StructDeclarator decl me) = do
    decl <- xDeclaration' (map toDeclSpec specs) decl
    case decl of
        (Declaration ty _ sym _) -> do
            width <- mapM xExpression me
            pure $ [StructEntry ty (Just sym) width]
        _ -> barf "bad struct field"
xStructDeclarator specs (AST.StructDeclaratorNoDecl e) = do
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
expandPtr ptr (Declaration ty sc nm ml) = do
    ty' <- mkPtr ptr ty
    pure $ Declaration ty' sc nm ml

expandPtr _ (FunDeclaration _ _ _ _) = undefined
expandPtr _ (TypedefDeclaration _ _) = undefined

-- FIXME: check vararg
convertParams :: AST.ParameterTypeList -> Translate [ParamEntry]
convertParams (AST.ParameterTypeList pds vararg) = mapM expand pds
    where
        expand (AST.PDDeclarator ds d) = xDeclaration' ds d >>= toEntry

        expand (AST.PDAbstract ds Nothing) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            pure $ ParamEntry (Type rt cvr) Nothing
            where
                (isTypedef, sc, ts, tq, fs, align) = dsSplit ds

        expand (AST.PDAbstract ds (Just adecl)) = do
            rt <- xTypeSpecifier ts
            cvr <- scanTypeQualifiers tq
            ty' <- applyAbstractDeclarator adecl (Type rt cvr)
            pure $ ParamEntry (Type rt cvr) Nothing
            where
                (isTypedef, sc, ts, tq, fs, align) = dsSplit ds

        -- FIXME: handle anonymous params
        toEntry :: Declaration -> Translate ParamEntry
        toEntry (Declaration t msc nm ml) = pure $ ParamEntry t (Just nm)
        toEntry (FunDeclaration t sc nm fs) = barf "Fun declaration can't be a param type"
        toEntry (TypedefDeclaration _ _) = undefined

expandDD :: Type -> (Maybe StorageClass) -> AST.DirectDeclarator -> Translate Declaration
expandDD ty sc (AST.DDIdentifier nm)          = do
    sym <- defVar nm
    pure $ Declaration ty sc sym Nothing
expandDD ty sc (AST.DDNested d)               = applyDeclarator ty sc d

-- FIXME: apply the qualifiers
expandDD ty sc (AST.DDArray dd' tq Nothing _ _) =
    expandDD (Type (TyArray ty Nothing) S.empty) sc dd'

-- FIXME: apply the qualifiers
expandDD ty sc (AST.DDArray dd' tq (Just e) _ _) = do
    e' <- xExpression e
    expandDD (Type (TyArray ty (Just e')) S.empty) sc dd'

expandDD ty sc (AST.DDFun dd' params) = do
    ps <- convertParams params
    ed <- applyDeclarator ty sc (AST.Declarator Nothing dd')
    case ed of
        (Declaration ty' sc' nm _) -> pure $ Declaration (Type (TyFunction (FunType ty' ps S.empty)) S.empty) sc' nm Nothing
        (FunDeclaration _ _ _ _)   -> barf "Unexpected function declaration"
        (TypedefDeclaration _ _) -> undefined
expandDD ty sc (AST.DDFunPtr dd' nms)         = undefined

initToDecl :: AST.InitDeclarator -> AST.Declarator
initToDecl (AST.InitDeclarator d _) = d

xDeclaration' :: [AST.DeclarationSpecifier] -> AST.Declarator -> Translate Declaration
xDeclaration' ds declarator = do
    rt <- xTypeSpecifier ts
    sClass <- extractStorageClass sc
    cvr <- scanTypeQualifiers tq
    decl <- applyDeclarator (Type rt cvr) sClass declarator
    if isTypedef
        then case decl of
            (Declaration ty Nothing nm Nothing) -> pure $ TypedefDeclaration ty nm
            _ -> barf "bad typedef"
        else pure decl
    where
        (isTypedef, sc, ts, tq, fs, align) = dsSplit ds

-- Declaration Type StorageClass Identifier (Maybe Literal)
xDeclaration :: AST.Declaration -> Translate [Declaration]
xDeclaration (AST.Declaration ds declarators) =
    mapM (xDeclaration' ds) .
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
    syms <- mapM defVar nms
    CompoundStatement syms . concat <$> mapM xBlockItem items

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

xExternalDeclaration (AST.FunDef specs d decls s) = do
    d' <- xDeclaration' specs d
    case d' of
        (Declaration t@(Type (TyFunction ft) _) _ nm  _) -> do
            params' <- xParams decls
            s' <- xStatement s
            pure [FunDef ft nm s'] -- FIXME: missing cvr
        x -> pure [ExternalDeclaration x]
    where
        singleton = (: [])

xExternalDeclaration (AST.AsmDeclaration _) =
    pure [AsmDeclaration]

xParams :: [AST.Declaration] -> Translate [ParamEntry]
xParams ds = mapM singleDecl ds
    where
        singleDecl :: AST.Declaration -> Translate ParamEntry
        singleDecl (AST.Declaration specs [AST.InitDeclarator d _]) = do
            d' <- xDeclaration' specs d
            case d' of
                (Declaration t _ nm Nothing) -> pure $ ParamEntry t (Just nm)
                _ -> barf "bad parameter"

toHir :: AST.TranslationUnit -> Either String TranslationUnit
toHir ast = runTranslate $ xTranslationUnit ast
