module Main where

import qualified AST as AST
import HIR
import Lexer
import Parser

import Data.Set (Set)
import qualified Data.Set as S

---------------------------------------------------------------------
-- Expression translation

xBinOp :: AST.BinOp -> Translate BinOp
xBinOp AST.LOGICAL_OR = pure LOGICAL_OR
xBinOp AST.LOGICAL_AND = pure LOGICAL_AND
xBinOp AST.BIT_OR = pure BIT_OR
xBinOp AST.BIT_AND = pure BIT_AND
xBinOp AST.XOR = pure XOR
xBinOp AST.EQ = pure HIR.EQ
xBinOp AST.NEQ = pure NEQ
xBinOp AST.LT = pure HIR.LT
xBinOp AST.LTE = pure LTE
xBinOp AST.GT = pure HIR.GT
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
xTypeName = undefined

xExpression :: AST.Exp -> Translate Exp
xExpression (AST.VarExp i) =
    pure $ VarExp i

xExpression (AST.IntConstExp n) =
    pure $ LiteralExp $ IntValue n

xExpression (AST.StringConstExp str) =
    pure $ LiteralExp $ StringValue str

xExpression (AST.CharConstExp c) =
    pure $ LiteralExp $ CharValue (head c)

xExpression (AST.CompoundLiteral tn inits) =
    undefined

xExpression (AST.SubscriptExp e1 e2) =
    SubscriptExp <$> xExpression e1 <*> xExpression e2

xExpression (AST.FuncallExp e params) =
    FuncallExp <$> xExpression e <*> mapM xExpression params

xExpression (AST.StructElt e n) =
    StructElt <$> xExpression e <*> pure n

xExpression (AST.StructDeref e n) =
    StructDeref <$> xExpression e <*> pure n

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
    BlockExp nms <$> mapM xBlockItem items

xExpression (AST.BuiltinVaArg) =
    pure BuiltinVaArg

xExpression (AST.BuiltinOffsetOf) =
    pure BuiltinOffsetOf

xExpression (AST.BuiltinTypesCompatible) =
    pure BuiltinTypesCompatible

xExpression (AST.BuiltinConvertVector) =
    pure BuiltinConvertVector

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

type Translate a = Either String a

extractStorageClass :: [AST.StorageClass] -> Translate (Maybe StorageClass)
extractStorageClass specs = case specs of
    []  -> Right Nothing
    [x] -> getSC x
    _   -> Left $ "Too many storage classes: " ++ (show specs)
    where
        -- Typedef is a storage clas in the AST, but a separate
        -- declaration type in HIR.
        getSC AST.Typedef       = bad
        getSC AST.Extern        = Right $ Just Extern
        getSC AST.Static        = Right $ Just Static
        getSC AST.ThreadLocal   = bad
        getSC AST.Auto          = Right $ Just Auto
        getSC AST.Register      = Right $ Just Register

        bad = Left "unsupported storage class"

dsSplit :: [AST.DeclarationSpecifier] -> ([AST.StorageClass], [AST.TypeSpecifier],
                                          [AST.TypeQualifier], [AST.FunctionSpecifier],
                                          [AST.AlignmentSpecifier])
dsSplit = foldr get ([], [], [], [], [])
    where
        get (AST.DSStorageClass x) (sc, ts, tq, fs, align) = (x:sc, ts, tq, fs, align)
        get (AST.DSTypeSpecifier x) (sc, ts, tq, fs, align) = (sc, x:ts, tq, fs, align)
        get (AST.DSTypeQualifier x) (sc, ts, tq, fs, align) = (sc, ts, x:tq, fs, align)
        get (AST.DSFunctionSpecifier x) (sc, ts, tq, fs, align) = (sc, ts, tq, x:fs, align)
        get (AST.DSAlignmentSpecifier x) (sc, ts, tq, fs, align) = (sc, ts, tq, fs, x:align)
        get (AST.DSAttr _) r = r

scanTypeQualifiers :: [AST.TypeQualifier] -> Translate (Set CVR)
scanTypeQualifiers = foldr insert (Right S.empty)
    where
        insert _ e@(Left _)     = e
        insert AST.Const s    = S.insert CONST <$> s
        insert AST.Volatile s = S.insert VOLATILE <$> s
        insert AST.Restrict s = S.insert RESTRICT <$> s
        insert AST.Atomic _   = Left "Atomic not supported"

xTypeSpecifier :: [AST.TypeSpecifier] -> Translate RawType
xTypeSpecifier specs = case specs of
    [] -> Left "No type specifier given"
    [x] -> getTS x
    _ -> Left $ "Too many type specifiers: " ++ (show specs)

    where
        getTS :: AST.TypeSpecifier -> Translate RawType
        getTS AST.Void                  = Right TyVoid
        getTS AST.Char                  = Right $ signed CHAR
        getTS AST.UnsignedChar          = Right $ unsigned CHAR
        getTS AST.Short                 = Right $ signed SHORT
        getTS AST.UnsignedShort         = Right $ unsigned SHORT
        getTS AST.Int                   = Right $ signed INT
        getTS AST.UnsignedInt           = Right $ unsigned INT
        getTS AST.Int128                = Right $ signed INT128
        getTS AST.UnsignedInt128        = Right $ unsigned INT128
        getTS AST.Long                  = Right $ signed LONG
        getTS AST.UnsignedLong          = Right $ unsigned LONG
        getTS AST.LongLong              = Right $ signed LONG_LONG
        getTS AST.UnsignedLongLong      = Right $ unsigned LONG_LONG
        getTS AST.Float                 = Right TyFloat
        getTS AST.Double                = Right TyDouble
        getTS AST.Bool                  = Right TyBool
        getTS AST.Complex               = Left "complex nrs not implemented"
        getTS AST.AtomicSpecifier       = Left "atomics not implemented"
        getTS (AST.StructOrUnionSpecifier ty mnm fields) = undefined
            -- getTS TyStruct (convertStructType ty) mnm (concatMap convertStructEntry $ fields)
        getTS (AST.EnumDefSpecifier _ _) = undefined
        getTS (AST.EnumRefSpecifier _)  = undefined
        getTS (AST.TSTypedefName _)     = undefined
        getTS (AST.TSTypeofExp _)       = undefined
        getTS (AST.TSTypeofDecl _)      = undefined

        signed x = TyInt SIGNED x
        unsigned x = TyInt UNSIGNED x

{-
        convertStructType AST.Struct = Struct
        convertStructType AST.Union = Union

        convertStructEntry (AST.StructDeclaration specs declarators) = undefined
        convertStructEntry AST.StructStaticAssert = Left "Struct static assert not supported"
        -}

applyDeclarator :: Type -> (Maybe StorageClass) -> AST.Declarator -> Translate Declaration
applyDeclarator ty sc (AST.Declarator Nothing dd) = expandDD ty sc dd
applyDeclarator ty sc (AST.Declarator (Just ptr) dd) = expandDD ty sc dd >>= expandPtr ptr

-- FIXME: expand mp
expandPtr :: AST.Pointer -> Declaration -> Translate Declaration
expandPtr (AST.Pointer tq mp) (Declaration ty sc nm ml) = do
    cvr <- scanTypeQualifiers tq
    pure $ Declaration (Type (TyPointer ty) cvr) sc nm ml
expandPtr (AST.Pointer _ _) (FunDeclaration _ _ _ _) = undefined
expandPtr (AST.Pointer _ _) (TypedefDeclaration _ _) = undefined

-- FIXME: check vararg
convertParams :: AST.ParameterTypeList -> Translate [ParamEntry]
convertParams (AST.ParameterTypeList pds vararg) = do
    decls <- mapM expand pds
    mapM toEntry decls
    where
        expand (AST.PDDeclarator ds d) = xDeclaration' ds d
        expand (AST.PDAbstract ds md) = undefined

        -- FIXME: handle anonymous params
        toEntry :: Declaration -> Translate ParamEntry
        toEntry (Declaration t msc nm ml) = Right $ ParamEntry t (Just nm)
        toEntry (FunDeclaration t sc nm fs) = Left "Fun declaration can't be a param type"
        toEntry (TypedefDeclaration _ _) = undefined

expandDD :: Type -> (Maybe StorageClass) -> AST.DirectDeclarator -> Translate Declaration
expandDD ty sc (AST.DDIdentifier nm)          = Right $ Declaration ty sc nm Nothing
expandDD ty sc (AST.DDNested d)               = applyDeclarator ty sc d
expandDD ty sc (AST.DDArray dd' _ me _ _)     = expandDD (Type (TyArray ty (Just 0)) S.empty) sc dd'  -- FIXME: apply the qualifiers
expandDD ty sc (AST.DDFun dd' params)         = do
    ps <- convertParams params
    ed <- applyDeclarator ty sc (AST.Declarator Nothing dd')
    case ed of
        (Declaration ty' sc' nm _) -> pure $ Declaration (Type (TyFunction (FunType ty' ps S.empty)) S.empty) sc' nm Nothing
        (FunDeclaration _ _ _ _)   -> Left "Unexpected function declaration"
        (TypedefDeclaration _ _) -> undefined
expandDD ty sc (AST.DDFunPtr dd' nms)         = undefined

initToDecl :: AST.InitDeclarator -> AST.Declarator
initToDecl (AST.InitDeclarator d _) = d

xDeclaration' :: [AST.DeclarationSpecifier] -> AST.Declarator -> Translate Declaration
xDeclaration' ds declarator = do
    rt <- xTypeSpecifier ts
    sClass <- extractStorageClass sc
    cvr <- scanTypeQualifiers tq
    applyDeclarator (Type rt cvr) sClass $ declarator
    where
        (sc, ts, tq, fs, align) = dsSplit ds

-- Declaration Type StorageClass Identifier (Maybe Literal)
xDeclaration :: AST.Declaration -> Translate [Declaration]
xDeclaration (AST.Declaration ds declarators) =
    mapM (xDeclaration' ds) .
    map initToDecl $
    declarators
xDeclaration _ = undefined

---------------------------------------------------------------------
-- Statement translation
xStatement :: AST.Statement -> Translate Statement
xStatement (AST.LabelStatement nm s) =
    LabelStatement nm <$> xStatement s

xStatement (AST.CaseStatement e Nothing s) =
    CaseStatement <$> xExpression e <*> (pure Nothing) <*> xStatement s

xStatement (AST.CaseStatement e (Just e2) s) =
    CaseStatement <$> xExpression e <*> (Just <$> xExpression e2) <*> xStatement s

xStatement (AST.DefaultStatement s) =
    DefaultStatement <$> xStatement s

xStatement (AST.CompoundStatement nm items) =
    CompoundStatement nm <$> mapM xBlockItem items

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

xStatement (AST.ForStatement md me1 me2 me3 s) =
    undefined

xStatement (AST.GotoStatement nm) =
    pure $ GotoStatement nm

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

xBlockItem :: AST.BlockItem -> Translate BlockItem
xBlockItem (AST.BIDeclaration decl) = undefined
xBlockItem (AST.BIStatement s) = BIStatement <$> xStatement s

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
        (Declaration t _ _ _) -> do
            params' <- xParams decls
            (singleton . FunDef (FunType t params' S.empty)) <$> xStatement s
        (FunDeclaration _ _ _ _) -> undefined
        (TypedefDeclaration _ _) -> undefined
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
                _ -> Left "bad parameter"

--------------------------------------------------------------

main :: IO ()
main = do
    input <- getContents
    let ast = parse input
    case ast of
        Left e -> error e
        Right ast' -> print . xTranslationUnit $ ast'
    where
        parse s = runAlex s translation_unit

