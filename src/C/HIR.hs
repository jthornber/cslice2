module C.HIR (
    IntType(..),
    EnumEntry(..),
    StructEntry(..),
    StructType(..),
    ParamEntry(..),
    RawType(..),
    FunFlag(..),
    FunType(..),
    Sign(..),
    CVR(..),
    Type(..),
    BinOp(..),
    UnaryOp(..),
    Exp(..),
    Declaration(..),
    Literal(..),
    Value(..),
    StorageClass(..),
    Statement(..),
    BlockItem(..),
    TranslationUnit(..),
    ExternalDeclaration(..)
    ) where

import C.Identifier

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc

printCVR :: Set CVR -> Doc ann
printCVR = hsep . map pretty . S.toList

printType :: Type -> Maybe Identifier -> Doc ann
printType (Type rt cvr) Nothing = printCVR cvr <+> printAbstractType rt
printType (Type rt cvr) (Just nm) = printCVR cvr <+> printConcreteType rt nm

printAbstractType :: RawType -> Doc ann
printAbstractType TyVoid = pretty "void"
printAbstractType TyBool = pretty "bool"

printAbstractType (TyInt sign ty) =
    pretty sign <+> pretty ty

printAbstractType TyFloat = pretty "float"
printAbstractType TyDouble = pretty "double"

printAbstractType TyLongDouble = pretty "long double"

printAbstractType (TyEnumeration mnm menum) = undefined

printAbstractType (TyArray ty Nothing) =
    printType ty Nothing <+> lbracket <> rbracket

printAbstractType (TyArray ty (Just size)) = hsep [
    printType ty Nothing,
    lbracket,
    pretty size,
    rbracket]

printAbstractType (TyStruct st mnm mentries) = header <+> body
    where
        header = hsep [pretty "struct", maybe emptyDoc pretty mnm]
        body = case mentries of
            Nothing -> emptyDoc
            (Just entries) -> lbracket <+> (vsep $ map pretty entries) <+> rbracket

printAbstractType (TyPointer ty) =
    printType ty Nothing <+> (pretty '*')

printAbstractType (TyFunction ft) = pretty ft

printAbstractType (TyAlias nm _) = pretty nm

printConcreteType :: RawType -> Identifier -> Doc ann
printConcreteType TyVoid nm = pretty "void" <+> pretty nm
printConcreteType TyBool nm = pretty "bool" <+> pretty nm

printConcreteType (TyInt sign ty) nm =
    pretty sign <+> pretty ty <+> pretty nm

printConcreteType TyFloat nm = pretty "float" <+> pretty nm
printConcreteType TyDouble nm = pretty "double" <+> pretty nm

printConcreteType TyLongDouble nm = pretty "long double" <+> pretty nm

printConcreteType (TyEnumeration mnm menum) _ = undefined

printConcreteType (TyArray ty Nothing) nm =
    printType ty (Just nm) <+> lbracket <> rbracket

printConcreteType (TyArray ty (Just size)) nm = hsep [
    printType ty (Just nm),
    lbracket,
    pretty size,
    rbracket]

printConcreteType (TyStruct st mnm mentries) nm = header <+> body
    where
        header = hsep [pretty "struct", maybe emptyDoc pretty mnm, pretty nm]
        body = case mentries of
            Nothing -> emptyDoc
            (Just entries) -> lbracket <+> (vsep $ map pretty entries) <+> rbracket

printConcreteType (TyPointer ty) nm =
    printType ty Nothing <+> (pretty '*') <+> pretty nm

printConcreteType (TyFunction ft) _ = pretty ft

printConcreteType (TyAlias alias _) nm = pretty alias <+> pretty nm

data IntType =
    CHAR |
    SHORT |
    INT |
    INT128 |
    LONG |
    LONG_LONG
    deriving (Eq, Show)

instance Pretty IntType where
    pretty CHAR = pretty "char"
    pretty SHORT = pretty "short"
    pretty INT = pretty "int"
    pretty INT128 = pretty "__int128"
    pretty LONG = pretty "long"
    pretty LONG_LONG = pretty "long long"

data EnumEntry = EnumEntry Identifier Integer
    deriving (Eq, Show)

instance Pretty EnumEntry where
    pretty (EnumEntry nm n) = (pretty nm) <+> (pretty "=") <+> (pretty n)

data StructEntry = StructEntry Type (Maybe Identifier) (Maybe Integer)
    deriving (Eq, Show)

instance Pretty StructEntry where
    pretty (StructEntry t mnm Nothing) = printType t mnm
    pretty (StructEntry t mnm (Just n)) =
        (printType t mnm) <+> (pretty ":") <+> (pretty n)

data ParamEntry =
    ParamEntry Type (Maybe Identifier)
    deriving (Eq, Show)

instance Pretty ParamEntry where
    pretty (ParamEntry t mnm) = printType t mnm

data FunFlag =
    INLINE |
    NORETURN |
    VARARG
    deriving (Eq, Ord, Show)

instance Pretty FunFlag where
    pretty INLINE = pretty "inline"
    pretty NORETURN = pretty "__Noreturn"
    pretty VARARG = pretty "..."

data FunType = FunType Type [ParamEntry] (Set FunFlag)
    deriving (Eq, Show)

instance Pretty FunType where
    pretty (FunType t params flags) = inline <> noreturn <> printType t Nothing <+> params'
       where
        flag f = if S.member f flags
                 then pretty f <> pretty " "
                 else emptyDoc

        inline = flag INLINE
        noreturn = flag NORETURN

        ps = map pretty params
        ps' = if S.member VARARG flags
              then ps ++ [pretty VARARG]
              else ps
        params' = encloseSep lbracket rbracket comma ps'

data StructType =
    Struct |
    Union
    deriving (Eq, Show)

instance Pretty StructType where
    pretty Struct = pretty "struct"
    pretty Union = pretty "union"

data Sign =
    SIGNED |
    UNSIGNED
    deriving (Eq, Show)

instance Pretty Sign where
    pretty SIGNED = emptyDoc -- signed is always optional
    pretty UNSIGNED = pretty "unsigned"

data RawType =
    -- Basic types
    TyVoid |
    TyBool |
    TyInt Sign IntType |  -- Signed
    TyFloat |
    TyDouble |
    TyLongDouble |

    -- Enumerations
    TyEnumeration (Maybe Identifier) (Maybe [EnumEntry]) |

    -- Derived types
    TyArray Type (Maybe Integer) |
    TyStruct StructType (Maybe Identifier) (Maybe [StructEntry]) |
    TyPointer Type |

    TyFunction FunType |

    -- Typedefs.  This is a reference to a previously defined typedef
    TyAlias Identifier Type

    deriving (Eq, Show)

data CVR =
    CONST |
    VOLATILE |
    RESTRICT
    deriving (Eq, Ord, Show)

instance Pretty CVR where
    pretty CONST = pretty "const"
    pretty VOLATILE = pretty "volatile"
    pretty RESTRICT = pretty "restrict"

data Type = Type RawType (Set CVR)
    deriving (Eq, Show)

data BinOp =
    LOGICAL_OR |
    LOGICAL_AND |
    BIT_OR |
    BIT_AND |
    XOR |
    EQ |
    NEQ |
    LT |
    LTE |
    GT |
    GTE |
    LSHIFT |
    RSHIFT |
    PLUS |
    MINUS |
    MULT |
    DIV |
    MOD
    deriving (Eq, Show)

instance Pretty BinOp where
    pretty LOGICAL_OR = pretty "||"
    pretty LOGICAL_AND = pretty "&&"
    pretty BIT_OR = pretty "|"
    pretty BIT_AND = pretty "&"
    pretty XOR = pretty "^"
    pretty C.HIR.EQ = pretty "=="
    pretty NEQ = pretty "!="
    pretty C.HIR.LT = pretty "<"
    pretty LTE = pretty "<="
    pretty C.HIR.GT = pretty ">"
    pretty GTE = pretty ">="
    pretty LSHIFT = pretty "<<"
    pretty RSHIFT = pretty ">>"
    pretty PLUS = pretty "+"
    pretty MINUS = pretty "-"
    pretty MULT = pretty "*"
    pretty DIV = pretty "/"
    pretty MOD = pretty "%"

data UnaryOp =
    ADDRESS_OF |
    LABEL_ADDRESS |
    DEREF |
    UNARY_PLUS |
    UNARY_MINUS |
    BIT_NOT |
    LOGICAL_NOT |
    PRE_INC |
    POST_INC |
    PRE_DEC |
    POST_DEC
    deriving (Eq, Ord, Show)

instance Pretty UnaryOp where
    pretty ADDRESS_OF = pretty "&"
    pretty LABEL_ADDRESS = pretty "&&"
    pretty DEREF = pretty "*"
    pretty UNARY_PLUS = pretty "+"
    pretty UNARY_MINUS = pretty "-"
    pretty BIT_NOT = pretty "~"
    pretty LOGICAL_NOT = pretty "!"
    pretty PRE_INC = pretty "++"
    pretty POST_INC = pretty "++"
    pretty PRE_DEC = pretty "--"
    pretty POST_DEC = pretty "--"

data Exp =
    VarExp Identifier |
    LiteralExp Value |
    SubscriptExp Exp Exp |
    FuncallExp Exp [Exp] |
    StructElt Exp Identifier |
    StructDeref Exp Identifier |
    CommaExp Exp Exp |
    AssignExp Exp Exp |
    ConditionalExp Exp Exp Exp |
    BinaryExp BinOp Exp Exp |
    UnaryExp UnaryOp Exp |
    CastExp Type Exp |
    SizeofValueExp Exp |
    SizeofTypeExp Type |
    AlignofExp Type |
    BlockExp [Identifier] [BlockItem] |     -- GNU extension
    BuiltinVaArg |
    BuiltinOffsetOf |
    BuiltinTypesCompatible |
    BuiltinConvertVector
    deriving (Eq, Show)

instance Pretty Exp where
    pretty (VarExp nm) = pretty nm
    pretty (LiteralExp v) = pretty v

    pretty (SubscriptExp e1 e2) = hsep [
        lbracket,
        pretty e1,
        rbracket,
        pretty '[',
        pretty e2,
        pretty ']' ]

    pretty (FuncallExp e1 params) = hsep [
        pretty '(',
        pretty e1,
        pretty ')'] <> encloseSep lbracket rbracket comma (map pretty params)

    pretty (StructElt e nm) = hsep [
        lbracket,
        pretty e,
        rbracket,
        pretty '.',
        pretty nm]

    pretty (StructDeref e nm) = hsep [
        lbracket,
        pretty e,
        rbracket,
        pretty "->",
        pretty nm]

    pretty (CommaExp e1 e2) = hsep [pretty e1, comma, pretty e2]

    pretty (AssignExp e1 e2) = hsep [pretty e1, pretty '=', pretty e2]
    pretty (ConditionalExp e1 e2 e3) = hsep [
        parens $ pretty e1,
        pretty '?',
        parens $ pretty e2,
        pretty ':',
        parens $ pretty e3]

    pretty (BinaryExp op e1 e2) = hsep [
        parens $ pretty e1,
        pretty op,
        parens $ pretty e2]
    pretty (UnaryExp op e) = hsep [pretty op, parens $ pretty e]

    pretty (CastExp t e) = hsep [
        parens $ printType t Nothing,
        parens $ pretty e]

    pretty (SizeofValueExp e) = hsep [
        pretty "sizeof",
        parens $ pretty e]

    pretty (SizeofTypeExp t) = hsep [
        pretty "sizeof",
        parens $ printType t Nothing]

    pretty (AlignofExp t) = hsep [
        pretty "alignof",
        parens $ printType t Nothing]

    -- FIXME: not sure what the nms are for
    pretty (BlockExp _ items) = parens $
        encloseSep lbracket rbracket (pretty ';' <> line) (map pretty items)

    pretty (BuiltinVaArg) = pretty "__builtinvararg"
    pretty (BuiltinOffsetOf) = pretty "__builtinoffsetof"
    pretty (BuiltinTypesCompatible) = pretty "__builtintypescompatible"
    pretty (BuiltinConvertVector) = pretty "__builtinconvertvector"

data Declaration =
    Declaration Type (Maybe StorageClass) Identifier (Maybe Literal) |
    FunDeclaration Type StorageClass Identifier (Set FunctionSpecifier) |
    TypedefDeclaration Type Identifier
    deriving (Eq, Show)

instance Pretty Declaration where
    pretty (Declaration t msc nm mlit) = hsep [
        maybe (emptyDoc) pretty msc,
        printType t (Just nm),
        maybe (emptyDoc) pretty mlit]

    pretty (FunDeclaration t sc nm flags) = hsep [
        pretty sc,
        hsep $ map pretty $ S.toList flags,
        printType t (Just nm)]

    pretty (TypedefDeclaration t nm) = hsep [
        pretty "typedef",
        printType t (Just nm)]

data FunctionSpecifier =
    Inline |
    Noreturn
    deriving (Eq, Ord, Show)

instance Pretty FunctionSpecifier where
    pretty Inline = pretty "inline"
    pretty Noreturn = pretty "__noreturn"

-- FIXME: how does this differ from LiteralExp?  Why do we have the type included?
data Literal = Literal Type Value
    deriving (Eq, Show)

instance Pretty Literal where
    pretty (Literal t v) = hsep [
        parens $ printType t Nothing,
        pretty v]

data Value =
    IntValue Integer |
    FloatValue Float |
    DoubleValue Double |
    EnumValue Identifier |
    StructValue [Value] |
    StringValue String |
    CharValue Char
    deriving (Eq, Show)

instance Pretty Value where
    pretty (IntValue n) = pretty n
    pretty (FloatValue f) = pretty f
    pretty (DoubleValue d) = pretty d
    pretty (EnumValue nm) = pretty nm
    pretty (StructValue vs) = encloseSep lbrace rbrace comma $ map pretty vs
    pretty (StringValue str) = dquotes $ pretty str    -- FIXME: handle quoting
    pretty (CharValue c) = squotes $ pretty c          -- FIXME: handle quoting

data StorageClass =
    Extern |
    Static |
    Auto |
    Register
    deriving (Eq, Show)

instance Pretty StorageClass where
    pretty Extern = pretty "extern"
    pretty Static = pretty "static"
    pretty Auto = pretty "auto"
    pretty Register = pretty "register"

data Statement =
    LabelStatement Identifier Statement |
    CaseStatement Exp (Maybe Exp) Statement |
    DefaultStatement Statement |
    CompoundStatement [Identifier] [BlockItem] |
    ExpressionStatement Exp |
    IfStatement Exp Statement (Maybe Statement) |
    SwitchStatement Exp Statement |
    WhileStatement Exp Statement |
    DoStatement Statement Exp |
    ForStatement (Maybe Declaration) (Maybe Exp) (Maybe Exp) (Maybe Exp) Statement |
    GotoStatement Identifier |
    ContinueStatement |
    BreakStatement |
    ReturnStatement (Maybe Exp) |
    EmptyStatement |
    AsmStatement
    deriving (Eq, Show)

withSemi :: Doc ann -> Doc ann
withSemi d = d <> pretty ";"

instance Pretty Statement where
    pretty (LabelStatement nm s) = withSemi $ vsep [
        pretty nm <> pretty ":",
        pretty s]

    pretty (CaseStatement e Nothing s) = withSemi $ vsep [
        pretty e <> pretty ":",
        pretty s]

    pretty (CaseStatement e1 (Just e2) s) = withSemi $ vsep [
        pretty e1 <> pretty ".." <> pretty e2 <> pretty ":",
        pretty s]

    pretty (DefaultStatement s) = withSemi $ vsep [
        pretty "default:",
        pretty s]

    -- FIXME: what are the identifiers?
    pretty (CompoundStatement _ items) =
        withSemi . brackets . indent 8 . vsep . map pretty $ items

    pretty (ExpressionStatement e) = withSemi $ pretty e

    pretty (IfStatement e t Nothing) = withSemi $ vsep [
        hsep [pretty "if", parens $ pretty e],
        pretty t]

    pretty (IfStatement e t (Just f)) = withSemi $ vsep [
        hsep [pretty "if", parens $ pretty e],
        pretty t,
        pretty "else",
        pretty f]

    pretty (SwitchStatement e s) = withSemi $ vsep [
        hsep [pretty "switch", parens $ pretty e],
        pretty s]

    pretty (WhileStatement e s) = withSemi $ vsep [
        hsep [pretty "while", parens $ pretty e],
        pretty s]

    pretty (DoStatement s e) = withSemi $ vsep [
        pretty "do" <> pretty s,
        parens $ pretty e]

    pretty (ForStatement md me1 me2 me3 s) = withSemi $ vsep [
        hsep [pretty "for", lparen, praps md, praps me1,
              semi, praps me2, semi, praps me3, rparen],
        pretty s]

    pretty (GotoStatement nm) = withSemi $ pretty "goto" <> pretty nm

    pretty (ContinueStatement) = withSemi $ pretty "continue"

    pretty (BreakStatement) = withSemi $ pretty "break"

    pretty (ReturnStatement Nothing) = withSemi $ pretty "return"
    pretty (ReturnStatement (Just e)) = withSemi $ pretty "return" <+> pretty e
    pretty (EmptyStatement) = withSemi emptyDoc
    pretty (AsmStatement) = error "assembly not supported"

praps :: Pretty a => Maybe a -> Doc ann
praps Nothing = emptyDoc
praps (Just x) = pretty x

data BlockItem =
    BIDeclaration Declaration |
    BIStatement Statement
    deriving (Eq, Show)

instance Pretty BlockItem where
    pretty (BIDeclaration d) = pretty d
    pretty (BIStatement s) = pretty s

data TranslationUnit =
    TranslationUnit [ExternalDeclaration]
    deriving (Eq, Show)

instance Pretty TranslationUnit where
    pretty (TranslationUnit decls) = vsep $ map pretty decls

data ExternalDeclaration =
    ExternalDeclaration Declaration |
    FunDef FunType Statement |
    AsmDeclaration
    deriving (Eq, Show)

instance Pretty ExternalDeclaration where
    pretty (ExternalDeclaration decl) = pretty decl <> semi
    pretty (FunDef ft s) = pretty ft <> pretty s
    pretty (AsmDeclaration) = error "asm not supported"

