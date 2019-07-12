module AST (
    AssignOp(..),
    BinOp(..),
    UnaryOp(..),
    Type(..),
    Exp(..),

    Declaration(..),
    InitDeclarator(..),
    Initializer(..),
    InitializerPair(..),
    Designator(..),
    DeclarationSpecifier(..),
    StorageClass(..),
    TypeSpecifier(..),
    SpecifierQualifier(..),
    StructType(..),
    StructDeclaration(..),
    StructDeclarator(..),
    Enumerator(..),
    TypeQualifier(..),
    FunctionSpecifier(..),
    AlignmentSpecifier(..),
    Declarator(..),
    DirectDeclarator(..),
    Pointer(..),
    ParameterTypeList(..),
    ParameterDeclaration(..),
    AbstractDeclarator(..),
    DirectAbstractDeclarator(..),
    TypeName(..),

    Statement(..),
    BlockItem(..),

    TranslationUnit(..),
    ExternalDeclaration(..)
    ) where

import Token
import Lexer

data AssignOp =
    ASSIGN |
    MULT_ASSIGN |
    DIV_ASSIGN |
    MOD_ASSIGN |
    PLUS_ASSIGN |
    MINUS_ASSIGN |
    LSHIFT_ASSIGN |
    RSHIFT_ASSIGN |
    AND_ASSIGN |
    XOR_ASSIGN |
    OR_ASSIGN
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

data UnaryOp =
    ADDRESS_OF |
    DEREF |
    UNARY_PLUS |
    UNARY_MINUS |
    BIT_NOT |
    LOGICAL_NOT |
    PRE_INC |
    POST_INC |
    PRE_DEC |
    POST_DEC
    deriving (Eq, Show)

data Exp =
    VarExp Identifier |
    ConstExp (Token AlexPosn) |
    StringConstExp (Token AlexPosn) |
    SubscriptExp Exp Exp |
    FuncallExp Exp [Exp] |
    StructElt Exp Identifier |
    StructDeref Exp Identifier |
    CommaExp Exp Exp |
    AssignExp AssignOp Exp Exp |
    ConditionalExp Exp Exp Exp |
    BinaryExp BinOp Exp Exp |
    UnaryExp UnaryOp Exp |
    CastExp TypeName Exp |
    SizeofValueExp Exp |
    SizeofTypeExp TypeName |
    AlignofExp Exp
    deriving (Eq, Show)

data Declaration =
    Declaration [DeclarationSpecifier] [InitDeclarator]
    deriving (Eq, Show)

data InitDeclarator =
    InitDeclarator Declarator (Maybe Initializer)
    deriving (Eq, Show)

data Initializer =
    InitAssign Exp |
    InitList [InitializerPair]
    deriving (Eq, Show)

data InitializerPair =
    InitializerPair (Maybe [Designator]) Initializer
    deriving (Eq, Show)

data Designator =
    SubscriptDesignator Exp |
    StructDesignator Identifier
    deriving (Eq, Show)

data DeclarationSpecifier =
    DSStorageClass StorageClass |
    DSTypeSpecifier TypeSpecifier |
    DSTypeQualifier TypeQualifier |
    DSFunctionSpecifier FunctionSpecifier |
    DSAlignmentSpecifier AlignmentSpecifier
    deriving (Eq, Show)

data StorageClass =
    Typedef |
    Extern |
    Static |
    ThreadLocal |
    Auto |
    Register
    deriving (Eq, Show)

data TypeSpecifier =
    Void |
    Char |
    Short |
    Int |
    Long |
    Float |
    Double |
    Signed |
    Unsigned |
    Bool |
    Complex |
    AtomicSpecifier |
    StructOrUnionSpecifier StructType (Maybe Identifier) (Maybe [StructDeclaration]) |
    EnumDefSpecifier (Maybe Identifier) [Enumerator] |
    EnumRefSpecifier Identifier |
    TSTypedefName Identifier
    deriving (Eq, Show)

data SpecifierQualifier =
    SQTypeSpecifier TypeSpecifier |
    SQTypeQualifier TypeQualifier
    deriving (Eq, Show)

data StructType =
    Struct |
    Union
    deriving (Eq, Show)

data StructDeclaration =
    StructDeclaration [SpecifierQualifier] [StructDeclarator]
    deriving (Eq, Show)

data StructDeclarator =
    StructDeclarator Declarator (Maybe Exp) |
    StructDeclaratorNoDecl Exp
    deriving (Eq, Show)

data Enumerator =
    Enumerator Identifier (Maybe Exp)
    deriving (Eq, Show)

data TypeQualifier =
    Const |
    Restrict |
    Volatile |
    Atomic
    deriving (Eq, Show)

data FunctionSpecifier =
    Inline |
    NoReturn
    deriving (Eq, Show)

data AlignmentSpecifier =
    AlignAsType TypeName |
    AlignAsConst Exp
    deriving (Eq, Show)

data Declarator =
    Declarator (Maybe Pointer) DirectDeclarator
    deriving (Eq, Show)

data DirectDeclarator =
    DDIdentifier Identifier |
    DDNested Declarator |
    DDArray DirectDeclarator [TypeQualifier] (Maybe Exp) Bool Bool |   -- bools are 'static', '*'
    DDFun DirectDeclarator ParameterTypeList |
    DDFunOdd DirectDeclarator [Identifier]
    deriving (Eq, Show)

data Pointer =
    Pointer [TypeQualifier] (Maybe Pointer)
    deriving (Eq, Show)

data ParameterTypeList =
    ParameterTypeList [ParameterDeclaration] Bool
    deriving (Eq, Show)

data ParameterDeclaration =
    PDDeclarator [DeclarationSpecifier] Declarator |
    PDAbstract [DeclarationSpecifier] (Maybe AbstractDeclarator)
    deriving (Eq, Show)

data AbstractDeclarator =
    AbstractPointer Pointer |
    AbstractDeclarator (Maybe Pointer) DirectAbstractDeclarator
    deriving (Eq, Show)

data DirectAbstractDeclarator =
    DANested AbstractDeclarator |
    DAArray (Maybe DirectAbstractDeclarator) [TypeQualifier] (Maybe Exp) Bool |
    DAArrayStar (Maybe DirectAbstractDeclarator) |
    DAFun (Maybe DirectAbstractDeclarator) (Maybe ParameterTypeList)
    deriving (Eq, Show)

data TypeName =
    TypeName [SpecifierQualifier] (Maybe AbstractDeclarator)
    deriving (Eq, Show)

-- FIXME: Do we need this?
data Type = Type
    deriving (Eq, Show)

data Statement =
    LabelStatement Identifier Statement |
    CaseStatement Exp Statement |
    DefaultStatement Statement |
    CompoundStatement [BlockItem] |
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
    EmptyStatement
    deriving (Eq, Show)

data BlockItem =
    BIDeclaration Declaration |
    BIStatement Statement
    deriving (Eq, Show)

data TranslationUnit =
    TranslationUnit [ExternalDeclaration]
    deriving (Eq, Show)

data ExternalDeclaration =
    ExternalDeclaration Declaration |
    FunDef [DeclarationSpecifier] Declarator [Declaration] Statement
    deriving (Eq, Show)
