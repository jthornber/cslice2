module C.AST (
    AssignOp(..),
    BinOp(..),
    UnaryOp(..),
    Exp(..),
    Attr(..),
    Asm(..),
    AsmQualifier(..),
    AsmOperand(..),

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

import C.Identifier
import C.Int

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
    deriving (Eq, Show)

data Attr = Attr
    deriving (Eq, Show)

data Exp =
    VarExp Identifier |
    IntConstExp Sign IntType Integer |
    StringConstExp String |
    CharConstExp String |
    CompoundLiteral TypeName [InitializerPair] |
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
    AlignofExp TypeName |
    BlockExp [Identifier] [BlockItem] |     -- GNU extension
    BuiltinVaArg |
    BuiltinOffsetOf |
    BuiltinTypesCompatible |
    BuiltinConvertVector
    deriving (Eq, Show)

data Declaration =
    Declaration [DeclarationSpecifier] [InitDeclarator] |
    StaticAssert
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
    DSAlignmentSpecifier AlignmentSpecifier |
    DSAttr [Attr]
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
    UnsignedChar |
    Short |
    UnsignedShort |
    Int |
    UnsignedInt |
    Int128 |
    UnsignedInt128 |
    Long |
    UnsignedLong |
    LongLong |
    UnsignedLongLong |
    Float |
    Double |
    Bool |
    Complex |
    AtomicSpecifier |
    StructOrUnionSpecifier StructType (Maybe Identifier) (Maybe [StructDeclaration]) |
    EnumDefSpecifier (Maybe Identifier) [Enumerator] |
    EnumRefSpecifier Identifier |
    TSTypedefName Identifier |
    TSTypeofExp Exp |
    TSTypeofDecl [DeclarationSpecifier]
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
    StructDeclaration [SpecifierQualifier] [StructDeclarator] |
    StructStaticAssert
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
    DDFunPtr DirectDeclarator [Identifier]
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
    DAFun (Maybe DirectAbstractDeclarator) ParameterTypeList
    deriving (Eq, Show)

data Asm = Asm
    deriving (Eq, Show)

data AsmQualifier =
    AsmInline |
    AsmVolatile |
    AsmGoto
    deriving (Eq, Show)

data AsmOperand =
    AsmOperand String Exp
    deriving (Eq, Show)

data TypeName =
    TypeName [SpecifierQualifier] (Maybe AbstractDeclarator)
    deriving (Eq, Show)

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

data BlockItem =
    BIDeclaration Declaration |
    BIStatement Statement
    deriving (Eq, Show)

data TranslationUnit =
    TranslationUnit [ExternalDeclaration]
    deriving (Eq, Show)

data ExternalDeclaration =
    ExternalDeclaration Declaration |
    FunDef [DeclarationSpecifier] Declarator [Declaration] Statement |
    AsmDeclaration Asm
    deriving (Eq, Show)
