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
import C.SourcePos

import Data.Text (Text)

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
    VarExp Identifier SourcePos |
    IntConstExp Sign IntType Integer SourcePos |
    StringConstExp Text SourcePos |
    CharConstExp Char SourcePos |
    CompoundLiteral TypeName [InitializerPair] SourcePos |
    SubscriptExp Exp Exp SourcePos |
    FuncallExp Exp [Exp] SourcePos |
    StructElt Exp Identifier SourcePos |
    StructDeref Exp Identifier SourcePos |
    CommaExp Exp Exp SourcePos |
    AssignExp AssignOp Exp Exp SourcePos |
    ConditionalExp Exp Exp Exp SourcePos|
    BinaryExp BinOp Exp Exp SourcePos |
    UnaryExp UnaryOp Exp SourcePos |
    CastExp TypeName Exp SourcePos |
    SizeofValueExp Exp SourcePos |
    SizeofTypeExp TypeName SourcePos |
    AlignofExp TypeName SourcePos |
    BlockExp [Identifier] [BlockItem] SourcePos |
    BuiltinVaArg SourcePos |
    BuiltinOffsetOf SourcePos |
    BuiltinTypesCompatible SourcePos |
    BuiltinConvertVector SourcePos
    deriving (Eq, Show)

data Declaration =
    Declaration [DeclarationSpecifier] [InitDeclarator] SourcePos |
    StaticAssert SourcePos
    deriving (Eq, Show)

data InitDeclarator =
    InitDeclarator Declarator (Maybe Initializer) SourcePos
    deriving (Eq, Show)

data Initializer =
    InitAssign Exp SourcePos |
    InitList [InitializerPair] SourcePos
    deriving (Eq, Show)

data InitializerPair =
    InitializerPair (Maybe [Designator]) Initializer SourcePos
    deriving (Eq, Show)

data Designator =
    SubscriptDesignator Exp SourcePos |
    StructDesignator Identifier SourcePos
    deriving (Eq, Show)

data DeclarationSpecifier =
    DSStorageClass StorageClass SourcePos |
    DSTypeSpecifier TypeSpecifier SourcePos |
    DSTypeQualifier TypeQualifier SourcePos |
    DSFunctionSpecifier FunctionSpecifier SourcePos |
    DSAlignmentSpecifier AlignmentSpecifier SourcePos |
    DSAttr [Attr] SourcePos
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
    Void SourcePos |
    Char SourcePos |
    UnsignedChar SourcePos |
    Short SourcePos |
    UnsignedShort SourcePos |
    Int SourcePos |
    UnsignedInt SourcePos |
    Int128 SourcePos |
    UnsignedInt128 SourcePos |
    Long SourcePos |
    UnsignedLong SourcePos |
    LongLong SourcePos |
    UnsignedLongLong SourcePos |
    Float SourcePos |
    Double SourcePos |
    Bool SourcePos |
    Complex SourcePos |
    AtomicSpecifier SourcePos |
    StructOrUnionSpecifier StructType (Maybe Identifier) (Maybe [StructDeclaration]) SourcePos |
    EnumDefSpecifier (Maybe Identifier) [Enumerator] SourcePos |
    EnumRefSpecifier Identifier SourcePos |
    TSTypedefName Identifier SourcePos |
    TSTypeofExp Exp SourcePos |
    TSTypeofDecl [DeclarationSpecifier] SourcePos
    deriving (Eq, Show)

data SpecifierQualifier =
    SQTypeSpecifier TypeSpecifier SourcePos |
    SQTypeQualifier TypeQualifier SourcePos
    deriving (Eq, Show)

data StructType =
    Struct |
    Union
    deriving (Eq, Show)

data StructDeclaration =
    StructDeclaration [SpecifierQualifier] [StructDeclarator] SourcePos|
    StructStaticAssert SourcePos
    deriving (Eq, Show)

data StructDeclarator =
    StructDeclarator Declarator (Maybe Exp) SourcePos |
    StructDeclaratorNoDecl Exp SourcePos
    deriving (Eq, Show)

data Enumerator =
    Enumerator Identifier (Maybe Exp) SourcePos
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
    AlignAsType TypeName SourcePos |
    AlignAsConst Exp SourcePos
    deriving (Eq, Show)

data Declarator =
    Declarator (Maybe Pointer) DirectDeclarator SourcePos
    deriving (Eq, Show)

data DirectDeclarator =
    DDIdentifier Identifier SourcePos |
    DDNested Declarator SourcePos |
    DDArray DirectDeclarator [TypeQualifier] (Maybe Exp) Bool Bool SourcePos |   -- bools are 'static', '*'
    DDFun DirectDeclarator ParameterTypeList SourcePos |
    DDFunPtr DirectDeclarator [Identifier] SourcePos
    deriving (Eq, Show)

data Pointer =
    Pointer [TypeQualifier] (Maybe Pointer) SourcePos
    deriving (Eq, Show)

data ParameterTypeList =
    ParameterTypeList [ParameterDeclaration] Bool SourcePos
    deriving (Eq, Show)

data ParameterDeclaration =
    PDDeclarator [DeclarationSpecifier] Declarator SourcePos |
    PDAbstract [DeclarationSpecifier] (Maybe AbstractDeclarator) SourcePos
    deriving (Eq, Show)

data AbstractDeclarator =
    AbstractPointer Pointer SourcePos |
    AbstractDeclarator (Maybe Pointer) DirectAbstractDeclarator SourcePos
    deriving (Eq, Show)

data DirectAbstractDeclarator =
    DANested AbstractDeclarator SourcePos |
    DAArray (Maybe DirectAbstractDeclarator) [TypeQualifier] (Maybe Exp) Bool SourcePos |
    DAArrayStar (Maybe DirectAbstractDeclarator) SourcePos |
    DAFun (Maybe DirectAbstractDeclarator) ParameterTypeList SourcePos
    deriving (Eq, Show)

data Asm = Asm SourcePos
    deriving (Eq, Show)

data AsmQualifier =
    AsmInline |
    AsmVolatile |
    AsmGoto
    deriving (Eq, Show)

data AsmOperand =
    AsmOperand Text Exp
    deriving (Eq, Show)

data TypeName =
    TypeName [SpecifierQualifier] (Maybe AbstractDeclarator) SourcePos
    deriving (Eq, Show)

data Statement =
    LabelStatement Identifier Statement SourcePos |
    CaseStatement Exp (Maybe Exp) Statement SourcePos |
    DefaultStatement Statement SourcePos |
    CompoundStatement [Identifier] [BlockItem] SourcePos |
    ExpressionStatement Exp SourcePos |
    IfStatement Exp Statement (Maybe Statement) SourcePos |
    SwitchStatement Exp Statement SourcePos |
    WhileStatement Exp Statement SourcePos |
    DoStatement Statement Exp SourcePos |
    ForStatement (Maybe Declaration) (Maybe Exp) (Maybe Exp) (Maybe Exp) Statement SourcePos |
    GotoStatement Identifier SourcePos |
    ContinueStatement SourcePos |
    BreakStatement SourcePos |
    ReturnStatement (Maybe Exp) SourcePos |
    EmptyStatement SourcePos |
    AsmStatement SourcePos
    deriving (Eq, Show)

data BlockItem =
    BIDeclaration Declaration SourcePos |
    BIStatement Statement SourcePos
    deriving (Eq, Show)

data TranslationUnit =
    TranslationUnit [ExternalDeclaration] SourcePos
    deriving (Eq, Show)

data ExternalDeclaration =
    ExternalDeclaration Declaration SourcePos |
    FunDef [DeclarationSpecifier] Declarator [Declaration] Statement SourcePos |
    AsmDeclaration Asm SourcePos
    deriving (Eq, Show)
