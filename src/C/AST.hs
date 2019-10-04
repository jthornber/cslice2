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

instance Pos Exp where
    getPos (VarExp _ pos) = pos
    getPos (IntConstExp _ _ _ pos) = pos
    getPos (StringConstExp _ pos) = pos
    getPos (CharConstExp _ pos) = pos
    getPos (CompoundLiteral _ _ pos) = pos
    getPos (SubscriptExp _ _ pos) = pos
    getPos (FuncallExp _ _ pos) = pos
    getPos (StructElt _ _ pos) = pos
    getPos (StructDeref _ _ pos) = pos
    getPos (CommaExp _ _ pos) = pos
    getPos (AssignExp _ _ _ pos) = pos
    getPos (ConditionalExp _ _ _ pos) = pos
    getPos (BinaryExp _ _ _ pos) = pos
    getPos (UnaryExp _ _ pos) = pos
    getPos (CastExp _ _ pos) = pos
    getPos (SizeofValueExp _ pos) = pos
    getPos (SizeofTypeExp _ pos) = pos
    getPos (AlignofExp _ pos) = pos
    getPos (BlockExp _ _ pos) = pos
    getPos (BuiltinVaArg pos) = pos
    getPos (BuiltinOffsetOf pos) = pos
    getPos (BuiltinTypesCompatible pos) = pos
    getPos (BuiltinConvertVector pos) = pos

data Declaration =
    Declaration [DeclarationSpecifier] [InitDeclarator] SourcePos |
    StaticAssert SourcePos
    deriving (Eq, Show)

instance Pos Declaration where
    getPos (Declaration _ _ pos) = pos
    getPos (StaticAssert pos) = pos

data InitDeclarator =
    InitDeclarator Declarator (Maybe Initializer) SourcePos
    deriving (Eq, Show)

instance Pos InitDeclarator where
    getPos (InitDeclarator _ _ pos) = pos

data Initializer =
    InitAssign Exp SourcePos |
    InitList [InitializerPair] SourcePos
    deriving (Eq, Show)

instance Pos Initializer where
    getPos (InitAssign _ pos) = pos
    getPos (InitList _ pos) = pos

data InitializerPair =
    InitializerPair (Maybe [Designator]) Initializer SourcePos
    deriving (Eq, Show)

instance Pos InitializerPair where
    getPos (InitializerPair _ _ pos) = pos

data Designator =
    SubscriptDesignator Exp SourcePos |
    StructDesignator Identifier SourcePos
    deriving (Eq, Show)

instance Pos Designator where
    getPos (SubscriptDesignator _ pos) = pos
    getPos (StructDesignator _ pos) = pos

data DeclarationSpecifier =
    DSStorageClass StorageClass SourcePos |
    DSTypeSpecifier TypeSpecifier SourcePos |
    DSTypeQualifier TypeQualifier SourcePos |
    DSFunctionSpecifier FunctionSpecifier SourcePos |
    DSAlignmentSpecifier AlignmentSpecifier SourcePos |
    DSAttr [Attr] SourcePos
    deriving (Eq, Show)

instance Pos DeclarationSpecifier where
    getPos (DSStorageClass _ pos) = pos
    getPos (DSTypeSpecifier _ pos) = pos
    getPos (DSTypeQualifier _ pos) = pos
    getPos (DSFunctionSpecifier _ pos) = pos
    getPos (DSAlignmentSpecifier _ pos) = pos
    getPos (DSAttr _ pos) = pos

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

instance Pos TypeSpecifier where
    getPos (Void pos) = pos
    getPos (Char pos) = pos
    getPos (UnsignedChar pos) = pos
    getPos (Short pos) = pos
    getPos (UnsignedShort pos) = pos
    getPos (Int pos) = pos
    getPos (UnsignedInt pos) = pos
    getPos (Int128 pos) = pos
    getPos (UnsignedInt128 pos) = pos
    getPos (Long pos) = pos
    getPos (UnsignedLong pos) = pos
    getPos (LongLong pos) = pos
    getPos (UnsignedLongLong pos) = pos
    getPos (Float pos) = pos
    getPos (Double pos) = pos
    getPos (Bool pos) = pos
    getPos (Complex pos) = pos
    getPos (AtomicSpecifier pos) = pos
    getPos (StructOrUnionSpecifier _ _ _ pos) = pos
    getPos (EnumDefSpecifier _ _ pos) = pos
    getPos (EnumRefSpecifier _ pos) = pos
    getPos (TSTypedefName _ pos) = pos
    getPos (TSTypeofExp _ pos) = pos
    getPos (TSTypeofDecl _ pos) = pos

data SpecifierQualifier =
    SQTypeSpecifier TypeSpecifier SourcePos |
    SQTypeQualifier TypeQualifier SourcePos
    deriving (Eq, Show)

instance Pos SpecifierQualifier where
    getPos (SQTypeSpecifier _ pos) = pos
    getPos (SQTypeQualifier _ pos) = pos

data StructType =
    Struct |
    Union
    deriving (Eq, Show)

data StructDeclaration =
    StructDeclaration [SpecifierQualifier] [StructDeclarator] SourcePos|
    StructStaticAssert SourcePos
    deriving (Eq, Show)

instance Pos StructDeclaration where
    getPos (StructDeclaration _ _ pos) = pos
    getPos (StructStaticAssert pos) = pos

data StructDeclarator =
    StructDeclarator Declarator (Maybe Exp) SourcePos |
    StructDeclaratorNoDecl Exp SourcePos
    deriving (Eq, Show)

instance Pos StructDeclarator where
    getPos (StructDeclarator _ _ pos) = pos
    getPos (StructDeclaratorNoDecl _ pos) = pos

data Enumerator =
    Enumerator Identifier (Maybe Exp) SourcePos
    deriving (Eq, Show)

instance Pos Enumerator where
    getPos (Enumerator _ _ pos) = pos

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

instance Pos AlignmentSpecifier where
    getPos (AlignAsType _ pos) = pos
    getPos (AlignAsConst _ pos) = pos

data Declarator =
    Declarator (Maybe Pointer) DirectDeclarator SourcePos
    deriving (Eq, Show)

instance Pos Declarator where
    getPos (Declarator _ _ pos) = pos

data DirectDeclarator =
    DDIdentifier Identifier SourcePos |
    DDNested Declarator SourcePos |
    DDArray DirectDeclarator [TypeQualifier] (Maybe Exp) Bool Bool SourcePos |   -- bools are 'static', '*'
    DDFun DirectDeclarator ParameterTypeList SourcePos |
    DDFunPtr DirectDeclarator [Identifier] SourcePos
    deriving (Eq, Show)

instance Pos DirectDeclarator where
    getPos (DDIdentifier _ pos) = pos
    getPos (DDNested _ pos) = pos
    getPos (DDArray _ _ _ _ _ pos) = pos
    getPos (DDFun _ _ pos) = pos
    getPos (DDFunPtr _ _ pos) = pos

data Pointer =
    Pointer [TypeQualifier] (Maybe Pointer) SourcePos
    deriving (Eq, Show)

instance Pos Pointer where
    getPos (Pointer _ _ pos) = pos

data ParameterTypeList =
    ParameterTypeList [ParameterDeclaration] Bool SourcePos
    deriving (Eq, Show)

instance Pos ParameterTypeList where
    getPos (ParameterTypeList _ _ pos) = pos

data ParameterDeclaration =
    PDDeclarator [DeclarationSpecifier] Declarator SourcePos |
    PDAbstract [DeclarationSpecifier] (Maybe AbstractDeclarator) SourcePos
    deriving (Eq, Show)

instance Pos ParameterDeclaration where
    getPos (PDDeclarator _ _ pos) = pos
    getPos (PDAbstract _ _ pos) = pos

data AbstractDeclarator =
    AbstractPointer Pointer SourcePos |
    AbstractDeclarator (Maybe Pointer) DirectAbstractDeclarator SourcePos
    deriving (Eq, Show)

instance Pos AbstractDeclarator where
    getPos (AbstractPointer _ pos) = pos
    getPos (AbstractDeclarator _ _ pos) = pos

data DirectAbstractDeclarator =
    DANested AbstractDeclarator SourcePos |
    DAArray (Maybe DirectAbstractDeclarator) [TypeQualifier] (Maybe Exp) Bool SourcePos |
    DAArrayStar (Maybe DirectAbstractDeclarator) SourcePos |
    DAFun (Maybe DirectAbstractDeclarator) ParameterTypeList SourcePos
    deriving (Eq, Show)

instance Pos DirectAbstractDeclarator where
    getPos (DANested _ pos) = pos
    getPos (DAArray _ _ _ _ pos) = pos
    getPos (DAArrayStar _ pos) = pos
    getPos (DAFun _ _ pos) = pos

data Asm = Asm SourcePos
    deriving (Eq, Show)

instance Pos Asm where
    getPos (Asm pos) = pos

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

instance Pos TypeName where
    getPos (TypeName _ _ pos) = pos

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

instance Pos Statement where
    getPos (LabelStatement _ _ pos) = pos
    getPos (CaseStatement _ _ _ pos) = pos
    getPos (DefaultStatement _ pos) = pos
    getPos (CompoundStatement _ _ pos) = pos
    getPos (ExpressionStatement _ pos) = pos
    getPos (IfStatement _ _ _ pos) = pos
    getPos (SwitchStatement _ _ pos) = pos
    getPos (WhileStatement _ _ pos) = pos
    getPos (DoStatement _ _ pos) = pos
    getPos (ForStatement _ _ _ _ _ pos) = pos
    getPos (GotoStatement _ pos) = pos
    getPos (ContinueStatement pos) = pos
    getPos (BreakStatement pos) = pos
    getPos (ReturnStatement _ pos) = pos
    getPos (EmptyStatement pos) = pos
    getPos (AsmStatement pos) = pos

data BlockItem =
    BIDeclaration Declaration SourcePos |
    BIStatement Statement SourcePos
    deriving (Eq, Show)

instance Pos BlockItem where
    getPos (BIDeclaration _ pos) = pos
    getPos (BIStatement _ pos) = pos

data TranslationUnit =
    TranslationUnit [ExternalDeclaration] SourcePos
    deriving (Eq, Show)

instance Pos TranslationUnit where
    getPos (TranslationUnit _ pos) = pos

data ExternalDeclaration =
    ExternalDeclaration Declaration SourcePos |
    FunDef [DeclarationSpecifier] Declarator [Declaration] Statement SourcePos |
    AsmDeclaration Asm SourcePos
    deriving (Eq, Show)

instance Pos ExternalDeclaration where
    getPos (ExternalDeclaration _ pos) = pos
    getPos (FunDef _ _ _ _ pos) = pos
    getPos (AsmDeclaration _ pos) = pos

