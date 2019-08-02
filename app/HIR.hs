module HIR (
    IntType(..),
    EnumEntry(..),
    StructEntry(..),
    StructType(..),
    ParamEntry(..),
    RawType(..),
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

import Identifier

import Data.Set (Set)
import qualified Data.Set as S

data IntType =
    CHAR |
    SHORT |
    INT |
    INT128 |
    LONG |
    LONG_LONG
    deriving (Eq, Show)

data EnumEntry = EnumEntry Identifier Integer
    deriving (Eq, Show)

data StructEntry = StructEntry Type (Maybe Identifier) (Maybe Integer)
    deriving (Eq, Show)

data ParamEntry =
    ParamEntry Type (Maybe Identifier)
    deriving (Eq, Show)

data FunFlags =
    INLINE |
    NORETURN |
    VARARG
    deriving (Eq, Ord, Show)

data FunType = FunType Type [ParamEntry] (Set FunFlags)
    deriving (Eq, Show)

data StructType =
    Struct |
    Union
    deriving (Eq, Show)

data Sign =
    SIGNED |
    UNSIGNED
    deriving (Eq, Show)

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

    -- Typedefs
    TyAlias Identifier Type

    deriving (Eq, Show)

data CVR =
    CONST |
    VOLATILE |
    RESTRICT
    deriving (Eq, Ord, Show)

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

data Declaration =
    Declaration Type (Maybe StorageClass) Identifier (Maybe Literal) |
    FunDeclaration Type StorageClass Identifier (Set FunctionSpecifier)
    deriving (Eq, Show)

data FunctionSpecifier =
    Inline |
    Noreturn
    deriving (Eq, Ord, Show)

-- FIXME: how does this differ from LiteralExp?
data Literal = Literal Type Value
    deriving (Eq, Show)

data Value =
    IntValue Integer |
    FloatValue Float |
    DoubleValue Double |
    EnumValue Identifier |
    StructValue [Value] |
    StringValue String |
    CharValue Char
    deriving (Eq, Show)

data StorageClass =
    Extern |
    Static |
    Auto |
    Register
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
    FunDef FunType Statement |
    AsmDeclaration
    deriving (Eq, Show)

