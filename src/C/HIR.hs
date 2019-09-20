{-# LANGUAGE TemplateHaskell #-}

module C.HIR (
    EnumEntry(..),
    StructEntry(..),
    StructType(..),
    ParamEntry(..),
    RawType(..),
    FunFlag(..),
    FunType(..),
    CVR(..),
    Type(..),
    BinOp(..),
    UnaryOp(..),
    RawExp(..),
    Exp(..),
    Declaration(..),
    FunctionSpecifier(..),
    Literal(..),
    Value(..),
    Initializer(..),
    InitializerPair(..),
    Designator(..),
    StorageClass(..),
    Statement(..),
    BlockItem(..),
    TranslationUnit(..),
    ExternalDeclaration(..)
    ) where

import C.Int
import C.SymbolTable

import Data.Set (Set)

data EnumEntry = EnumEntry Symbol (Maybe Exp)
    deriving (Eq, Show)

data StructEntry =
    StructEntry Type (Maybe Symbol) (Maybe Exp) |
    StructEntryWidthOnly Exp
    deriving (Eq, Show)

data ParamEntry =
    ParamEntry Type (Maybe Symbol)
    deriving (Eq, Show)

data FunFlag =
    INLINE |
    NORETURN |
    VARARG
    deriving (Eq, Ord, Show)

data FunType = FunType Type [ParamEntry] (Set FunFlag)
    deriving (Eq, Show)

data StructType =
    Struct |
    Union
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
    -- FIXME: remove maybe from symbol and use anonymous sym if necc.
    TyEnum (Maybe Symbol) (Maybe [EnumEntry]) |

    -- Derived types
    TyArray Type (Maybe Exp) |
    TyStruct StructType Symbol [StructEntry] |
    TyStructRef StructType Symbol |
    TyPointer Type |

    TyFunction FunType |

    -- Typedefs.  This is a reference to a previously defined typedef
    TyAlias Symbol |

    TyTypeofExp Exp |
    TyTypeofType Type

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
    deriving (Eq, Ord, Show)

data RawExp =
    VarExp Symbol |
    LiteralExp Value |
    SubscriptExp Exp Exp |
    FuncallExp Exp [Exp] |
    StructElt Exp Symbol |
    StructDeref Exp Symbol |
    CommaExp Exp Exp |
    AssignExp Exp Exp |
    ConditionalExp Exp Exp Exp |
    BinaryExp BinOp Exp Exp |
    UnaryExp UnaryOp Exp |
    CastExp Type Exp |
    ImplicitCastExp Type Exp |
    SizeofValueExp Exp |
    SizeofTypeExp Type |
    AlignofExp Type |
    BlockExp [Symbol] [BlockItem] |     -- GNU extension
    BuiltinVaArg |
    BuiltinOffsetOf |
    BuiltinTypesCompatible |
    BuiltinConvertVector
    deriving (Eq, Show)

data Exp = Exp Type RawExp
    deriving (Eq, Show)

-- FIXME: ideally the TypeDeclaration would contain a symbol and
-- we'd lose the Maybe Symbol from TyStruct and TyEnum.  But
-- this would require splitting declarations into two sometimes.  eg,
--
--   struct foo {
--     int x;
--   } bar;
--
-- would become:
--
--   struct foo {
--     int x;
--   };
--   struct foo bar;
--
-- I'd like to do this eventually.
data Declaration =
    VarDeclaration Type (Maybe StorageClass) Symbol (Maybe Literal) |
    FunDeclaration Type StorageClass Symbol (Set FunctionSpecifier) |
    TypedefDeclaration Type Symbol |
    TypeDeclaration Type -- eg, a struct, union or enum
    deriving (Eq, Show)

data FunctionSpecifier =
    Inline |
    Noreturn
    deriving (Eq, Ord, Show)

-- FIXME: how does this differ from LiteralExp?  Why do we have the type included?
data Literal = Literal Type Value
    deriving (Eq, Show)

data Value =
    IntValue Integer |
    FloatValue Float |
    DoubleValue Double |
    EnumValue Symbol |
    CompoundValue Type [InitializerPair] |
    StringValue String |
    CharValue Char
    deriving (Eq, Show)

data Initializer =
    InitAssign Exp |
    InitList [InitializerPair]
    deriving (Eq, Show)

data InitializerPair = InitializerPair (Maybe [Designator]) Initializer
    deriving (Eq, Show)

data Designator =
    SubscriptDesignator Exp |
    StructDesignator Symbol
    deriving (Eq, Show)

data StorageClass =
    Extern |
    Static |
    Auto |
    Register
    deriving (Eq, Show)

data Statement =
    LabelStatement Symbol Statement |
    CaseStatement Exp (Maybe Exp) Statement |
    DefaultStatement Statement |
    CompoundStatement [Symbol] [BlockItem] |
    ExpressionStatement Exp |
    IfStatement Exp Statement (Maybe Statement) |
    SwitchStatement Exp Statement |
    WhileStatement Exp Statement |
    DoStatement Statement Exp |
    ForStatement [Declaration] (Maybe Exp) (Maybe Exp) (Maybe Exp) Statement |
    GotoStatement Symbol |
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
    FunDef FunType Symbol Statement |
    AsmDeclaration
    deriving (Eq, Show)
