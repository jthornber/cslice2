module AST (
    AssignOp(..),
    BinOp(..),
    UnaryOp(..),
    Type(..),
    Exp(..)
    ) where

import Token

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

data Type =
    Void
    deriving (Eq, Show)

data Exp =
    VarExp Token |
    ConstExp Token |
    StringConstExp Token |
    SubscriptExp Exp Exp |
    FuncallExp Exp [Exp] |
    StructElt Exp Token |
    StructDeref Exp Token |
    CommaExp Exp Exp |
    AssignExp AssignOp Exp Exp |
    ConditionalExp Exp Exp Exp |
    BinaryExp BinOp Exp Exp |
    UnaryExp UnaryOp Exp |
    CastExp Type Exp |
    SizeofValueExp Exp |
    SizeofTypeExp Type |
    AlignofExp Exp
    deriving (Eq, Show)

