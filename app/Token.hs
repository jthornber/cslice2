module Token (
    Token (..)
    ) where

data Token =
    T_AUTO |
    T_BREAK |
    T_CASE |
    T_CHAR |
    T_CONST |
    T_CONTINUE |
    T_DEFAULT |
    T_DO |
    T_DOUBLE |
    T_ELSE |
    T_ENUM |
    T_EXTERN |
    T_FLOAT |
    T_FOR |
    T_GOTO |
    T_IF |
    T_INLINE |
    T_INT |
    T_LONG |
    T_REGISTER |
    T_RESTRICT |
    T_RETURN |
    T_SHORT |
    T_SIGNED |
    T_SIZEOF |
    T_STATIC |
    T_STRUCT |
    T_SWITCH |
    T_TYPEDEF |
    T_UNION |
    T_UNSIGNED |
    T_VOID |
    T_VOLATILE |
    T_WHILE |
    T_ALIGNAS |
    T_ALIGNOF |
    T_ATOMIC |
    T_BOOL |
    T_COMPLEX |
    T_GENERIC |
    T_IMAGINARY |
    T_NORETURN |
    T_STATIC_ASSERT |
    T_THREAD_LOCAL |

    T_IDENTIFIER String |
    T_INTEGER Integer String |
    T_STRING String |

    T_ARROW |
    T_ASSIGN |
    T_BIT_AND |
    T_BIT_AND_ASSIGN |
    T_BIT_OR |
    T_BIT_OR_ASSIGN |
    T_CLOSE_CURLY |
    T_CLOSE_PAREN |
    T_CLOSE_SQUARE |
    T_COLON |
    T_COMMA |
    T_DEC |
    T_DIV |
    T_DIV_ASSIGN |
    T_DOT |
    T_ELIPSIS |
    T_EQ |
    T_GT |
    T_GTE |
    T_HASH |
    T_HASH_HASH |
    T_HAT |
    T_HAT_ASSIGN |
    T_INC |
    T_LOGICAL_AND |
    T_LOGICAL_OR |
    T_LSHIFT |
    T_LSHIFT_ASSIGN |
    T_LT |
    T_LTE |
    T_MINUS |
    T_MINUS_MINUS |
    T_MINUS_ASSIGN |
    T_MOD_ASSIGN |
    T_MOD |
    T_NEQ |
    T_NOT |
    T_OPEN_CURLY |
    T_OPEN_PAREN |
    T_OPEN_SQUARE |
    T_PERCENT |
    T_PLUS |
    T_PLUS_PLUS |
    T_PLUS_ASSIGN |
    T_QUESTION |
    T_RSHIFT |
    T_RSHIFT_ASSIGN |
    T_SEMI |
    T_STAR |
    T_STAR_ASSIGN |
    T_TILDE
    deriving (Eq, Show)

