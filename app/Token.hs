module Token (
    Identifier(..),
    Token (..),
    getAttr
    ) where

data Identifier =
    Identifier String
    deriving (Eq, Show, Ord)

data Token a =
    T_ASM a |
    T_ATTRIBUTE a |
    T_AUTO a |
    T_BREAK a |
    T_CASE a |
    T_CHAR a |
    T_CONST a |
    T_CONTINUE a |
    T_DEFAULT a |
    T_DO a |
    T_DOUBLE a |
    T_ELSE a |
    T_ENUM a |
    T_EXTERN a |
    T_FLOAT a |
    T_FOR a |
    T_GOTO a |
    T_IF a |
    T_INLINE a |
    T_INT a |
    T_LONG a |
    T_REGISTER a |
    T_RESTRICT a |
    T_RETURN a |
    T_SHORT a |
    T_SIGNED a |
    T_SIZEOF a |
    T_STATIC a |
    T_STRUCT a |
    T_SWITCH a |
    T_TYPEOF a |
    T_TYPEDEF a |
    T_UNION a |
    T_UNSIGNED a |
    T_VOID a |
    T_VOLATILE a |
    T_WHILE a |
    T_ALIGNAS a |
    T_ALIGNOF a |
    T_ATOMIC a |
    T_BOOL a |
    T_COMPLEX a |
    T_GENERIC a |
    T_IMAGINARY a |
    T_NORETURN a |
    T_STATIC_ASSERT a |
    T_THREAD_LOCAL a |

    T_IDENTIFIER String a |
    T_INTEGER Integer String a |
    T_CHAR_LIT String a |
    T_STRING String a |
    T_TYPEDEF_NAME String a |

    T_ARROW a |
    T_ASSIGN a |
    T_BIT_AND a |
    T_BIT_AND_ASSIGN a |
    T_BIT_OR a |
    T_BIT_OR_ASSIGN a |
    T_CLOSE_CURLY a |
    T_CLOSE_PAREN a |
    T_CLOSE_SQUARE a |
    T_COLON a |
    T_COMMA a |
    T_DIV a |
    T_DIV_ASSIGN a |
    T_DOT a |
    T_ELIPSIS a |
    T_EQ a |
    T_GT a |
    T_GTE a |
    T_HASH a |
    T_HASH_HASH a |
    T_HAT a |
    T_HAT_ASSIGN a |
    T_INC a |
    T_DEC a |
    T_LOGICAL_AND a |
    T_LOGICAL_OR a |
    T_LSHIFT a |
    T_LSHIFT_ASSIGN a |
    T_LT a |
    T_LTE a |
    T_MINUS a |
    T_MINUS_ASSIGN a |
    T_MOD_ASSIGN a |
    T_MOD a |
    T_NEQ a |
    T_NOT a |
    T_OPEN_CURLY a |
    T_OPEN_PAREN a |
    T_OPEN_SQUARE a |
    T_PERCENT a |
    T_PLUS a |
    T_PLUS_ASSIGN a |
    T_QUESTION a |
    T_RSHIFT a |
    T_RSHIFT_ASSIGN a |
    T_SEMI a |
    T_STAR a |
    T_STAR_ASSIGN a |
    T_TILDE a |
    T_EOF a
    deriving (Eq, Show)

getAttr :: Token a -> a
getAttr (T_AUTO x) = x
getAttr (T_BREAK x) = x
getAttr (T_CASE x) = x
getAttr (T_CHAR x) = x
getAttr (T_CONST x) = x
getAttr (T_CONTINUE x) = x
getAttr (T_DEFAULT x) = x
getAttr (T_DO x) = x
getAttr (T_DOUBLE x) = x
getAttr (T_ELSE x) = x
getAttr (T_ENUM x) = x
getAttr (T_EXTERN x) = x
getAttr (T_FLOAT x) = x
getAttr (T_FOR x) = x
getAttr (T_GOTO x) = x
getAttr (T_IF x) = x
getAttr (T_INLINE x) = x
getAttr (T_INT x) = x
getAttr (T_LONG x) = x
getAttr (T_REGISTER x) = x
getAttr (T_RESTRICT x) = x
getAttr (T_RETURN x) = x
getAttr (T_SHORT x) = x
getAttr (T_SIGNED x) = x
getAttr (T_SIZEOF x) = x
getAttr (T_STATIC x) = x
getAttr (T_STRUCT x) = x
getAttr (T_SWITCH x) = x
getAttr (T_TYPEDEF x) = x
getAttr (T_UNION x) = x
getAttr (T_UNSIGNED x) = x
getAttr (T_VOID x) = x
getAttr (T_VOLATILE x) = x
getAttr (T_WHILE x) = x
getAttr (T_ALIGNAS x) = x
getAttr (T_ALIGNOF x) = x
getAttr (T_ATOMIC x) = x
getAttr (T_BOOL x) = x
getAttr (T_COMPLEX x) = x
getAttr (T_GENERIC x) = x
getAttr (T_IMAGINARY x) = x
getAttr (T_NORETURN x) = x
getAttr (T_STATIC_ASSERT x) = x
getAttr (T_THREAD_LOCAL x) = x
getAttr (T_IDENTIFIER _ x) = x
getAttr (T_INTEGER _ _ x) = x
getAttr (T_STRING _ x) = x
getAttr (T_ARROW x) = x
getAttr (T_ASSIGN x) = x
getAttr (T_BIT_AND x) = x
getAttr (T_BIT_AND_ASSIGN x) = x
getAttr (T_BIT_OR x) = x
getAttr (T_BIT_OR_ASSIGN x) = x
getAttr (T_CLOSE_CURLY x) = x
getAttr (T_CLOSE_PAREN x) = x
getAttr (T_CLOSE_SQUARE x) = x
getAttr (T_COLON x) = x
getAttr (T_COMMA x) = x
getAttr (T_DIV x) = x
getAttr (T_DIV_ASSIGN x) = x
getAttr (T_DOT x) = x
getAttr (T_ELIPSIS x) = x
getAttr (T_EQ x) = x
getAttr (T_GT x) = x
getAttr (T_GTE x) = x
getAttr (T_HASH x) = x
getAttr (T_HASH_HASH x) = x
getAttr (T_HAT x) = x
getAttr (T_HAT_ASSIGN x) = x
getAttr (T_INC x) = x
getAttr (T_DEC x) = x
getAttr (T_LOGICAL_AND x) = x
getAttr (T_LOGICAL_OR x) = x
getAttr (T_LSHIFT x) = x
getAttr (T_LSHIFT_ASSIGN x) = x
getAttr (T_LT x) = x
getAttr (T_LTE x) = x
getAttr (T_MINUS x) = x
getAttr (T_MINUS_ASSIGN x) = x
getAttr (T_MOD_ASSIGN x) = x
getAttr (T_MOD x) = x
getAttr (T_NEQ x) = x
getAttr (T_NOT x) = x
getAttr (T_OPEN_CURLY x) = x
getAttr (T_OPEN_PAREN x) = x
getAttr (T_OPEN_SQUARE x) = x
getAttr (T_PERCENT x) = x
getAttr (T_PLUS x) = x
getAttr (T_PLUS_ASSIGN x) = x
getAttr (T_QUESTION x) = x
getAttr (T_RSHIFT x) = x
getAttr (T_RSHIFT_ASSIGN x) = x
getAttr (T_SEMI x) = x
getAttr (T_STAR x) = x
getAttr (T_STAR_ASSIGN x) = x
getAttr (T_TILDE x) = x
getAttr (T_EOF x) = x
