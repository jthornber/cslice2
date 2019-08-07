{
module C.Lexer (
    Alex(..),
    AlexPosn(..),
    AlexUserState(..),
    alexError,
    runAlex,
    lexStep,
    insertTypedef,
    insertStruct
    ) where

import C.Token

import Debug.Trace
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
}

%wrapper "monadUserState"

$digit = [0-9]
$hexDigit = [0-9a-fA-F]
$octDigit = [0-7]
$alpha = [a-zA-Z]
$eol = \n
$identletter = [a-zA-Z_\$]
$instr    = . # [ \\ \" \n \r ]
$inchar   = . # [ \\ \' \n \r ]

@charesc  = \\([ntvbrfaeE\\\?\'\"]|$octDigit{1,3}|x$hexDigit+)
@intSuffix = [uU][lL]?|[uU](ll|LL)?|[lL][uU]?|ll[uU]?|LL[uU]?|u?
@s_chars = [^"]*
@int = $digit+

tokens :-

$white+			;

\#$white*@int$white*(\"[^\"]+\"$white*)?(@int$white*)*\r?$eol	;

"["		{punc T_OPEN_SQUARE}
"]"		{punc T_CLOSE_SQUARE}
"("		{punc T_OPEN_PAREN}
")"		{punc T_CLOSE_PAREN}
"{"		{punc T_OPEN_CURLY}
"}"		{punc T_CLOSE_CURLY}
"*="		{punc T_STAR_ASSIGN}
"/="		{punc T_DIV_ASSIGN}
"%="		{punc T_MOD_ASSIGN}
"+="		{punc T_PLUS_ASSIGN}
"-="		{punc T_MINUS_ASSIGN}
"<<="		{punc T_LSHIFT_ASSIGN}
">>="		{punc T_RSHIFT_ASSIGN}
"&="		{punc T_BIT_AND_ASSIGN}
"^="		{punc T_HAT_ASSIGN}
"|="		{punc T_BIT_OR_ASSIGN}
"."		{punc T_DOT}
"->"		{punc T_ARROW}
"++"		{punc T_INC}
"--"		{punc T_DEC}
"&&"		{punc T_LOGICAL_AND}
"&"		{punc T_BIT_AND}
"*"		{punc T_STAR}
"+"		{punc T_PLUS}
"-"		{punc T_MINUS}
"~"		{punc T_TILDE}
"!"		{punc T_NOT}
"/"		{punc T_DIV}
"%"		{punc T_MOD}
"<<"		{punc T_LSHIFT}
">>"		{punc T_RSHIFT}
"<="		{punc T_LTE}
">="		{punc T_GTE}
"<"		{punc T_LT}
">"		{punc T_GT}
"=="		{punc T_EQ}
"!="		{punc T_NEQ}
"^"		{punc T_HAT}
"||"		{punc T_LOGICAL_OR}
"|"		{punc T_BIT_OR}
"?"		{punc T_QUESTION}
":"		{punc T_COLON}
";"		{punc T_SEMI}
"..."		{punc T_ELIPSIS}
"="		{punc T_ASSIGN}
","		{punc T_COMMA}

__attribute__	{keyword T_ATTRIBUTE}
asm             {keyword T_ASM}
__asm__         {keyword T_ASM}
auto		{keyword T_AUTO}
break		{keyword T_BREAK}
case		{keyword T_CASE}
char		{keyword T_CHAR}
const		{keyword T_CONST}
continue	{keyword T_CONTINUE}
default		{keyword T_DEFAULT}
do		{keyword T_DO}
double		{keyword T_DOUBLE}
else		{keyword T_ELSE}
enum		{keyword T_ENUM}
extern		{keyword T_EXTERN}
__extension__	;
float		{keyword T_FLOAT}
for		{keyword T_FOR}
goto		{keyword T_GOTO}
if		{keyword T_IF}
inline		{keyword T_INLINE}
__inline__      {keyword T_INLINE}
int		{keyword T_INT}
__int128	{keyword T_INT128}
__label__	{keyword T_LABEL}
long		{keyword T_LONG}
register	{keyword T_REGISTER}
restrict	{keyword T_RESTRICT}
return		{keyword T_RETURN}
short		{keyword T_SHORT}
signed		{keyword T_SIGNED}
__signed__	{keyword T_SIGNED}
sizeof		{keyword T_SIZEOF}
static		{keyword T_STATIC}
struct		{keyword T_STRUCT}
switch		{keyword T_SWITCH}
typeof          {keyword T_TYPEOF}
__typeof__      {keyword T_TYPEOF}
typedef		{keyword T_TYPEDEF}
union		{keyword T_UNION}
unsigned	{keyword T_UNSIGNED}
void		{keyword T_VOID}
volatile	{keyword T_VOLATILE}
__volatile__    {keyword T_VOLATILE}
while		{keyword T_WHILE}
__alignas__	{keyword T_ALIGNAS}
_Alignas	{keyword T_ALIGNAS}
_Alignof	{keyword T_ALIGNOF}
__alignof__	{keyword T_ALIGNOF}
_Atomic		{keyword T_ATOMIC}
_Bool		{keyword T_BOOL}
_Complex	{keyword T_COMPLEX}
_Generic	{keyword T_GENERIC}
_Imaginary	{keyword T_IMAGINARY}
_Noreturn	{keyword T_NORETURN}
_Static_assert  {keyword T_STATIC_ASSERT}
_Thread_local   {keyword T_THREAD_LOCAL}

__builtin_va_arg		{keyword T_BUILTIN_VA_ARG}
__builtin_offsetof		{keyword T_BUILTIN_OFFSETOF}
__builtin_types_compatible_p	{keyword T_BUILTIN_TYPES_COMPATIBLE_P}
__builtin_convert_vector	{keyword T_BUILTIN_CONVERT_VECTOR}

$identletter($identletter|$digit)*	{withS (\s p -> return $ T_IDENTIFIER s p)}

[1-9]$digit*@intSuffix			{withS (\s p -> return $ intToken readDec s p)}
0[xX]$hexDigit+@intSuffix		{withS (\s p -> return $ intToken (readHex . drop 2) s p)}
0$octDigit*@intSuffix			{withS (\s p -> return $ intToken (readOct . drop 1) s p)}
\"($instr|@charesc)*\"			{withS (\s p -> return $ T_STRING s p)}
\'($inchar|@charesc)\'			{withS (\s p -> return $ T_CHAR_LIT s p)}

{
lexStep :: (Token AlexPosn -> Alex a) -> Alex a
lexStep k = do
    tok <- alexMonadScan
    case tok of
        (T_IDENTIFIER nm pos) -> do
            b <- isTypedef (Identifier nm)
            if b
                then k $ T_TYPEDEF_NAME nm pos
                else k $ T_IDENTIFIER nm pos
        t -> k t

withS fn (p, _, _, s) len = fn (take len s) p

keyword v (p, _, _, _) _ = return (v p)
punc = keyword

charPos ::Char -> Char -> Char -> Maybe Integer
charPos b e c =
    if ord c >= ord b && ord c <= ord e
        then Just . fromIntegral $ ord c - ord b
        else Nothing

charPosMany :: [(Char, Char, Integer)] -> Char -> Maybe Integer
charPosMany [] _ = Nothing 
charPosMany ((b, e, offset):rs) c = case charPos b e c of
    Just n -> Just $ n + offset
    Nothing -> charPosMany rs c

intToken :: (String -> Integer) -> String -> AlexPosn -> Token AlexPosn
intToken fn s p = T_INTEGER (fn ns) suffix p
    where
        (suffix', ns') = span suffixChar . reverse $ s
        suffix = reverse suffix'
        ns = reverse ns'
        suffixChar c = c == 'u' || c == 'U' || c == 'l' || c == 'L'

readOct, readDec, readHex :: String -> Integer
readOct = foldl' (\acc n -> acc * 8 + fromChar n) 0
    where
        fromChar = fromJust . charPos '0' '7'

readDec = foldl' (\acc n -> acc * 10 + fromChar n) 0
    where
        fromChar = fromJust . charPos '0' '9'

readHex = foldl' (\acc n -> acc * 16 + fromChar n) 0
    where
        fromChar = fromJust . charPosMany [('0', '9', 0),
                                           ('a', 'f', 9),
                                           ('A', 'F', 9)]

traceIt :: (Show a) => a -> a
traceIt x = trace (show x) x

alexEOF = return $ T_EOF (AlexPn 0 0 0)

data AlexUserState = AlexUserState {
    typedefs :: Set Identifier,
    structs :: Set Identifier
} deriving (Eq, Show)

getTypedefs :: Alex (Set Identifier)
getTypedefs = do
    us <- alexGetUserState
    return $ typedefs us

modifyTypedefs :: (Set Identifier -> Set Identifier) -> Alex ()
modifyTypedefs fn = do
    us <- alexGetUserState
    alexSetUserState $ us { typedefs = fn $ typedefs us }

insertTypedef :: Identifier -> Alex ()
insertTypedef = modifyTypedefs . S.insert

isTypedef :: Identifier -> Alex Bool
isTypedef nm = S.member nm <$> getTypedefs

getStructs :: Alex (Set Identifier)
getStructs = do
    us <- alexGetUserState
    return $ structs us

modifyStructs :: (Set Identifier -> Set Identifier) -> Alex ()
modifyStructs fn = do
    us <- alexGetUserState
    alexSetUserState $ us { structs = fn $ structs us }

isStruct :: Identifier -> Alex Bool
isStruct nm = S.member nm <$> getStructs

insertStruct :: Identifier -> Alex ()
insertStruct = modifyStructs . S.insert . traceIt

builtinTypedefs :: Set Identifier
builtinTypedefs = S.fromList . map Identifier $ types
    where
        types = [ "__builtin_va_list"
                , "_Bool"
                ]

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
        typedefs = builtinTypedefs,
        structs = S.empty
    }
}
