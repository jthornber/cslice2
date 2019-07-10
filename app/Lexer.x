{
module Lexer (
    Alex(..),
    alexError,
    runAlex,
    lexStep
    ) where

import Token

import Debug.Trace
import Data.Char
import Data.List
import Data.Maybe
}

%wrapper "monad"

$digit = 0-9
$hexDigit = [0-9a-fA-F]
$alpha = [a-zA-Z]

@intSuffix = [uU][lL]?|[uU](ll|LL)?|[lL][uU]?|ll[uU]?|LL[uU]?|u?
@s_chars = [^"]*

tokens :-

$white+			;

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
float		{keyword T_FLOAT}
for		{keyword T_FOR}
goto		{keyword T_GOTO}
if		{keyword T_IF}
inline		{keyword T_INLINE}
int		{keyword T_INT}
long		{keyword T_LONG}
register	{keyword T_REGISTER}
restrict	{keyword T_RESTRICT}
return		{keyword T_RETURN}
short		{keyword T_SHORT}
signed		{keyword T_SIGNED}
sizeof		{keyword T_SIZEOF}
static		{keyword T_STATIC}
struct		{keyword T_STRUCT}
switch		{keyword T_SWITCH}
typedef		{keyword T_TYPEDEF}
union		{keyword T_UNION}
unsigned	{keyword T_UNSIGNED}
void		{keyword T_VOID}
volatile	{keyword T_VOLATILE}
while		{keyword T_WHILE}
_Alignas	{keyword T_ALIGNAS}
_Alignof	{keyword T_ALIGNOF}
_Atomic		{keyword T_ATOMIC}
_Bool		{keyword T_BOOL}
_Complex	{keyword T_COMPLEX}
_Generic	{keyword T_GENERIC}
_Imaginary	{keyword T_IMAGINARY}
_Noreturn	{keyword T_NORETURN}
_Static_assert  {keyword T_STATIC_ASSERT}
_Thread_local   {keyword T_THREAD_LOCAL}

$alpha [$alpha $digit \_]*	{withS (return . T_IDENTIFIER)}
[1-9]$digit*@intSuffix		{withS (return . intToken readDec)}
0[xX]$hexDigit+@intSuffix	{withS (return . intToken readHex . drop 2)}
0[1-9]*@intSuffix		{withS (return . intToken readOct . drop 1)}
\"@s_chars\"			{withS (return . T_STRING)}


{
lexStep :: (Token -> Alex a) -> Alex a
lexStep k = do
    token <- alexMonadScan
    k token

withS fn (_, _, _, s) len = fn (take len s)

keyword v _ _ = return v
punc = keyword

charPos ::Char -> Char -> Char -> Maybe Integer
charPos b e c =
    if ord c >= ord b && ord c <= ord e
        then Just . fromIntegral $ ord c - ord b
        else Nothing

charPosMany :: [(Char, Char, Integer)] -> Char -> Maybe Integer
charPosMany [] c = Nothing 
charPosMany ((b, e, offset):rs) c = case charPos b e c of
    Just n -> Just $ n + offset
    Nothing -> charPosMany rs c

intToken :: (String -> Integer) -> String -> Token
intToken fn s = T_INTEGER (fn ns) suffix
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

alexEOF = return T_EOF
}
