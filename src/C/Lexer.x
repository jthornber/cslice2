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
import C.Int

import Debug.Trace
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

import Control.Applicative as App (Applicative (..))
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits
}

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
-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8

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
intToken fn s p = T_INTEGER (fn ns) (sign suffix) (intType suffix) p
    where
        (suffix', ns') = span suffixChar . reverse $ s
        suffix = reverse suffix'
        ns = reverse ns'
        suffixChar c = c == 'u' || c == 'U' || c == 'l' || c == 'L'
        sign cs = 
            if elem 'u' cs || elem 'U' cs
            then UNSIGNED
            else SIGNED
        intType cs = case (length $ filter isLong cs) of
            0 -> INT
            1 -> LONG
            2 -> LONG_LONG
            _ -> error "how many longs?"
        isLong c = c == 'l' || c == 'L'

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


-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- -----------------------------------------------------------------------------
-- Default monad


data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode

      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program

    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
                        alex_bytes = [],

                        alex_ust = alexInitUserState,

                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
                  state__@(AlexState{}) -> Right (state__, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())


alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())


alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)

}
