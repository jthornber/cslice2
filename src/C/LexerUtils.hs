{-# LANGUAGE OverloadedStrings #-}
module C.LexerUtils (
    utf8Encode,
    Byte,
    withS,
    keyword,
    punc,
    charPos,
    charPosMany,
    intToken,
    readOct,
    readDec,
    readHex,
    alexEOF,
    AlexUserState(..),
    getTypedefs,
    modifyTypedefs,
    insertTypedef,
    isTypedef,
    getStructs,
    modifyStructs,
    isStruct,
    insertStruct,
    builtinTypedefs,
    alexInitUserState,
    Input(..),
    AlexInput,
    alexInputPrevChar,
    alexGetByte,
    SourcePos(..),
    alexStartPos,
    alexMove,
    AlexState(..),
    runAlex,
    Alex(..),
    alexGetInput,
    alexSetInput,
    alexError,
    alexGetUserState,
    alexSetUserState,
    getCurrentPos,
    ParseError
    ) where

import C.Token
import C.Int
import C.SourcePos

import Debug.Trace
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Ascii

import Control.Applicative as App (Applicative (..))
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits


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

withS :: (Text -> SourcePos -> Token SourcePos) ->
         Input -> Int -> Alex (Token SourcePos)
withS fn inp len = pure $ fn (Text.take len (inputText inp)) (inputPos inp)

keyword :: (SourcePos -> Token SourcePos) ->
           Input -> Int -> Alex (Token SourcePos)
keyword fn inp _len = pure . fn . inputPos $ inp

punc :: (SourcePos -> Token SourcePos) -> Input -> Int -> Alex (Token SourcePos)
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

intToken :: (String -> Integer) -> Text -> SourcePos -> Token SourcePos
intToken fn txt pos = T_INTEGER (fn ns) (sign suffix) (intType suffix) pos
    where
        s = Text.unpack txt
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

alexEOF :: Alex (Token SourcePos)
alexEOF = return . T_EOF $ SourcePos 0 0 "<eof>"

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

data Input = Input
  { inputPos      :: {-# UNPACK #-} !SourcePos
    -- ^ Current input position.

  , inputText     :: {-# UNPACK #-} !Text
    -- ^ The text that needs to be lexed.

  , inputPrev     :: {-# UNPACK #-} !SourcePos
    -- ^ Location of the last consumed character.

  , inputPrevChar :: {-# UNPACK #-} !Char
    -- ^ The last consumed character.
  }

type AlexInput = Input

-- | Prepare the text for lexing.
initialInput :: Text {- ^ Where the text came from -} ->
                Text {- ^ The text to lex -} -> Input
initialInput file str = Input
  { inputPos      = startPos file
  , inputPrev     = beforeStartPos file
  , inputPrevChar = '\n'    -- end of the virtual previous line
  , inputText     = str
  }

startPos :: Text {- ^ Name of file/thing containing this -} -> SourcePos
startPos file = SourcePos { sourceLine    = 1
                          , sourceColumn  = 1
                          , sourceFile    = file
                          }

beforeStartPos :: Text -> SourcePos
beforeStartPos file = SourcePos { sourceLine    = 0
                                , sourceColumn  = 0
                                , sourceFile    = file
                                }

{- | Move one position back.  Assumes that newlines use a single bytes.

This function is intended to be used when starting the lexing somewhere
in the middle of the input, for example, if we are implementing a quasi
quoter, and the previous part of the input is not in our language.
-}
{-
prevPos :: SourcePos -> SourcePos
prevPos p
  | sourceColumn p > 1 = p { sourceColumn = sourceColumn p - 1
                           }

  | sourceLine p > 1   = p { sourceLine   = sourceLine p - 1
                           , sourceColumn = 1
                           }

  | otherwise          = beforeStartPos (sourceFile p)
-}

-- | The file/thing for the current position.
--
 {-
inputFile :: Input -> Text
inputFile = sourceFile . inputPos
-}

-- | Update a 'SourcePos' for a particular matched character
moveSourcePos :: Char -> SourcePos -> SourcePos
moveSourcePos c p = SourcePos { sourceLine   = newLine
                              , sourceColumn = newColumn
                              , sourceFile   = sourceFile p
                              }
  where
  line   = sourceLine p
  column = sourceColumn p

  (newLine,newColumn) = case c of
                          '\t' -> (line, ((column + 7) `div` 8) * 8 + 1)
                          '\n' -> (line + 1, 1)
                          _    -> (line, column + 1)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = inputPrevChar

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (Input { inputPos = p, inputText = text }) =
  do (c,text') <- Text.uncons text
     let p'  = moveSourcePos c p
         x   = ascii c
         inp = Input { inputPrev     = p
                     , inputPrevChar = c
                     , inputPos      = p'
                     , inputText     = text'
                     }
     x `seq` inp `seq` return (x, inp)

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

alexStartPos :: Text -> SourcePos
alexStartPos file = SourcePos 1 1 file

tab_size :: Int
tab_size = 8

alexMove :: SourcePos -> Char -> SourcePos
alexMove p '\t' = p { sourceColumn = (sourceColumn p) + tab_size }
alexMove p '\n' = p { sourceLine = (sourceLine p + 1) }
alexMove p _ = p { sourceColumn = (sourceColumn p) + 1 }

-- -----------------------------------------------------------------------------
--
data AlexState = AlexState {
        alex_inp :: AlexInput,     -- the current input
        alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

data ParseError = ParseError {
    errorPos :: SourcePos,
    errorMessage :: Text
} deriving (Eq, Show)

runAlex :: Text -> Text -> Alex a -> Either ParseError a
runAlex source input (Alex f) =
    case f (AlexState {alex_inp = initialInput source input,
                       alex_ust = alexInitUserState
                      }) of
        Left err -> Left err
        Right (_, a) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either ParseError (AlexState, a) }

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
alexGetInput = Alex $ \s@(AlexState inp _) ->
    Right (s, inp)

alexSetInput :: AlexInput -> Alex ()
alexSetInput inp = Alex $ \s -> Right (s {alex_inp = inp}, ())

alexError :: Text -> Alex a
alexError message = do
    pos <- getCurrentPos
    Alex . const . Left $ ParseError pos message

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())

getCurrentPos :: Alex SourcePos
getCurrentPos = Alex $ \s -> Right (s, inputPos . alex_inp $ s)

