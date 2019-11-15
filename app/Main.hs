{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified C.AST as AST
import C.ErrorType
import C.HIR
import C.Identifier
import C.Lexer
import C.Parser
import C.PreProcessor
import C.PrettyPrint
import qualified C.Slice as Slice
import C.SymbolTable
import C.Translate

import Data.Maybe
import Data.Set (Set)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc hiding (indent)
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace
import Options.Applicative hiding (ParseError)
import Text.Pretty.Simple (pPrint)

import System.IO (hClose)
import System.Process.Typed
import qualified Data.ByteString.Lazy as L
import Control.Concurrent.STM (atomically)

------------------------------------------------------

-- Runs C code through the GNU indent tool
indentCode :: Text -> IO Text
indentCode code = withProcess_ indent $ \p -> do
    T.hPutStr (getStdin p) code
    hClose (getStdin p)
    decodeUtf8 . L.toStrict <$> atomically (getStdout p)
    where
        indent = setStdin createPipe
               $ setStdout byteStringOutput
               $ proc "indent" ["-linux"]

------------------------------------------------------

data Flags = Flags {
    flagShowAST :: Bool,
    flagShowPreHIR :: Bool,
    flagShowPostHIR :: Bool,
    flagIncludes :: [String],
    flagDefines :: [String],
    flagInputFile :: String
} deriving (Show)

flags' :: Parser Flags
flags' = Flags <$> switch (long "ast" <>
                           help "Show the abstract syntax tree")
               <*> switch (long "hir-pre-slice" <>
                           help "Show high level intermediate representation, before slice")
               <*> switch (long "hir-post-slice" <>
                           help "Show high level intermediate representation, after slice")
               <*> many (strOption (short 'I' <>
                                    metavar "INCLUDE" <>
                                    help "Specify include directory for the preprocessor"))
               <*> many (strOption (short 'D' <>
                                    metavar "DEFINE" <>
                                    help "Preprocessor definition"))
               <*> argument str (metavar "FILE")

options :: ParserInfo Flags
options = info (flags' <**> helper)
            (fullDesc <> progDesc "Slice and dice C programs in exciting ways."
                      <> header "cslice - Unit test legacy C code")

when' :: Applicative f => Bool -> f () -> f ()
when' b m = if b then m else pure ()

findFunDef :: Identifier -> TranslationUnit -> Maybe ExternalDeclaration
findFunDef nm (TranslationUnit edecls) = listToMaybe $ filter isFun edecls
    where
        isFun (FunDef _ (Symbol _ nm') _) | nm == nm' = True
        isFun _ = False

funRefs :: Identifier -> TranslationUnit -> Maybe (Set Symbol)
funRefs nm tu = do
    fun <- trace "1" $ findFunDef nm tu
    pure $ Slice.refs fun

sliceFun :: Identifier -> TranslationUnit -> Either SliceError TranslationUnit
{-
-- FIXME: remove explicit cases
sliceFun nm@(Identifier nm') tu = do
    case funRefs nm tu of
        Nothing -> Left $ SliceError Nothing $ T.concat [
            "Couldn't find function definition '", nm', "'"] 
        (Just rs) -> case Slice.slice rs tu of
            Nothing -> Left $ SliceError Nothing "slice failed"
            (Just tu) -> Right tu
            -}
sliceFun _nm = Right

prefixErr :: Text -> Text
prefixErr = T.append "error: "

-- FIXME: slow
showPos :: SourcePos -> Text -> IO ()
showPos pos input = do
    T.putStr "    "
    T.putStrLn $ (T.lines input) !! ((sourceLine pos) - 1)

printErr :: SliceError -> Text -> IO ()
printErr (SliceError Nothing msg) _ = T.putStrLn $ prefixErr msg
printErr (SliceError (Just pos) msg) input = do
    T.putStrLn . prefixErr $ T.concat [ T.pack $ show pos, ": ", msg]
    showPos pos input

slice :: Text -> Identifier -> Either SliceError (AST.TranslationUnit, TranslationUnit, TranslationUnit)
slice input nm = do
    ast <- runAlex "<stdin>" input translation_unit
    hirPre <- toHir ast
    hirPost <- sliceFun nm hirPre
    pure (ast, hirPre, hirPost)

printIt :: (Show a) => Bool -> a -> IO ()
printIt b v = when' b $ pPrint v >> T.putStrLn "\n"

main :: IO ()
main = do
    flags <- execParser options

    let cppOpts = CppOptions {
        cppInputFile = T.pack $ flagInputFile flags,
        cppIncludes = map T.pack $ flagIncludes flags,
        cppDefines = map T.pack $ flagDefines flags
    }
    
    input <- cpp cppOpts
    
    case slice input (Identifier "inc_x") of
        (Left err) -> printErr err input
        (Right (ast, hirPre, hirPost)) -> do
            
            printIt (flagShowAST flags) ast
            printIt (flagShowPreHIR flags) hirPre
            printIt (flagShowPostHIR flags) hirPost

            code <- indentCode . renderStrict . layoutCompact . ppTranslationUnit $ hirPost
            T.putStr code


