{-# LANGUAGE OverloadedStrings #-}
module Main where
        
import C.ErrorType
import C.HIR
import C.Identifier
import C.Lexer
import C.Parser
import C.PrettyPrint
import C.Slice
import C.SymbolTable
import C.Translate

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace
import Options.Applicative hiding (ParseError)
import Text.Pretty.Simple (pPrint)

import System.IO (hClose)
import System.Process.Typed
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.STM (atomically)

normCode :: Text -> IO Text
normCode code = withProcess_ indent $ \p -> do
    T.hPutStr (getStdin p) code
    hClose (getStdin p)
    decodeUtf8 . L.toStrict <$> atomically (getStdout p)
    where
        indent = setStdin createPipe
               $ setStdout byteStringOutput
               $ proc "indent" ["-linux"]

------------------------------------------------------

data Flags = Flags {
    showAST :: Bool,
    showPreHIR :: Bool,
    showPostHIR :: Bool
}

flags :: Parser Flags
flags = Flags <$> switch (long "ast" <> help "Show the abstract syntax tree")
              <*> switch (long "hir-pre-slice" <> help "Show high level intermediate representation, before slice")
              <*> switch (long "hir-post-slice" <> help "Show high level intermediate representation, after slice")

options :: ParserInfo Flags
options = info (flags <**> helper)
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
    pure $ refs fun

sliceFun :: Identifier -> TranslationUnit -> Either SliceError TranslationUnit
{-
-- FIXME: remove explicit cases
sliceFun nm@(Identifier nm') tu = do
    case funRefs nm tu of
        Nothing -> Left $ SliceError Nothing $ T.concat [
            "Couldn't find function definition '", nm', "'"] 
        (Just rs) -> case slice rs tu of
            Nothing -> Left $ SliceError Nothing "slice failed"
            (Just tu) -> Right tu
            -}
sliceFun _nm = Right

prefixErr :: Text -> Text
prefixErr = T.append "error: "

printErr :: SliceError -> IO ()
printErr (SliceError Nothing msg) = T.putStrLn $ prefixErr msg
printErr (SliceError (Just pos) msg) =
    T.putStrLn $ prefixErr $ T.concat [
        T.pack $ show pos,
        ": ",
        msg]

-- FIXME: remove explicit cases
runSlice :: Flags -> Text -> Identifier -> IO ()
runSlice flags input nm = do
    case parse input of
        (Left err) -> printErr err
        (Right ast) -> do
            when' (showAST flags) $ do
                pPrint ast
                T.putStrLn "\n"

            case toHir ast of
                (Left err) -> printErr err
                (Right hir) -> do
                    when' (showPreHIR flags) $ do
                        pPrint hir
                        T.putStrLn "\n"

                    case sliceFun nm hir of
                        (Left err) -> printErr err
                        (Right hir') -> do
                            when' (showPostHIR flags) $ do
                                pPrint hir'
                                T.putStrLn "\n"

                            putDocW 80 $ ppTranslationUnit hir'
                            T.putStrLn ""
                            pure ()
    where
        parse s = runAlex "<stdin>" s translation_unit

main :: IO ()
main = do
    flags <- execParser options
    input <- T.getContents
    input `seq` runSlice flags input (Identifier "inc_x")

