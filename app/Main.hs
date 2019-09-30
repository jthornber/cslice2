{-# LANGUAGE OverloadedStrings #-}
module Main where
        
import C.HIR
import C.Lexer
import C.Parser
import C.PrettyPrint
import C.Prune
import C.Translate
import C.Identifier
import C.SymbolTable

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace
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

main :: IO ()
main = do
    input <- T.getContents
    let ast = input `seq` parse input
    case ast of
        Left e -> error e
        Right ast' -> do
            -- pPrint ast'
            -- T.putStrLn "\n"
            case toHir ast' of
                Left e -> error . T.unpack $ e
                Right hir -> do
                    -- pPrint hir
                    -- T.putStrLn "\n"
                    putDocW 80 $ ppTranslationUnit hir
                    T.putStrLn ""
                    print $ refs hir
    where
        parse s = runAlex "<stdin>" s translation_unit

{-
findFunDef :: Identifier -> TranslationUnit -> Maybe ExternalDeclaration
findFunDef nm (TranslationUnit edecls) = listToMaybe $ filter isFun edecls
    where
        isFun (FunDef _ (Symbol _ nm') _) | nm == nm' = True
        isFun _ = False

funRefs :: Identifier -> TranslationUnit -> Maybe (Set Symbol)
funRefs nm tu = do
    fun <- trace "1" $ findFunDef nm tu
    pure $ refs fun

pruneFun :: Identifier -> TranslationUnit -> Maybe TranslationUnit
pruneFun nm tu = do
    rs <- trace "2" $ funRefs nm tu
    prune rs tu

main :: IO ()
main = do
    input <- getContents
    let ast = input `seq` parse input
    case ast of
        Left e -> error e
        Right ast' -> do
            case toHir ast' of
                Left e -> error e
                Right hir -> do
                    case trace (show hir) $ pruneFun (Identifier "inc_x") hir of
                        Nothing -> error "prune failed"
                        (Just hir') -> do
                            putDocW 80 $ ppTranslationUnit hir'
    where
        parse s = runAlex s translation_unit
-}


