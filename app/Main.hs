module Main where

import C.Lexer
import C.Parser
import C.Translate

import Control.Monad
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

printAst = True

main :: IO ()
main = do
    input <- getContents
    let ast = input `seq` parse input
    case ast of
        Left e -> error e
        Right ast' -> do
            print ast'
            putStrLn "\n"

            case xTranslationUnit ast' of
                Left e -> error e
                Right hir -> do
                    print hir
                    putStrLn "\n"
                    putDocW 80 $ pretty hir
                    putStrLn ""
    where
        parse s = runAlex s translation_unit

