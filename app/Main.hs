module Main where

import C.Lexer
import C.Parser
import C.Translate

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

main :: IO ()
main = do
    input <- getContents
    let ast = parse input
    case ast of
        Left e -> error e
        Right ast' -> case xTranslationUnit ast' of
            Left e -> error e
            Right hir -> putDocW 80 $ pretty hir
    where
        parse s = runAlex s translation_unit

