module Main where

import AST
import Lexer
import Parser

parse s = runAlex s translation_unit

main :: IO ()
main = do
    input <- getContents
    print . parse $ input

