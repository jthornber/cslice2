module Main where

import AST
import Lexer
import Parser

main :: IO ()
main = do
    toks <- getContents >>= pure . cTokens
    print toks
    print . expression $ toks
