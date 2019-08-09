module C.HIRSpec (
    spec
    ) where

import C.Lexer
import C.Parser
import C.Translate

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import Data.Text.Prettyprint.Doc.Render.String

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

format w doc = layoutPretty layoutOptions (unAnnotate doc)
    where
        layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}

repl :: String -> String
repl input = do
    let ast = parse input
    case ast of
        Left e -> error e
        Right ast' -> case xTranslationUnit ast' of
            Left e -> error e
            Right hir -> renderString . format 80 . pretty $ hir
    where
        parse s = runAlex s translation_unit

idempotent txt = repl txt `shouldBe` txt

printsAs txt expected = repl txt `shouldBe` expected

spec :: Spec
spec = parallel $ do
    describe "Declaration.BasicTypes" $ do
        -- I know this isn't legal
        it "can declare void" $ idempotent "void x;"

        it "can declare char" $ idempotent "char x;"
        it "can declare signed char" $ "signed char x;" `printsAs` "char x;"
        it "can declare unsigned char" $ idempotent "unsigned char x;"

        it "can declare short" $ idempotent "short x;"
        it "can declare signed short" $ "signed short x;" `printsAs` "short x;"
        it "can declare unsigned short" $ idempotent "unsigned short x;"

        it "can declare int" $ idempotent "int x;"
        it "can declare signed int" $ "signed int x;" `printsAs` "int x;"
        it "can declare unsigned int" $ idempotent "unsigned int x;"

        it "can declare long" $ idempotent "long x;"
        it "can declare signed long" $ "signed long x;" `printsAs` "long x;"
        it "can declare unsigned long" $ idempotent "unsigned long x;"

        it "can declare long long" $ idempotent "long long x;"
        it "can declare signed long long" $ "signed long long x;" `printsAs` "long long x;"
        it "can declare unsigned long long" $ idempotent "unsigned long long x;"

        it "can declare __int128" $ idempotent "__int128 x;"
        it "can declare signed __int128" $ "signed __int128 x;" `printsAs` "__int128 x;"
        it "can declare unsigned __int128" $ idempotent "unsigned __int128 x;"

        it "can declare float" $ idempotent "float x;"
        it "can declare double" $ idempotent "double x;"

    describe "Declaration.StorageClass" $ do
        it "can declare static" $ idempotent "static int x;"
        it "can declare extern" $ idempotent "extern int x;"

    describe "Declaration.CVR" $ do
        it "can declare const" $ idempotent "const int x;"
        it "can declare volatile" $ idempotent "volatile int x;"
        it "can declare restrict" $ idempotent "restrict int x;"
        it "can declare const + volatile" $ idempotent "const volatile int x;"
        it "can declare volatile + const" $
            "volatile const int x;" `printsAs` "const volatile int x;"
        it "can declare restrict + const" $
            "restrict const int x;" `printsAs` "const restrict int x;"
        it "can declare const + restrict" $ idempotent "const restrict int x;"
        it "can declare const + volatile + restrict" $
            idempotent "const volatile restrict int x;"

    describe "Declaration.Array" $ do
        it "can declare a sized array" $ idempotent "int x[1];"

