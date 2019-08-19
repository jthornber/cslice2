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
        format w doc = layoutPretty (layoutOptions w) (unAnnotate doc)
        layoutOptions w = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}

printSame :: String -> Expectation
printSame txt = repl txt `shouldBe` txt

printsAs :: String -> String -> Expectation
printsAs txt expected = repl txt `shouldBe` expected

inFn :: String -> String
inFn body = "void fn() {\n        " ++ body ++ "\n}"

spec :: Spec
spec = parallel $ do
    describe "Declaration.BasicTypes" $ do
        -- I know this isn't legal
        it "can declare void" $ printSame "void x;"

        it "can declare char" $ printSame "char x;"
        it "can declare signed char" $ "signed char x;" `printsAs` "char x;"
        it "can declare unsigned char" $ printSame "unsigned char x;"

        it "can declare short" $ printSame "short x;"
        it "can declare signed short" $ "signed short x;" `printsAs` "short x;"
        it "can declare unsigned short" $ printSame "unsigned short x;"

        it "can declare int" $ printSame "int x;"
        it "can declare signed int" $ "signed int x;" `printsAs` "int x;"
        it "can declare unsigned int" $ printSame "unsigned int x;"

        it "can declare long" $ printSame "long x;"
        it "can declare signed long" $ "signed long x;" `printsAs` "long x;"
        it "can declare unsigned long" $ printSame "unsigned long x;"

        it "can declare long long" $ printSame "long long x;"
        it "can declare signed long long" $ "signed long long x;" `printsAs` "long long x;"
        it "can declare unsigned long long" $ printSame "unsigned long long x;"

        it "can declare __int128" $ printSame "__int128 x;"
        it "can declare signed __int128" $ "signed __int128 x;" `printsAs` "__int128 x;"
        it "can declare unsigned __int128" $ printSame "unsigned __int128 x;"

        it "can declare float" $ printSame "float x;"
        it "can declare double" $ printSame "double x;"

    describe "Declaration.Struct" $ do
        it "can declare a struct var" $
            printSame "struct foo x;"

        it "can declare an empty struct" $
            printSame "struct {} x;"

        it "can declare an empty named struct" $
            printSame "struct foo {} x;"

        it "can declare an anonymous struct with fields" $
            printSame "struct {\n        int x;\n        int y;\n} foo;"

        it "can declare a named struct with fields" $
            printSame "struct foo {\n        int x;\n} bar;"

    describe "Declaration.Enum" $ do
        it "can declare an enum var" $ printSame "enum foo x;"

        it "can declare a trivial enum" $
            printSame "enum {\n        FOO\n} x;"

        it "can declare with many entries" $
            printSame "enum {\n        ONE,\n        TWO,\n        THREE\n} x;"

        it "can handle an extra comma" $
            "enum {ONE,TWO,THREE,} x;" `printsAs` "enum {\n        ONE,\n        TWO,\n        THREE\n} x;"

        it "can specify values" $
            printSame "enum foo {\n        ONE = 1,\n        TWO,\n        THREE = 3\n} x;"

    describe "Declaration.StorageClass" $ do
        it "can declare static" $ printSame "static int x;"
        it "can declare extern" $ printSame "extern int x;"

    describe "Declaration.CVR" $ do
        it "can declare const" $ printSame "const int x;"
        it "can declare volatile" $ printSame "volatile int x;"
        it "can declare restrict" $ printSame "restrict int x;"
        it "can declare const + volatile" $ printSame "const volatile int x;"
        it "can declare volatile + const" $
            "volatile const int x;" `printsAs` "const volatile int x;"
        it "can declare restrict + const" $
            "restrict const int x;" `printsAs` "const restrict int x;"
        it "can declare const + restrict" $ printSame "const restrict int x;"
        it "can declare const + volatile + restrict" $
            printSame "const volatile restrict int x;"

    describe "Declaration.Array" $ do
        it "can declare a sized array" $ printSame "int x[1];"
        it "can declare a zero sized array" $ printSame "int x[0];"
        it "can declare an unsized array" $ printSame "int x[];"
        it "can declare cvr arrays" $ printSame "const volatile int x[12];"

    describe "Declaration.Pointer" $ do
        it "can declare simple pointers" $ printSame "int *x;"
        it "can declare multiple pointers" $
            "int x, *y, **z, ***w;" `printsAs` "int x;\nint *y;\nint **z;\nint ***w;"

    describe "Declaration.Function" $ do
        it "can declare a simple function" $ printSame "int square(int x);"

    describe "Declaration.Typedef" $ do
        it "can declare a simple typedef" $ printSame "typedef int foo;"
        it "can declare an array typedef" $ printSame "typedef int foo[4];"
        it "can declare a function typedef" $ printSame "typedef int (*error_fn)(int x);"
        it "can declare a struct typedef" $
            printSame "typedef struct {\n        int x;\n        int y;\n} coord;"

    describe "Declaration.MultipleDeclarators" $ do
        it "can declare multiple vars" $
            "int x, y;" `printsAs` "int x;\nint y;"

    describe "Definition.Function" $ do
        it "can define a simple function" $
            printSame "int square(int x) {\n        return (x) * (x);\n}"

    describe "Expression.Assignment" $ do
        it "can show a simple assignment" $ printSame $ inFn "x = 4;"

