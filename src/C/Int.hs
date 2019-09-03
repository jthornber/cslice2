module C.Int (
    Sign(..),
    IntType(..)
    ) where

import Data.Text.Prettyprint.Doc

data Sign =
    SIGNED |
    UNSIGNED
    deriving (Eq, Show)

instance Pretty Sign where
    pretty SIGNED = emptyDoc -- signed is always optional
    pretty UNSIGNED = pretty "unsigned "

data IntType =
    CHAR |
    SHORT |
    INT |
    LONG |
    LONG_LONG |
    INT128
    deriving (Eq, Ord, Enum, Show)

instance Pretty IntType where
    pretty CHAR = pretty "char"
    pretty SHORT = pretty "short"
    pretty INT = pretty "int"
    pretty INT128 = pretty "__int128"
    pretty LONG = pretty "long"
    pretty LONG_LONG = pretty "long long"

