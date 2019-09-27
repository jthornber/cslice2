module C.Identifier (
    Identifier(..)
    ) where

import Data.Text
import Data.Text.Prettyprint.Doc

data Identifier =
    Identifier Text
    deriving (Eq, Ord)

instance Show Identifier where
    show (Identifier nm) = show nm

instance Pretty Identifier where
    pretty (Identifier nm) = pretty nm

