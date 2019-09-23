module C.Identifier (
    Identifier(..)
    ) where

import Data.Text.Prettyprint.Doc

data Identifier =
    Identifier String
    deriving (Eq, Ord)

instance Show Identifier where
    show (Identifier nm) = nm

instance Pretty Identifier where
    pretty (Identifier nm) = pretty nm

