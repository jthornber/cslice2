module Identifier (
    Identifier(..)
    ) where

import Data.Text.Prettyprint.Doc

data Identifier =
    Identifier String
    deriving (Eq, Show, Ord)

instance Pretty Identifier where
    pretty (Identifier nm) = pretty nm

