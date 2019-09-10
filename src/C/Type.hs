module C.Type (
    EnumEntry(..),
    StructEntry(..),
    StructType(..),
    ParamEntry(..),
    FunFlag(..),
    FunType(..),
    CVR(..),
    Type(..),
    RawType(..)
    ) where

import C.Identifier
import C.Int
import C.SymbolTable

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text.Prettyprint.Doc

withBraces :: Doc ann -> [Doc ann] -> Doc ann
withBraces sep [] = braces emptyDoc
withBraces sep xs = concatWith (<>) [lbrace, line, indent 8 entries, line, rbrace]
    where
        entries = concatWith join xs
        join x y = x <> sep <> line <> y

mPretty :: (Pretty a) => Maybe a -> Doc ann
mPretty Nothing = emptyDoc
mPretty (Just x) = pretty x

addName :: Doc ann -> Maybe Symbol -> Doc ann
addName d Nothing = d
addName d (Just nm) = d <> space <> pretty nm

-- Includes trailing space if non-empty result
printCVR :: Set CVR -> Doc ann
printCVR cvr = case S.toList cvr of
    [] -> emptyDoc
    xs -> (hsep . map pretty $ xs) <> space

printType :: Type -> Doc ann -> Doc ann
printType (Type rt cvr) nm = printCVR cvr <> printRawType rt nm

printRawType :: RawType -> Doc ann -> Doc ann
printRawType TyVoid nm = pretty "void" <+> nm
printRawType TyBool nm = pretty "__Bool" <+> nm

printRawType (TyInt sign ty) nm = pretty sign <> pretty ty <+> nm

printRawType TyFloat nm = pretty "float" <+> nm
printRawType TyDouble nm = pretty "double" <+> nm

printRawType TyLongDouble nm = pretty "long double" <+> nm

printRawType (TyEnum mnm' mentries) nm = header <> body <+> nm
    where
        header = pretty "enum" `addName` mnm'
        body = case mentries of
            (Just entries) -> space <> (withBraces comma $ map pretty entries)
            _ -> emptyDoc

printRawType (TyArray ty Nothing) nm =
    (printType ty nm) <> lbracket <> rbracket

printRawType (TyArray ty (Just size)) nm = hcat [
    printType ty nm,
    lbracket,
    pretty size,
    rbracket]

printRawType (TyStruct st mnm' mentries) nm = header <> body <+> nm
    where
        header = pretty "struct" `addName` mnm'
        body = case mentries of
            Nothing -> emptyDoc
            (Just []) -> space <> braces emptyDoc
            (Just entries) -> space <>
                              lbrace <>
                              line <>
                              indent 8 (vsep $ map ((<> semi) . pretty) $ entries) <>
                              line <>
                              rbrace

printRawType (TyPointer ty) nm = printType ty (pretty '*' <> nm)
printRawType (TyFunction ft) nm = prettyFun ft nm

printRawType (TyAlias alias) nm = pretty alias <> nm

printRawType (TyTypeofExp e) nm = pretty "typeof(" <> pretty e <> pretty ")" <> nm
printRawType (TyTypeofType t) nm = pretty "typeof(" <> printType t emptyDoc <> pretty ")" <> nm

data EnumEntry = EnumEntry Symbol (Maybe Integer)
    deriving (Eq, Show)

instance Pretty EnumEntry where
    pretty (EnumEntry nm (Just n)) = (pretty nm) <+> (pretty "=") <+> (pretty n)
    pretty (EnumEntry nm Nothing) = pretty nm

data StructEntry =
    StructEntry Type (Maybe Symbol) (Maybe Integer) |
    StructEntryWidthOnly Integer
    deriving (Eq, Show)

instance Pretty StructEntry where
    pretty (StructEntry t mnm Nothing) = printType t $ mPretty mnm
    pretty (StructEntry t mnm (Just n)) =
        (printType t $ mPretty mnm) <+> (pretty ":") <+> (pretty n)
    pretty (StructEntryWidthOnly e) = pretty ":" <+> pretty e

data ParamEntry =
    ParamEntry Type (Maybe Symbol)
    deriving (Eq, Show)

instance Pretty ParamEntry where
    pretty (ParamEntry t Nothing) = printType t emptyDoc
    pretty (ParamEntry t (Just nm)) = printType t (pretty nm)

data FunFlag =
    INLINE |
    NORETURN |
    VARARG
    deriving (Eq, Ord, Show)

instance Pretty FunFlag where
    pretty INLINE = pretty "inline"
    pretty NORETURN = pretty "__Noreturn"
    pretty VARARG = pretty "..."

data FunType = FunType Type [ParamEntry] (Set FunFlag)
    deriving (Eq, Show)

prettyFun :: FunType -> Doc ann -> Doc ann
prettyFun (FunType t params flags) nm = inline <> noreturn <> (printType t emptyDoc) <> nm <> params'
   where
    flag f = if S.member f flags
             then pretty f <> space
             else emptyDoc

    inline = flag INLINE
    noreturn = flag NORETURN

    ps = map pretty params

    ps' = if S.member VARARG flags
          then ps ++ [pretty VARARG]
          else ps

    params' = encloseSep lparen rparen comma ps'

data StructType =
    Struct |
    Union
    deriving (Eq, Show)

instance Pretty StructType where
    pretty Struct = pretty "struct"
    pretty Union = pretty "union"

data RawType =
    -- Basic types
    TyVoid |
    TyBool |
    TyInt Sign IntType |  -- Signed
    TyFloat |
    TyDouble |
    TyLongDouble |

    -- Enumerations
    TyEnum (Maybe Symbol) (Maybe [EnumEntry]) |

    -- Derived types
    TyArray Type (Maybe Integer) |
    TyStruct StructType (Maybe Symbol) (Maybe [StructEntry]) |
    TyPointer Type |

    TyFunction FunType |

    -- Typedefs.  This is a reference to a previously defined typedef
    TyAlias Symbol |

    TyTypeofExp Integer |
    TyTypeofType Type

    deriving (Eq, Show)

data CVR =
    CONST |
    VOLATILE |
    RESTRICT
    deriving (Eq, Ord, Show)

instance Pretty CVR where
    pretty CONST = pretty "const"
    pretty VOLATILE = pretty "volatile"
    pretty RESTRICT = pretty "restrict"

data Type = Type RawType (Set CVR)
    deriving (Eq, Show)

