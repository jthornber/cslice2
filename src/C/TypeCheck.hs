module C.TypeCheck (
    deref,
    usualArithmeticConversion,
    arrayEltType,
    returnType,
    structEltType
    ) where

import C.HIR
import C.Int
import C.SymbolTable

import Data.Set (Set)
import qualified Data.Set as S

typeError :: String -> a
typeError = undefined

notImplemented = typeError "not implemented"

-- FIXME: rename to derefT?
deref :: Type -> Type
deref = notImplemented

-- Works with either a pointer or an array type
arrayEltType :: Type -> Type
arrayEltType (Type (TyArray ty _) _) = ty
arrayEltType (Type (TyPointer ty) _) = ty
arrayEltType _ = typeError "not an array type"

structEltType :: Type -> Symbol -> Type
structEltType (Type (TyStruct _ _ fields) _) sym = case find fields of
    Nothing -> typeError "No such field"
    (Just t) -> t
    where
        find [] = Nothing
        find ((StructEntry t (Just sym') _):xs) | sym == sym' = Just t
        find (_:xs) = find xs
structEltType (Type (TyStructRef _ _) _) sym = typeError "Not a struct"
structEltType _ sym = typeError "not a struct"

returnType :: Type -> Type
returnType (Type (TyFunction (FunType t _ _)) _) = t

intRank :: IntType -> Int
intRank it = fromEnum it

higherRank :: IntType -> IntType -> IntType
higherRank t1 t2 = if intRank t1 > intRank t2 then t1 else t2

intRange :: Sign -> IntType -> (Integer, Integer)
intRange SIGNED CHAR = (-128, 127)
intRange SIGNED SHORT = (-32768, 32767)
intRange SIGNED INT = (-2147483648, 2147483647)
intRange SIGNED LONG = (-2147483648, 2147483647)
intRange SIGNED LONG_LONG = (-9223372036854775808, 9223372036854775807)
intRange SIGNED INT128 = (-9223372036854775808, 9223372036854775807)
intRange UNSIGNED CHAR = (0, 255)
intRange UNSIGNED SHORT = (0, 65535)
intRange UNSIGNED INT = (0, 4294967295)
intRange UNSIGNED LONG = (0, 4294967295)
intRange UNSIGNED LONG_LONG = (0, 18446744073709551615)
intRange UNSIGNED INT128 = (0, 18446744073709551615)

covers :: (Integer, Integer) -> (Integer, Integer) -> Bool
covers (l1, h1) (l2, h2) = l1 <= l2 && h1 >= h2

mismatchedSigns :: IntType -> IntType -> RawType
mismatchedSigns signedInt unsignedInt =
    if intRank unsignedInt > intRank signedInt
    then TyInt UNSIGNED unsignedInt
    else if (intRange SIGNED signedInt) `covers` (intRange UNSIGNED unsignedInt)
         then TyInt SIGNED signedInt
         else TyInt UNSIGNED signedInt

unifyRawType :: RawType -> RawType -> RawType
unifyRawType TyLongDouble _ = TyLongDouble
unifyRawType _ TyLongDouble = TyLongDouble
unifyRawType TyDouble _ = TyDouble
unifyRawType _ TyDouble = TyDouble
unifyRawType TyFloat _ = TyFloat
unifyRawType _ TyFloat = TyFloat
unifyRawType t@(TyInt s1 t1) (TyInt s2 t2) | s1 == s2 && t1 == t2 = t
unifyRawType t@(TyInt SIGNED t1) (TyInt SIGNED t2) = TyInt SIGNED $ higherRank t1 t2
unifyRawType t@(TyInt UNSIGNED t1) (TyInt UNSIGNED t2) = TyInt UNSIGNED $ higherRank t1 t2
unifyRawType (TyInt SIGNED t1) (TyInt UNSIGNED t2) = mismatchedSigns t1 t2
unifyRawType (TyInt UNSIGNED t1) (TyInt SIGNED t2) = mismatchedSigns t2 t1
unifyRawType _ _ = typeError "not a numeric type"

usualArithmeticConversion :: Type -> Type -> Type
usualArithmeticConversion (Type rt1 _) (Type rt2 _) = Type (unifyRawType rt1 rt2) S.empty

