module Main where

import qualified AST as AST
import HIR
import Lexer
import Parser
import SymbolTable

import Data.Set (Set)
import qualified Data.Set as S

{-
xTranslationUnit :: AST.TranslationUnit -> TranslationUnit
xTranslationUnit (AST.TranslationUnit xs) = TranslationUnit (map xExternalDeclaration xs)

xExternalDeclaration :: AST.ExternalDeclaration -> [ExternalDeclaration]
xExternalDeclaration (ExternalDeclaration d) = undefined
xExternalDeclaration (FunDef dss declarator decls statement) = FunDef FunType Statement
xExternalDeclaration (AsmDeclaration a) = undefined

-- hir
data Declaration =
    Declaration Type StorageClass Identifier (Maybe Literal)

xDeclaration :: AST.Declaration -> [Declaration]
xDeclaration (AST.Declaration dss inits) = undefined
xDeclaration AST.StaticAssert = []

data DeclarationSpecifier =
    DSStorageClass StorageClass |
    DSTypeSpecifier TypeSpecifier |
    DSTypeQualifier TypeQualifier |
    DSFunctionSpecifier FunctionSpecifier |
    DSAlignmentSpecifier AlignmentSpecifier |
    DSAttr [Attr]
    deriving (Eq, Show)

xStorageClass :: AST.StorageClass -> StorageClass
xStorageClass AST.Extern = Extern
xStorageClass AST.Static = Static
xStorageClass AST.Auto = Auto
xStorageClass AST.Register = Register
xStorageClass ThreadLocal = error "not supporting threads"

-- A little combinator library for building types




-- declaration specifiers contain information about scope and type
mkExtern :: Type -> Type



xDeclarationSpecifiers :: [AST.DeclarationSpecifier] -> Type
xDeclarationSpecifiers (AST.DSStorageClass AST.Extern) =

data RawType =
    -- Basic types
    TyVoid |
    TyInt Bool IntType |
    TyFloat |
    TyDouble |
    TyLongDouble |

    -- Enumerations
    TyEnumeration (Maybe Identifier) [EnumEntry] |

    -- Derived types
    TyArray Type (Maybe Integer) |
    TyStruct StructType [StructEntry] |
    TyPointer Type |

    TyFunction FunType |

    -- Typedefs
    TyAlias Identifier Type

    deriving (Eq, Show)

data Type = Type RawType Bool Bool Bool   -- const, volatile, restrict
    deriving (Eq, Show)

xTypeSpecifier :: AST.TypeSpecifier
xTypeSpecifier AST.Void = TyVoid
xTypeSpecifier AST.Char = TyInt True CHAR
xTypeSpecifier AST.Short = TyInt True SHORT
xTypeSpecifier AST.Int = TyInt True INT
xTypeSpecifier AST.Int128 = TyInt True INT128
xTypeSpecifier AST.Long = TyInt True LONG
xTypeSpecifier AST.Float = TyFloat
xTypeSpecifier AST.Double = TyDouble
xTypeSpecifier AST.Signed = TyInt False
xTypeSpecifier AST.Unsigned =
xTypeSpecifier AST.Bool =
xTypeSpecifier AST.Complex =
xTypeSpecifier AST.AtomicSpecifier =
xTypeSpecifier AST.StructOrUnionSpecifier StructType (Maybe Identifier) (Maybe [StructDeclaration]) =
xTypeSpecifier AST.EnumDefSpecifier (Maybe Identifier) [Enumerator] =
xTypeSpecifier AST.EnumRefSpecifier Identifier =
xTypeSpecifier AST.TSTypedefName Identifier =
xTypeSpecifier AST.TSTypeofExp Exp =
xTypeSpecifier AST.TSTypeofDecl [DeclarationSpecifier]
-}


{-
In the AST the types are split into two halves.

i) A list of DeclarationSpecifiers, which contain:
   - storage class (static, extern, auto, register)
   - Basic type info
   - const, volatile, restrict qualfiers
   - FunctionSpecifier (inline, noreturn)
   - AlignmentSpecifiers

ii) A declarator; which is a messy combination of optional identifier, pointer,
    param list and arrays.

In addition the fields of structs and unions can have a width specified
if they're a basic type.

Decoding this mess is quite tricky.  eg.  In HIR the cvr qualifiers are
applied after the fundamental type is constructed.  Also there's more info
in here than is represented by the HIR 'Type' (storage class).

Processing an AST.Declaration is going to give us:
  ([(Type, Maybe Identifier)], StorageClass, [FunctionSpecifier], Maybe AlignmentSpecifier)

Processing an AST.StructDeclaration gives us:
  ([(Type, Maybe Identifier, Int)])
-}

type Error a = Either String a

extractStorageClass :: [AST.StorageClass] -> Error (Maybe StorageClass)
extractStorageClass specs = case specs of
    []  -> Right Nothing
    [x] -> getSC x
    _   -> Left "Too many storage classes"
    where
        getSC AST.Typedef       = bad
        getSC AST.Extern        = Right $ Just Extern
        getSC AST.Static        = Right $ Just Static
        getSC AST.ThreadLocal   = bad
        getSC AST.Auto          = Right $ Just Auto
        getSC AST.Register      = Right $ Just Register

        bad = Left "unsupported storage class"

dsSplit :: [AST.DeclarationSpecifier] -> ([AST.StorageClass], [AST.TypeSpecifier],
                                          [AST.TypeQualifier], [AST.FunctionSpecifier],
                                          [AST.AlignmentSpecifier])
dsSplit = foldr get ([], [], [], [], [])
    where
        get (AST.DSStorageClass x) (sc, ts, tq, fs, align) = (x:sc, ts, tq, fs, align)
        get (AST.DSTypeSpecifier x) (sc, ts, tq, fs, align) = (sc, x:ts, tq, fs, align)
        get (AST.DSTypeQualifier x) (sc, ts, tq, fs, align) = (sc, ts, x:tq, fs, align)
        get (AST.DSFunctionSpecifier x) (sc, ts, tq, fs, align) = (sc, ts, tq, x:fs, align)
        get (AST.DSAlignmentSpecifier x) (sc, ts, tq, fs, align) = (sc, ts, tq, fs, align)
        get (AST.DSAttr _) r = r

scanTypeQualifiers :: [AST.TypeQualifier] -> Set CVR
scanTypeQualifiers = foldr insert S.empty
    where
        insert AST.Const s    = S.insert CONST s
        insert AST.Volatile s = S.insert VOLATILE s
        insert AST.Restrict s = S.insert RESTRICT s
        insert AST.Atomic _   = error "Atomic not supported"

applyTypeQualifier :: AST.TypeQualifier -> Type -> Type
applyTypeQualifier AST.Const (Type rt s) = Type rt (S.insert CONST s)
applyTypeQualifier AST.Volatile (Type rt s) = Type rt (S.insert VOLATILE s)
applyTypeQualifier AST.Restrict (Type rt s) = Type rt (S.insert RESTRICT s)
applyTypeQualifier AST.Atomic _ = error "Atomic not supported"

xlateTypeSpecifier :: [AST.TypeSpecifier] -> Error RawType
xlateTypeSpecifier specs = case specs of
    [] -> Left "No type specifier given"
    [x] -> getTS x
    _ -> Left "Too many type specifiers"

    where
        getTS :: AST.TypeSpecifier -> Error RawType
        getTS AST.Void                  = Right TyVoid
        getTS AST.Char                  = Right $ signed CHAR
        getTS AST.UnsignedChar          = Right $ unsigned CHAR
        getTS AST.Short                 = Right $ signed SHORT
        getTS AST.UnsignedShort         = Right $ unsigned SHORT
        getTS AST.Int                   = Right $ signed INT
        getTS AST.UnsignedInt           = Right $ unsigned INT
        getTS AST.Int128                = Right $ signed INT128
        getTS AST.UnsignedInt128        = Right $ unsigned INT128
        getTS AST.Long                  = Right $ signed LONG
        getTS AST.UnsignedLong          = Right $ unsigned LONG
        getTS AST.LongLong              = Right $ signed LONG_LONG
        getTS AST.UnsignedLongLong      = Right $ unsigned LONG_LONG
        getTS AST.Float                 = Right TyFloat
        getTS AST.Double                = Right TyDouble
        getTS AST.Bool                  = Right TyBool
        getTS AST.Complex               = Left "complex nrs not implemented"
        getTS AST.AtomicSpecifier       = Left "atomics not implemented"
        getTS (AST.StructOrUnionSpecifier ty mnm fields) = undefined
            -- getTS TyStruct (convertStructType ty) mnm (concatMap convertStructEntry $ fields)
        getTS (AST.EnumDefSpecifier _ _) = undefined
        getTS (AST.EnumRefSpecifier _)  = undefined
        getTS (AST.TSTypedefName _)     = undefined
        getTS (AST.TSTypeofExp _)       = undefined
        getTS (AST.TSTypeofDecl _)      = undefined

        signed x = TyInt SIGNED x
        unsigned x = TyInt UNSIGNED x

        convertStructType AST.Struct = Struct
        convertStructType AST.Union = Union

        convertStructEntry (AST.StructDeclaration specs declarators) = undefined
        convertStructEntry AST.StructStaticAssert = Left "Struct static assert not supported"

applyDeclarator :: Type -> (Maybe StorageClass) -> AST.Declarator -> Error Declaration
applyDeclarator ty sc (AST.Declarator Nothing dd) = expandDD ty sc dd
applyDeclarator ty sc (AST.Declarator (Just ptr) dd) = expandDD ty sc dd >>= expandPtr ptr

-- FIXME: expand mp
expandPtr :: AST.Pointer -> Declaration -> Error Declaration
expandPtr (AST.Pointer tq mp) (Declaration ty sc nm ml) =
    pure $
    Declaration (Type (TyPointer ty) (scanTypeQualifiers tq)) sc nm ml

-- FIXME: check vararg
convertParams :: AST.ParameterTypeList -> Error [ParamEntry]
convertParams (AST.ParameterTypeList pds vararg) = do
    decls <- mapM expand pds
    mapM toEntry decls
    where
        expand (AST.PDDeclarator ds d) = xlateDeclaration' ds d
        expand (AST.PDAbstract ds md) = undefined

        -- FIXME: handle anonymous params
        toEntry :: Declaration -> Error ParamEntry
        toEntry (Declaration t msc nm ml) = Right $ ParamEntry t (Just nm)
        toEntry (FunDeclaration t sc nm fs) = Left "Fun declaration can't be a param type"

expandDD :: Type -> (Maybe StorageClass) -> AST.DirectDeclarator -> Error Declaration
expandDD ty sc (AST.DDIdentifier nm)          = Right $ Declaration ty sc nm Nothing
expandDD ty sc (AST.DDNested d)               = applyDeclarator ty sc d
expandDD ty sc (AST.DDArray dd' _ me _ _)     = expandDD (Type (TyArray ty (Just 0)) S.empty) sc dd'  -- FIXME: apply the qualifiers
expandDD ty sc (AST.DDFun dd' params)         = do
    ps <- convertParams params
    ed <- applyDeclarator ty sc (AST.Declarator Nothing dd')
    case ed of
        (Declaration ty' sc' nm _) -> pure $ Declaration (Type (TyFunction (FunType ty' ps S.empty)) S.empty) sc' nm Nothing
        (FunDeclaration _ _ _ _)   -> Left "Unexpected function declaration"
expandDD ty sc (AST.DDFunPtr dd' nms)         = undefined

-- FIXME: a quick hack
initToDecl :: AST.InitDeclarator -> AST.Declarator
initToDecl (AST.InitDeclarator d _) = d

xlateDeclaration' :: [AST.DeclarationSpecifier] -> AST.Declarator -> Error Declaration
xlateDeclaration' ds declarator = do
    rt <- xlateTypeSpecifier ts
    let t = foldr applyTypeQualifier (Type rt S.empty) tq
    sClass <- extractStorageClass sc
    applyDeclarator t sClass $ declarator
    where
        (sc, ts, tq, fs, align) = dsSplit ds

-- Declaration Type StorageClass Identifier (Maybe Literal)
xlateDeclaration :: AST.Declaration -> Error [Declaration]
xlateDeclaration (AST.Declaration ds declarators) =
    mapM (xlateDeclaration' ds) .
    map initToDecl $
    declarators

--------------------------------------------------------------

parse s = runAlex s declaration

main :: IO ()
main = do
    input <- getContents
    let ast = parse input
    case ast of
        Left e -> error e
        Right ast' -> print . xlateDeclaration $ ast'

