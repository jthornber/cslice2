module C.SymbolTable (
    NameSpace(..),
    Scope(..),
    Symbol(..),
    SymbolTable(..),
    empty,
    defFun,
    defStruct,
    defStructElt,
    defEnum,
    defEnumElt,
    defLabel,
    defVar,
    defTypedef,
    refFun,
    refStruct,
    refStructElt,
    refEnum,
    refEnumElt,
    refLabel,
    refVar,
    refTypedef,
    beginBlock,
    endBlock
    ) where

import C.Identifier

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text.Prettyprint.Doc

-- We have a couple of non-lexical scopes: global, and file.  Then we
-- have function scope (eg, labels), and block scope (lexical scoping).

-- We need a way of denoting a scope in code, eg,
-- [ScopeFunction "dm_btree_remove", ScopeBlock [1, 2, 1]]

-- Namespaces:
-- struct/union
-- enum
-- label
-- general

data NameSpace =
    NSStruct |
    NSEnum |
    NSLabel |
    NSGeneral
    deriving (Eq, Show)

-- FIXME: these aren't all scopes
-- FIXME: differentiate between the name of a struct, and a field of a struct/union/enum
data Scope =
    ScopeGlobal |
    ScopeFile |
    ScopeParam |
    ScopeFunction Symbol |
    ScopeBlock Symbol [Int]
    deriving (Eq, Show)

data Symbol = Symbol Identifier NameSpace Scope
    deriving (Eq, Show)

instance Pretty Symbol where
    pretty (Symbol nm _ _) = pretty nm

data Frame = Frame {
    fStructs :: Map Identifier Symbol,
    fEnums :: Map Identifier Symbol,
    fLabels :: Map Identifier Symbol,
    fVars :: Map Identifier Symbol,
    fTypedefs :: Map Identifier Symbol
} deriving (Eq, Show)

data SymbolTable = SymbolTable [Frame]
    deriving (Eq, Show)

empty :: SymbolTable
empty = SymbolTable []

-------------------
-- FIXME: stubbed
-------------------

defFun, defStruct, defStructElt, defEnum, defEnumElt, defLabel, defVar, defTypedef
    :: Identifier -> SymbolTable -> (SymbolTable, Symbol)

defFun nm st = (st, Symbol nm NSGeneral ScopeGlobal)
defStruct nm st = (st, Symbol nm NSStruct ScopeGlobal)
defStructElt nm st = (st, Symbol nm NSStruct ScopeGlobal)
defEnum nm st = (st, Symbol nm NSEnum ScopeGlobal)
defEnumElt nm st = (st, Symbol nm NSEnum ScopeGlobal)
defLabel nm st = (st, Symbol nm NSLabel ScopeGlobal)
defVar nm st = (st, Symbol nm NSGeneral ScopeGlobal)
defTypedef nm st = (st, Symbol nm NSGeneral ScopeGlobal)

refFun, refStruct, refStructElt, refEnum, refLabel, refVar, refTypedef
    :: Identifier -> SymbolTable -> Maybe Symbol

refFun nm st = Just $ Symbol nm NSGeneral ScopeGlobal
refStruct nm st = Just $ Symbol nm NSGeneral ScopeGlobal
refStructElt nm st = Just $ Symbol nm NSGeneral ScopeGlobal
refEnum nm st = Just $ Symbol nm NSGeneral ScopeGlobal
refEnumElt nm st = Just $ Symbol nm NSGeneral ScopeGlobal
refLabel nm st = Just $ Symbol nm NSGeneral ScopeGlobal
refVar nm st = Just $ Symbol nm NSGeneral ScopeGlobal
refTypedef nm st = Just $ Symbol nm NSGeneral ScopeGlobal

beginBlock :: SymbolTable -> SymbolTable
beginBlock st = st

endBlock :: SymbolTable -> SymbolTable
endBlock st = st

