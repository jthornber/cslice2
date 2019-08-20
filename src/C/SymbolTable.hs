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
    NameSpaceStruct |
    NameSpaceEnum |
    NameSpaceLabel |
    NameSpaceGeneral
    deriving (Eq, Show)

data Symbol = Symbol Identifier NameSpace Scope
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

data SymbolTable = SymbolTable {
    stStructs :: Map Identifier Symbol,
    stEnums :: Map Identifier Symbol,
    stLabels :: Map Identifier Symbol,
    stVars :: Map Identifier Symbol,
    stTypedefs :: Map Identifier Symbol
} deriving (Eq, Show)

empty :: SymbolTable
empty = SymbolTable {
    stStructs = M.empty,
    stEnums = M.empty,
    stLabels = M.empty,
    stVars = M.empty,
    stTypedefs = M.empty
}

-------------------
-- FIXME: stubbed
-------------------

defFun, defStruct, defStructElt, defEnum, defEnumElt, defLabel, defVar, defTypedef
    :: Identifier -> SymbolTable -> (SymbolTable, Symbol)

defFun nm st = (st, Symbol nm NameSpaceGeneral ScopeGlobal)
defStruct nm st = (st, Symbol nm NameSpaceStruct ScopeGlobal)
defStructElt nm st = (st, Symbol nm NameSpaceStruct ScopeGlobal)
defEnum nm st = (st, Symbol nm NameSpaceEnum ScopeGlobal)
defEnumElt nm st = (st, Symbol nm NameSpaceEnum ScopeGlobal)
defLabel nm st = (st, Symbol nm NameSpaceLabel ScopeGlobal)
defVar nm st = (st, Symbol nm NameSpaceGeneral ScopeGlobal)
defTypedef nm st = (st, Symbol nm NameSpaceGeneral ScopeGlobal)

refFun, refStruct, refStructElt, refEnum, refLabel, refVar, refTypedef
    :: Identifier -> SymbolTable -> Symbol

refFun nm st = Symbol nm NameSpaceGeneral ScopeGlobal
refStruct nm st = Symbol nm NameSpaceGeneral ScopeGlobal
refStructElt nm st = Symbol nm NameSpaceGeneral ScopeGlobal
refEnum nm st = Symbol nm NameSpaceGeneral ScopeGlobal
refEnumElt nm st = Symbol nm NameSpaceGeneral ScopeGlobal
refLabel nm st = Symbol nm NameSpaceGeneral ScopeGlobal
refVar nm st = Symbol nm NameSpaceGeneral ScopeGlobal
refTypedef nm st = Symbol nm NameSpaceGeneral ScopeGlobal

beginBlock :: SymbolTable -> SymbolTable
beginBlock st = st

endBlock :: SymbolTable -> SymbolTable
endBlock st = st

