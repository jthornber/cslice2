{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module C.SymbolTable (
            Scope(..),
    UUID(..),
    Symbol(..),
    SymbolTable(..),
    empty,
    defFun,
    defStruct,
    defEnum,
    defLabel,
    defVar,
    rmVar,
    defTypedef,
    refFun,
    refStruct,
    refStructElt,
    refEnum,
    refLabel,
    refVar,
    refTypedef,
    enterScope,
    leaveScope
    ) where

import C.Identifier

import Control.Lens hiding (element)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

import Data.Text.Prettyprint.Doc

-- We have a couple of non-lexical scopes: global, and file.  Then we
-- have function scope (eg, labels), and block scope (lexical scoping).

-- We need a way of denoting a scope in code, eg,
-- [ScopeFunction "dm_btree_remove", ScopeBlock [1, 2, 1]]

-- FIXME: these aren't all scopes
-- FIXME: differentiate between the name of a struct, and a field of a struct/union/enum
data Scope =
    ScopeGlobal |
    ScopeFile |
    ScopeParam |
    ScopeFunction |
    ScopeBlock
    deriving (Eq, Show)

type UUID = Int

-- Each symbol has a unique int id
data Symbol = Symbol UUID Identifier
    deriving (Eq, Show)

instance Pretty Symbol where
    pretty (Symbol _ nm) = pretty nm

data Frame = Frame {
    _fScope :: Scope,
    _fFuns :: Map Identifier Symbol,
    _fStructs :: Map Identifier Symbol,
    _fEnums :: Map Identifier Symbol,
    _fEnumElts :: Map Identifier Symbol,
    _fLabels :: Map Identifier Symbol,
    _fVars :: Map Identifier Symbol,
    _fTypedefs :: Map Identifier Symbol
} deriving (Eq, Show)

makeLenses ''Frame

data SymbolTable = SymbolTable [Frame]
    deriving (Eq, Show)

empty :: SymbolTable
empty = SymbolTable
    [Frame ScopeGlobal M.empty M.empty M.empty M.empty M.empty M.empty M.empty]

defFun, defStruct, defEnum, defVar, defTypedef
    :: UUID -> Identifier -> SymbolTable -> Maybe (SymbolTable, Symbol)

defThing :: Lens' Frame (Map Identifier Symbol) -> UUID -> Identifier ->
            SymbolTable -> Maybe (SymbolTable, Symbol)
defThing lens uuid nm (SymbolTable (f:fs)) =
    if M.member nm $ view lens f
    then Nothing
    else Just (st, Symbol uuid nm)
    where
        sym = Symbol uuid nm
        st = SymbolTable ((over lens (M.insert nm sym) f):fs)

defFun = defThing fFuns
defStruct = defThing fStructs
defEnum = defThing fEnums
defVar = defThing fVars
defTypedef = defThing fTypedefs

defLabel :: UUID -> Identifier -> SymbolTable -> Maybe (SymbolTable, Symbol)
defLabel = undefined

-- This is a hack to make the Translate module a bit easier to code
rmVar :: Identifier -> SymbolTable -> Maybe SymbolTable
rmVar nm (SymbolTable (f:fs)) =
    if M.member nm $ view fVars f
    then Just $ SymbolTable $ (over fVars (M.delete nm) f) : fs
    else Nothing
    where
        vars = view fVars f

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- refThing :: (SymbolTable -> Map Identifier Symbol) -> Identifier -> SymbolTable -> Maybe Symbol
refThing fn nm (SymbolTable frames) = firstJust lookup frames
    where
        lookup :: Frame -> Maybe Symbol
        lookup = M.lookup nm . view fn

refFun :: Identifier -> SymbolTable -> Maybe Symbol
refFun = refThing fFuns

refStruct :: Identifier -> SymbolTable -> Maybe Symbol
refStruct = refThing fStructs

refStructElt :: Identifier -> SymbolTable -> Maybe Symbol
refStructElt nm st = Nothing

refEnum :: Identifier -> SymbolTable -> Maybe Symbol
refEnum = refThing fEnums

refLabel :: Identifier -> SymbolTable -> Maybe Symbol
refLabel = refThing fLabels

refVar :: Identifier -> SymbolTable -> Maybe Symbol
refVar = refThing fVars

refTypedef :: Identifier -> SymbolTable -> Maybe Symbol
refTypedef = refThing fTypedefs

enterScope :: Scope -> SymbolTable -> SymbolTable
enterScope sc (SymbolTable frames) = SymbolTable $
    (Frame sc M.empty M.empty M.empty M.empty M.empty M.empty M.empty : frames)

leaveScope :: SymbolTable -> Maybe SymbolTable
leaveScope (SymbolTable []) = Nothing
leaveScope (SymbolTable (f:fs)) = Just $ SymbolTable fs

