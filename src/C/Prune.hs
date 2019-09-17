module C.Prune (
    Reference(..)
    ) where

import C.HIR
import C.SymbolTable

import Data.Set (Set)
import qualified Data.Set as S

-- All the symbols are unique, so we can calculate all references
-- of a HIR tree by just collecting a Set of the Symbols contained
-- within it.

class Reference a where
    refs :: a -> Set Symbol

instance (Reference a) => Reference (Maybe a) where
    refs Nothing = S.empty
    refs (Just e) = refs e

instance (Reference a) => Reference [a] where
    refs = foldr S.union S.empty . map refs

instance Reference Symbol where
    refs sym = S.singleton sym

instance Reference EnumEntry where
    refs (EnumEntry sym me) = S.singleton sym `S.union` refs me

instance Reference StructEntry where
    refs (StructEntry t ms me) = refs t `S.union` refs ms `S.union` refs me
    refs (StructEntryWidthOnly e) = refs e

instance Reference ParamEntry where
    refs (ParamEntry t ms) = refs t `S.union` refs ms

instance Reference FunType where
    refs (FunType t ps _) = refs t `S.union` refs ps

instance Reference RawType where
    refs (TyEnum ms mes) = refs ms `S.union` refs mes
    refs (TyArray t me) = refs t `S.union` refs me
    refs (TyStruct _ sym es) = S.singleton sym `S.union` refs es
    refs (TyStructRef _ sym) = S.singleton sym
    refs (TyPointer t) = refs t
    refs (TyFunction ft) = refs ft
    refs (TyAlias sym) = S.singleton sym
    refs (TyTypeofExp e) = refs e
    refs (TyTypeofType t) = refs t

instance Reference Type where
    refs (Type rt _) = refs rt

instance Reference RawExp where
    refs (VarExp sym) = S.singleton sym
    refs (LiteralExp v) = refs v
    refs (SubscriptExp e1 e2) = refs e1 `S.union` refs e2
    refs (FuncallExp e es) = refs e `S.union` refs es
    refs (StructElt e sym) = refs e `S.union` S.singleton sym
    refs (StructDeref e sym) = refs e `S.union` S.singleton sym
    refs (CommaExp e1 e2) = refs e1 `S.union` refs e2
    refs (AssignExp e1 e2) = refs e1 `S.union` refs e2
    refs (ConditionalExp e1 e2 e3) = refs [e1, e2, e3]
    refs (BinaryExp _ e1 e2) = refs [e1, e2]
    refs (UnaryExp _ e) = refs e
    refs (CastExp t e) = refs t `S.union` refs e
    refs (ImplicitCastExp t e) = refs t `S.union` refs e
    refs (SizeofValueExp e) = refs e
    refs (SizeofTypeExp t) = refs t
    refs (AlignofExp t) = refs t
    refs (BlockExp syms items) = refs syms `S.union` refs items
    refs BuiltinVaArg = S.empty
    refs BuiltinOffsetOf = S.empty
    refs BuiltinTypesCompatible = S.empty
    refs BuiltinConvertVector = S.empty

instance Reference Exp where
    refs (Exp t re) = refs t `S.union` refs re

instance Reference Declaration where
    refs (Declaration t _ sym mlit) = refs t `S.union` (S.singleton sym) `S.union` refs mlit
    refs (FunDeclaration t _ sym _) = refs t `S.union` (S.singleton sym)
    refs (TypedefDeclaration t sym) = refs t `S.union` refs sym

instance Reference Literal where
    refs (Literal t v) = refs t `S.union` refs v

instance Reference Value where
    refs (EnumValue sym) = S.singleton sym
    refs (CompoundValue t pairs) = refs t `S.union` refs pairs
    refs _ = S.empty

instance Reference Initializer where
    refs (InitAssign e) = refs e
    refs (InitList pairs) = refs pairs

instance Reference InitializerPair where
    refs (InitializerPair mds i) = refs mds `S.union` refs i

instance Reference Designator where
    refs (SubscriptDesignator e) = refs e
    refs (StructDesignator sym) = S.singleton sym

instance Reference Statement where
    refs (LabelStatement sym s) = S.singleton sym `S.union` refs s
    refs (CaseStatement e me s) = refs e `S.union` refs me `S.union` refs s
    refs (DefaultStatement s) = refs s
    refs (CompoundStatement syms items) = refs syms `S.union` refs items
    refs (ExpressionStatement e) = refs e
    refs (IfStatement e s ms) = refs e `S.union` refs s `S.union` refs ms
    refs (SwitchStatement e s) = refs e `S.union` refs s
    refs (WhileStatement e s) = refs e `S.union` refs s
    refs (DoStatement s e) = refs s `S.union` refs e
    refs (ForStatement ds me1 me2 me3 s) = refs ds `S.union` refs [me1, me2, me3] `S.union` refs s
    refs (GotoStatement sym) = S.singleton sym
    refs ContinueStatement = S.empty
    refs BreakStatement = S.empty
    refs (ReturnStatement me) = refs me
    refs EmptyStatement = S.empty
    refs AsmStatement = S.empty

instance Reference BlockItem where
    refs (BIDeclaration d) = refs d
    refs (BIStatement s) = refs s

instance Reference TranslationUnit where
    refs (TranslationUnit ds) = refs ds

instance Reference ExternalDeclaration where
    refs (ExternalDeclaration d) = refs d
    refs (FunDef ft sym s) = refs ft `S.union` S.singleton sym `S.union` refs s
    refs AsmDeclaration = S.empty

{-
prune :: Set Symbol -> TranslationUnit -> TranslationUnit
prune rs
-}
