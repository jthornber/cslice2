module C.PrettyPrint (
    ppTranslationUnit
    ) where

import C.HIR
import C.SymbolTable

import Data.List (intercalate)
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

printRawType (TyStruct st nm' mentries) nm = header <> body <+> nm
    where
        header = pretty "struct" <+> pretty nm'
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

instance Pretty EnumEntry where
    pretty (EnumEntry nm (Just n)) = (pretty nm) <+> (pretty "=") <+> (pretty n)
    pretty (EnumEntry nm Nothing) = pretty nm

instance Pretty StructEntry where
    pretty (StructEntry t mnm Nothing) = printType t $ mPretty mnm
    pretty (StructEntry t mnm (Just n)) =
        (printType t $ mPretty mnm) <+> (pretty ":") <+> (pretty n)
    pretty (StructEntryWidthOnly e) = pretty ":" <+> pretty e

instance Pretty ParamEntry where
    pretty (ParamEntry t Nothing) = printType t emptyDoc
    pretty (ParamEntry t (Just nm)) = printType t (pretty nm)

instance Pretty FunFlag where
    pretty INLINE = pretty "inline"
    pretty NORETURN = pretty "__Noreturn"
    pretty VARARG = pretty "..."

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

instance Pretty StructType where
    pretty Struct = pretty "struct"
    pretty Union = pretty "union"

instance Pretty CVR where
    pretty CONST = pretty "const"
    pretty VOLATILE = pretty "volatile"
    pretty RESTRICT = pretty "restrict"

instance Pretty BinOp where
    pretty LOGICAL_OR = pretty "||"
    pretty LOGICAL_AND = pretty "&&"
    pretty BIT_OR = pretty "|"
    pretty BIT_AND = pretty "&"
    pretty XOR = pretty "^"
    pretty C.HIR.EQ = pretty "=="
    pretty NEQ = pretty "!="
    pretty C.HIR.LT = pretty "<"
    pretty LTE = pretty "<="
    pretty C.HIR.GT = pretty ">"
    pretty GTE = pretty ">="
    pretty LSHIFT = pretty "<<"
    pretty RSHIFT = pretty ">>"
    pretty PLUS = pretty "+"
    pretty MINUS = pretty "-"
    pretty MULT = pretty "*"
    pretty DIV = pretty "/"
    pretty MOD = pretty "%"

instance Pretty UnaryOp where
    pretty ADDRESS_OF = pretty "&"
    pretty LABEL_ADDRESS = pretty "&&"
    pretty DEREF = pretty "*"
    pretty UNARY_PLUS = pretty "+"
    pretty UNARY_MINUS = pretty "-"
    pretty BIT_NOT = pretty "~"
    pretty LOGICAL_NOT = pretty "!"
    pretty PRE_INC = pretty "++"
    pretty POST_INC = pretty "++"
    pretty PRE_DEC = pretty "--"
    pretty POST_DEC = pretty "--"

instance Pretty RawExp where
    pretty (VarExp nm) = pretty nm
    pretty (LiteralExp v) = pretty v

    pretty (SubscriptExp e1 e2) = hsep [
        lbracket,
        pretty e1,
        rbracket,
        pretty '[',
        pretty e2,
        pretty ']' ]

    pretty (FuncallExp e1 params) = pretty e1 <> encloseSep lparen rparen (comma <> space) (map pretty params)

    pretty (StructElt e nm) = hsep [
        lbracket,
        pretty e,
        rbracket,
        pretty '.',
        pretty nm]

    pretty (StructDeref e nm) = hsep [
        lbracket,
        pretty e,
        rbracket,
        pretty "->",
        pretty nm]

    pretty (CommaExp e1 e2) = hsep [pretty e1, comma, pretty e2]

    pretty (AssignExp e1 e2) = hsep [pretty e1, pretty '=', pretty e2]
    pretty (ConditionalExp e1 e2 e3) = hsep [
        parens $ pretty e1,
        pretty '?',
        parens $ pretty e2,
        pretty ':',
        parens $ pretty e3]

    pretty (BinaryExp op e1 e2) = hsep [
        parens $ pretty e1,
        pretty op,
        parens $ pretty e2]
    pretty (UnaryExp op e) = hsep [pretty op, parens $ pretty e]

    pretty (CastExp t e) = hsep [
        parens $ printType t emptyDoc,
        parens $ pretty e]

    pretty (ImplicitCastExp _ e) = pretty e

    pretty (SizeofValueExp e) = hsep [
        pretty "sizeof",
        parens $ pretty e]

    pretty (SizeofTypeExp t) = hsep [
        pretty "sizeof",
        parens $ printType t emptyDoc]

    pretty (AlignofExp t) = hsep [
        pretty "alignof",
        parens $ printType t emptyDoc]

    -- FIXME: not sure what the nms are for
    pretty (BlockExp _ items) = parens $
        encloseSep lbracket rbracket (pretty ';' <> line) (map pretty items)

    pretty (BuiltinVaArg) = pretty "__builtinvararg"
    pretty (BuiltinOffsetOf) = pretty "__builtinoffsetof"
    pretty (BuiltinTypesCompatible) = pretty "__builtintypescompatible"
    pretty (BuiltinConvertVector) = pretty "__builtinconvertvector"

instance Pretty Exp where
    pretty (Exp _ e) = pretty e

instance Pretty Declaration where
    pretty (Declaration t msc nm mlit) = hcat [
        maybe (emptyDoc) ((<> space) . pretty) msc,
        printType t (pretty nm),
        maybe (emptyDoc) pretty mlit]

    pretty (FunDeclaration t sc nm flags) = hsep [
        pretty sc,
        hsep $ map pretty $ S.toList flags,
        printType t (pretty nm)]

    pretty (TypedefDeclaration t nm) = hsep [
        pretty "typedef",
        printType t (pretty nm)]

instance Pretty FunctionSpecifier where
    pretty Inline = pretty "inline"
    pretty Noreturn = pretty "__noreturn"

instance Pretty Literal where
    pretty (Literal t v) = hsep [
        parens $ printType t emptyDoc,
        pretty v]

instance Pretty Value where
    pretty (IntValue n) = pretty n
    pretty (FloatValue f) = pretty f
    pretty (DoubleValue d) = pretty d
    pretty (EnumValue nm) = pretty nm
    pretty (CompoundValue _ vs) = encloseSep lbrace rbrace comma $ map pretty vs
    pretty (StringValue str) = dquotes $ pretty str    -- FIXME: handle quoting
    pretty (CharValue c) = squotes $ pretty c          -- FIXME: handle quoting

instance Pretty Initializer where
    pretty (InitAssign e) = pretty "=" <+> pretty e
    pretty (InitList pairs) = encloseSep lbrace rbrace comma $ map pretty pairs

instance Pretty InitializerPair where
    pretty (InitializerPair Nothing i) = pretty i
    pretty (InitializerPair (Just ds) i) = (cat . map pretty $ ds) <+> pretty i

instance Pretty Designator where
    pretty (SubscriptDesignator e) = brackets $ pretty e
    pretty (StructDesignator nm) = dot <> pretty nm

instance Pretty StorageClass where
    pretty Extern = pretty "extern"
    pretty Static = pretty "static"
    pretty Auto = pretty "auto"
    pretty Register = pretty "register"

withSemi :: Doc ann -> Doc ann
withSemi d = d <> pretty ";"

instance Pretty Statement where
    pretty (LabelStatement nm s) = withSemi $ vsep [
        pretty nm <> pretty ":",
        pretty s]

    pretty (CaseStatement e Nothing s) = withSemi $ vsep [
        pretty e <> pretty ":",
        pretty s]

    pretty (CaseStatement e1 (Just e2) s) = withSemi $ vsep [
        pretty e1 <> pretty ".." <> pretty e2 <> pretty ":",
        pretty s]

    pretty (DefaultStatement s) = withSemi $ vsep [
        pretty "default:",
        pretty s]

    -- FIXME: what are the identifiers?
    pretty (CompoundStatement _ items) =
        nest 8 (lbrace <> line <> body) <> line <> rbrace
        where
            body = vsep $ map pretty items

    pretty (ExpressionStatement e) = withSemi $ pretty e

    pretty (IfStatement e t Nothing) = vsep [
        hsep [pretty "if", parens $ pretty e],
        pretty t]

    pretty (IfStatement e t (Just f)) = vsep [
        hsep [pretty "if", parens $ pretty e],
        pretty t,
        pretty "else",
        pretty f]

    pretty (SwitchStatement e s) = vsep [
        hsep [pretty "switch", parens $ pretty e],
        pretty s]

    pretty (WhileStatement e s) = vsep [
        hsep [pretty "while", parens $ pretty e],
        pretty s]

    pretty (DoStatement s e) = withSemi $ vsep [
        pretty "do" <> pretty s,
        parens $ pretty e]

    pretty (ForStatement decls me1 me2 me3 s) = withSemi $ vsep [
        hsep [pretty "for", lparen, pretty decls, praps me1,
              semi, praps me2, semi, praps me3, rparen],
        pretty s]

    pretty (GotoStatement nm) = withSemi $ pretty "goto" <> pretty nm

    pretty (ContinueStatement) = withSemi $ pretty "continue"

    pretty (BreakStatement) = withSemi $ pretty "break"

    pretty (ReturnStatement Nothing) = withSemi $ pretty "return"
    pretty (ReturnStatement (Just e)) = withSemi $ pretty "return" <+> pretty e
    pretty (EmptyStatement) = withSemi emptyDoc
    pretty (AsmStatement) = emptyDoc

praps :: Pretty a => Maybe a -> Doc ann
praps Nothing = emptyDoc
praps (Just x) = pretty x

instance Pretty BlockItem where
    pretty (BIDeclaration d) = pretty d
    pretty (BIStatement s) = pretty s

instance Pretty TranslationUnit where
    pretty (TranslationUnit decls) = vsep $ map pretty decls

ppTranslationUnit :: TranslationUnit -> Doc ann
ppTranslationUnit = pretty

instance Pretty ExternalDeclaration where
    pretty (ExternalDeclaration decl) = pretty decl <> semi
    pretty (FunDef ft nm s) = prettyFun ft (pretty nm) <+> pretty s
    pretty (AsmDeclaration) = emptyDoc

