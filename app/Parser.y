{
module Parser (
    expression,
    postfix_exp,
    unary_exp,
    cast_exp,
    additive_exp,
    conditional_exp,

    declaration,

    statement,
    translation_unit
    ) where

import AST
import Control.Monad
import Debug.Trace
import Token
import Lexer
}

%name expression expression
%name postfix_exp postfix_exp
%name unary_exp unary_exp
%name cast_exp cast_exp
%name additive_exp additive_exp
%name conditional_exp conditional_exp
%name statement statement

%name declaration declaration

%name translation_unit translation_unit

%lexer {lexStep} {T_EOF _}
%monad {Alex}
%tokentype {Token AlexPosn}
%error { parseError }

%token

'asm'           {T_ASM _}
'__attribute__'	{T_ATTRIBUTE _}
'auto'		{T_AUTO _}
'break'		{T_BREAK _}
'case'		{T_CASE _}
'char'		{T_CHAR _}
'const'		{T_CONST _}
'continue'	{T_CONTINUE _}
'default'	{T_DEFAULT _}
'do'		{T_DO _}
'double'	{T_DOUBLE _}
'else'		{T_ELSE _}
'enum'		{T_ENUM _}
'extern'	{T_EXTERN _}
'float'		{T_FLOAT _}
'for'		{T_FOR _}
'goto'		{T_GOTO _}
'if'		{T_IF _}
'inline'	{T_INLINE _}
'int'		{T_INT _}
'__int128'	{T_INT128 _}
'__label__'	{T_LABEL _}
'long'		{T_LONG _}
'register'	{T_REGISTER _}
'restrict'	{T_RESTRICT _}
'return'	{T_RETURN _}
'short'		{T_SHORT _}
'signed'	{T_SIGNED _}
'sizeof'	{T_SIZEOF _}
'static'	{T_STATIC _}
'struct'	{T_STRUCT _}
'switch'	{T_SWITCH _}
'typedef'	{T_TYPEDEF _}
'union'		{T_UNION _}
'unsigned'	{T_UNSIGNED _}
'void'		{T_VOID _}
'volatile'	{T_VOLATILE _}
'while'		{T_WHILE _}
'alignas'	{T_ALIGNAS _}
'alignof'	{T_ALIGNOF _}
'atomic'	{T_ATOMIC _}
'bool'		{T_BOOL _}
'complex'	{T_COMPLEX _}
'generic'	{T_GENERIC _}
'imaginary'	{T_IMAGINARY _}
'noreturn'	{T_NORETURN _}
'static_assert'	{T_STATIC_ASSERT _}
'thread_local'	{T_THREAD_LOCAL _}
'typeof'	{T_TYPEOF _}

'__builtin_va_arg'		{T_BUILTIN_VA_ARG _}
'__builtin_offsetof'		{T_BUILTIN_OFFSETOF _}
'__builtin_types_compatible_p'	{T_BUILTIN_TYPES_COMPATIBLE_P _}
'__builtin_convert_vector'	{T_BUILTIN_CONVERT_VECTOR _}

identifier_	{T_IDENTIFIER _ _}
integer_const	{T_INTEGER _ _ _}
string_const_	{T_STRING _ _}
char_const	{T_CHAR_LIT _ _}
typedef_name_    {T_TYPEDEF_NAME _ _}

'->'		{T_ARROW _}
'='		{T_ASSIGN _}
'&'		{T_BIT_AND _}
'&='		{T_BIT_AND_ASSIGN _}
'|'		{T_BIT_OR _}
'|='		{T_BIT_OR_ASSIGN _}
'}'		{T_CLOSE_CURLY _}
')'		{T_CLOSE_PAREN _}
']'		{T_CLOSE_SQUARE _}
':'		{T_COLON _}
','		{T_COMMA _}
'--'		{T_DEC _}
'/'		{T_DIV _}
'/='		{T_DIV_ASSIGN _}
'.'		{T_DOT _}
'...'		{T_ELIPSIS _}
'=='		{T_EQ _}
'>'		{T_GT _}
'>='		{T_GTE _}
'#'		{T_HASH _}
'##'		{T_HASH_HASH _}
'^'		{T_HAT _}
'^='		{T_HAT_ASSIGN _}
'++'		{T_INC _}
'&&'		{T_LOGICAL_AND _}
'||'		{T_LOGICAL_OR _}
'<<'		{T_LSHIFT _}
'<<='		{T_LSHIFT_ASSIGN _}
'<'		{T_LT _}
'<='		{T_LTE _}
'-'		{T_MINUS _}
'-='		{T_MINUS_ASSIGN _}
'%='		{T_MOD_ASSIGN _}
'%'		{T_MOD _}
'!='		{T_NEQ _}
'!'		{T_NOT _}
'{'		{T_OPEN_CURLY _}
'('		{T_OPEN_PAREN _}
'['		{T_OPEN_SQUARE _}
'+'		{T_PLUS _}
'+='		{T_PLUS_ASSIGN _}
'?'		{T_QUESTION _}
'>>'		{T_RSHIFT _}
'>>='		{T_RSHIFT_ASSIGN _}
';'		{T_SEMI _}
'*'		{T_STAR _}
'*='		{T_STAR_ASSIGN _}
'~'		{T_TILDE _}

%%

string_const :: {String}
    : string_const_			{unwrapString $1}
    | string_const string_const_	{$1 ++ (unwrapString $2)}

identifier :: {Identifier}
    : identifier_	{toIdentifier $1}

const_exp :: {Exp}
    : conditional_exp {$1}

field_assignment :: {(Identifier, Exp)}
    : '.' identifier '=' expression	{($2, $4)}

field_assignments :: {Reversed (Identifier, Exp)}
    : field_assignment				{rsingleton $1}
    | field_assignments ',' field_assignment	{rcons $3 $1}

field_assignments_opt :: {Reversed (Identifier, Exp)}
    : {- empty -}		{rempty}
    | field_assignments		{$1}

field_specifier :: {Reversed Identifier}
    : identifier			{rsingleton $1}
    | field_specifier '.' identifier	{rcons $3 $1}

primary_exp :: {Exp}
    : identifier		{VarExp $1}
    | integer_const		{ConstExp $1}
    | string_const		{StringConstExp $1}
    | char_const		{CharConstExp $ unwrapChar $1}
    | '(' expression ')'	{$2}

    | '__builtin_va_arg' '(' assignment_exp ',' type_name ')' {
        BuiltinVaArg
    }
    | '__builtin_offsetof' '(' type_name ',' field_specifier ')' {
        BuiltinOffsetOf
    }
    | '__builtin_types_compatible_p' '(' type_name ',' type_name ')' {
        BuiltinTypesCompatible 
    }

    | '__builtin_convert_vector' '(' assignment_exp ',' type_name ')' {
        BuiltinConvertVector
    }

{-
    | generic_selection		{$1}

generic_selection :: {Exp}
    : generic '(' assignment_exp ',' generic_assoc_list ')'	{}

generic_assoc_list
    : generic_association 	{}
    | generic_assoc-list ',' generic_association	{}

generic_association
    : type_name ':' assignment_exp		{}
    | default ':' assignment_exp			{}
-}

postfix_exp :: {Exp}
    : primary_exp		{$1}
    | postfix_exp '[' expression ']'	{SubscriptExp $1 $3}
    | postfix_exp '(' argument_exp_list ')'	{FuncallExp $1 $ unreverse $3}
    | postfix_exp '.' identifier	{StructElt $1 $3}
    | postfix_exp '->' identifier 	{StructDeref $1 $3}
    | postfix_exp '++'			{UnaryExp POST_INC $1}
    | postfix_exp '--'			{UnaryExp POST_DEC $1}
    | '(' type_name ')' '{' initializer_list_opt '}'	{CompoundLiteral $2 $ unreverse $5}
    | '(' type_name ')' '{' initializer_list ',' '}'	{CompoundLiteral $2 $ unreverse $5}
    | '(' compound_statement ')' 	{
        case $2 of
            (CompoundStatement labels items) -> BlockExp labels items
            _ -> error "not a block"  -- FIXME: parseError
        }

argument_exp_list :: {Reversed Exp}
    : {- empty -}	{Reversed []}
    | assignment_exp	{Reversed [$1]}
    | argument_exp_list ',' assignment_exp {rcons $3 $1}

unary_exp :: {Exp}
    : postfix_exp			{$1}
    | '++' unary_exp    		{UnaryExp PRE_INC $2}
    | '--' unary_exp    		{UnaryExp PRE_INC $2}
    | unary_operator cast_exp 		{UnaryExp $1 $2}
    | 'sizeof' unary_exp 		{SizeofValueExp $2}
    | 'sizeof' '(' type_name ')' 	{SizeofTypeExp $3}
    | 'alignof' '(' type_name ')' 	{AlignofExp $3}

unary_operator :: {UnaryOp}
    : '&&'	{LABEL_ADDRESS}
    | '&' 	{ADDRESS_OF}
    | '*' 	{DEREF}
    | '+' 	{UNARY_PLUS}
    | '-' 	{UNARY_MINUS}
    | '~' 	{BIT_NOT}
    | '!' 	{LOGICAL_NOT}

-- FIXME: casts aren't parsing
cast_exp :: {Exp}
    : unary_exp {$1}
    | '(' type_name ')' cast_exp {CastExp $2 $4}

multiplicative_exp :: {Exp}
    : cast_exp {$1}
    | multiplicative_exp '*' cast_exp {BinaryExp MULT $1 $3}
    | multiplicative_exp '/' cast_exp {BinaryExp DIV $1 $3}
    | multiplicative_exp '%' cast_exp {BinaryExp MOD $1 $3}

additive_exp :: {Exp}
    : multiplicative_exp {$1}
    | additive_exp '+' multiplicative_exp {BinaryExp PLUS $1 $3}
    | additive_exp '-' multiplicative_exp {BinaryExp MINUS $1 $3}

shift_exp :: {Exp}
    : additive_exp {$1}
    | shift_exp '<<' additive_exp {BinaryExp LSHIFT $1 $3}
    | shift_exp '>>' additive_exp {BinaryExp RSHIFT $1 $3}

relational_exp :: {Exp}
    : shift_exp {$1}
    | relational_exp '<' shift_exp {BinaryExp AST.LT $1 $3}
    | relational_exp '>' shift_exp {BinaryExp AST.GT $1 $3}
    | relational_exp '<=' shift_exp {BinaryExp LTE $1 $3}
    | relational_exp '>=' shift_exp {BinaryExp GTE $1 $3}

equality_exp :: {Exp}
    : relational_exp {$1}
    | equality_exp '==' relational_exp {BinaryExp AST.EQ $1 $3}
    | equality_exp '!=' relational_exp {BinaryExp NEQ $1 $3}

and_exp :: {Exp}
    : equality_exp {$1}
    | and_exp '&' equality_exp {BinaryExp BIT_AND $1 $3}

exclusive_or_exp :: {Exp}
    : and_exp {$1}
    | exclusive_or_exp '^' and_exp {BinaryExp XOR $1 $3}

inclusive_or_exp :: {Exp}
    : exclusive_or_exp {$1}
    | inclusive_or_exp '|' exclusive_or_exp {BinaryExp BIT_OR $1 $3}

logical_and_exp :: {Exp}
    : inclusive_or_exp {$1}
    | logical_and_exp '&&' inclusive_or_exp {BinaryExp LOGICAL_AND $1 $3}

logical_or_exp :: {Exp}
    : logical_and_exp {$1}
    | logical_or_exp '||' logical_and_exp {BinaryExp LOGICAL_OR $1 $3}

conditional_exp :: {Exp}
    : logical_or_exp {$1}
    | logical_or_exp '?' expression ':' conditional_exp {ConditionalExp $1 $3 $5}

assignment_exp :: {Exp}
    : conditional_exp {$1}
    | unary_exp assignment_op assignment_exp {AssignExp $2 $1 $3}

assignment_exp_opt :: {Maybe Exp}
    : {- empty -} 	{Nothing}
    | assignment_exp	{Just $1}
    
assignment_op :: {AssignOp}
    : '='	{ASSIGN}
    | '*='	{MULT_ASSIGN}
    | '/='	{DIV_ASSIGN}
    | '%='	{MOD_ASSIGN}
    | '+='	{PLUS_ASSIGN}
    | '-='	{MINUS_ASSIGN}
    | '<<='	{LSHIFT_ASSIGN}
    | '>>='	{RSHIFT_ASSIGN}
    | '&='	{AND_ASSIGN}
    | '^='	{XOR_ASSIGN}
    | '|='	{OR_ASSIGN}

expression :: {Exp}
    : assignment_exp {$1}
    | expression ',' assignment_exp {CommaExp $1 $3}

expression_opt :: {Maybe Exp}
    : {- empty -} 	{Nothing}
    | expression	{Just $1}

exps :: {Reversed Exp}
    : expression			{rsingleton $1}
    | exps ',' expression		{rcons $3 $1}

exps_opt :: {Reversed Exp}
    : {- empty -}	{rempty}
    | exps		{$1} 

----------------
-- Declarations
----------------

-- We use 1 token of look ahead, so have to make sure that maybeAddTypedef is
-- called before a possible typedef name is read.  So this 'internal' production
-- is really just 'declaration' without the trailing ';'.
declaration_internal :: {([DeclarationSpecifier], [InitDeclarator])}
    : declaration_specifiers init_declarator_list_opt {% do
        let inits = unreverse $2
        maybeAddTypedef $1 inits
        return ($1, inits)
    }

declaration :: {Declaration}
    : declaration_internal ';'		{Declaration (fst $1) (snd $1)}
    | static_assert_declaration		{$1}

declaration_specifiers_opt :: {[DeclarationSpecifier]}
    : {- empty -}			{[]}
    | declaration_specifiers		{$1}

declaration_specifiers :: {[DeclarationSpecifier]}
    : declaration_specifier declaration_specifiers_opt		{$1 : $2}

declaration_specifier :: {DeclarationSpecifier}
    : storage_class_specifier	{DSStorageClass $1}
    | type_specifier		{DSTypeSpecifier $1}
    | type_qualifier		{DSTypeQualifier $1}
    | function_specifier	{DSFunctionSpecifier $1}
    | alignment_specifier	{DSAlignmentSpecifier $1}
    | attr			{DSAttr $1}


init_declarator_list :: {Reversed InitDeclarator}
    : init_declarator				{Reversed [$1]}
    | init_declarator_list ',' init_declarator	{rcons $3 $1}

init_declarator_list_opt :: {Reversed InitDeclarator}
    : {- empty -}		{rempty}
    | init_declarator_list	{$1}

init_declarator :: {InitDeclarator}
    : declarator 			{InitDeclarator $1 Nothing}
    | declarator '=' initializer	{InitDeclarator $1 (Just $3)}
    | declarator asm_declarator		{InitDeclarator $1 Nothing}

asm_declarator :: {String}
    : 'asm' '(' string_const ')'	{$3}

storage_class_specifier :: {StorageClass}
    : 'typedef'		{Typedef}
    | 'extern'		{Extern}
    | 'static'		{Static}
    | 'thread_local' 	{ThreadLocal}
    | 'auto'		{Auto}
    | 'register'	{Register}

type_specifier :: {TypeSpecifier}
    : 'void'		{Void}
    | 'char'		{Char}
    | 'short'		{Short}
    | 'int'		{Int}
    | '__int128'	{Int128}
    | 'long'		{Long}
    | 'float'		{Float}
    | 'double'		{Double}
    | 'signed'		{Signed}
    | 'unsigned'	{Unsigned}
    | 'bool'		{Bool}
    | 'complex'		{Complex}
    | struct_or_union_specifier		{$1}
    | enum_specifier			{$1}
    | typedef_name			{TSTypedefName $1}
    | 'typeof' '(' expression ')'	{TSTypeofExp $3}
    | 'typeof' '(' declaration_specifiers abstract_declarator_opt ')'	{TSTypeofDecl $3} -- FIXME: need direct abstract declarator?

  {-
    | atomic_type_specifier		{$1}
   -}

struct_or_union_specifier :: {TypeSpecifier}
    : struct_or_union identifier '{' struct_declaration_list_opt '}' attrs_opt 	{% do
        insertStruct $2
        return $ StructOrUnionSpecifier $1 (Just $2) (Just $ unreverse $4)
    }
    | struct_or_union '{' struct_declaration_list_opt '}' attrs_opt 			{StructOrUnionSpecifier $1 Nothing (Just $ unreverse $3)}
    | struct_or_union identifier						{StructOrUnionSpecifier $1 (Just $2) Nothing}

identifier_opt :: {Maybe Identifier}
    : {- empty -} 	{Nothing}
    | identifier	{Just $1}

struct_or_union :: {StructType}
    : 'struct'	{Struct}
    | 'union'	{Union}

struct_declaration_list :: {Reversed StructDeclaration}
    : struct_declaration				{Reversed [$1]}
    | struct_declaration_list struct_declaration	{rcons $2 $1}

struct_declaration_list_opt :: {Reversed StructDeclaration}
    : {- empty -} 		{rempty}
    | struct_declaration_list	{$1}

struct_declaration :: {StructDeclaration}
    : specifier_qualifier_list struct_declarator_list_opt ';'	{StructDeclaration (unreverse $1) (unreverse $2)}
    | static_assert_declaration					{StructStaticAssert}

specifier_qualifier_list :: {Reversed SpecifierQualifier}
    : specifier_qualifier				{rsingleton $1}
    | specifier_qualifier_list specifier_qualifier	{rcons $2 $1}

specifier_qualifier_list_opt :: {Reversed SpecifierQualifier}
    : {- empty -}		{rempty}
    | specifier_qualifier_list	{$1}

specifier_qualifier :: {SpecifierQualifier}
    : type_specifier attrs_opt		{SQTypeSpecifier $1}
    | type_qualifier attrs_opt		{SQTypeQualifier $1}

struct_declarator_list :: {Reversed StructDeclarator}
    : struct_declarator					{Reversed [$1]}
    | struct_declarator_list ',' struct_declarator 	{rcons $3 $1}

struct_declarator_list_opt :: {Reversed StructDeclarator}
    : {- empty -}		{rempty}
    | struct_declarator_list	{$1}

struct_declarator :: {StructDeclarator}
    : declarator			{StructDeclarator $1 Nothing}
    | declarator ':' const_exp		{StructDeclarator $1 (Just $3)}
    | ':' const_exp			{StructDeclaratorNoDecl $2}

enum_specifier :: {TypeSpecifier}
    : 'enum' identifier_opt '{' enumerator_list '}'	{EnumDefSpecifier $2 $ unreverse $4}
    | 'enum' identifier_opt '{' enumerator_list ',' '}'	{EnumDefSpecifier $2 $ unreverse $4}
    | 'enum' identifier					{EnumRefSpecifier $2}

enumerator_list :: {Reversed Enumerator}
    : enumerator			{Reversed [$1]}
    | enumerator_list ',' enumerator	{rcons $3 $1}

enumerator :: {Enumerator}
    : enumeration_constant				{Enumerator $1 Nothing}
    | enumeration_constant '=' const_exp		{Enumerator $1 (Just $3)}

-- FIXME: not sure how an enumeration constant is different from 
-- an identifier.
enumeration_constant :: {Identifier}
    : identifier {$1}

{-
atomic_type_specifier
    : 'atomic' '(' type_name ')'
 -}

type_qualifier :: {TypeQualifier}
    : 'const'		{Const}
    | 'restrict'	{Restrict}
    | 'volatile'	{Volatile}
    | 'atomic'		{Atomic}

function_specifier :: {FunctionSpecifier}
    : 'inline'		{Inline}
    | 'noreturn'	{NoReturn}

alignment_specifier :: {AlignmentSpecifier}
    : 'alignas' '(' type_name ')'		{AlignAsType $3}
    | 'alignas' '(' const_exp ')'		{AlignAsConst $3}

declarator :: {Declarator}
    : pointer_opt direct_declarator attrs_opt	 {Declarator $1 $2}

declarator_opt :: {Maybe Declarator}
    : {- empty -}	{Nothing}
    | declarator	{Just $1}

direct_declarator :: {DirectDeclarator}
    : identifier		{DDIdentifier $1}
    | '(' declarator ')'	{DDNested $2}
    | direct_declarator '[' type_qualifier_list_opt assignment_exp_opt ']' 	{DDArray $1 (unreverse $3) $4 False False}
    | direct_declarator '[' 'static' type_qualifier_list_opt assignment_exp ']'	{DDArray $1 (unreverse $4) (Just $5) True False}
    | direct_declarator '[' type_qualifier_list 'static' assignment_exp ']'	{DDArray $1 (unreverse $3) (Just $5) True False}
    | direct_declarator '[' type_qualifier_list_opt '*' ']'			{DDArray $1 (unreverse $3) Nothing False True}
    | direct_declarator '(' parameter_type_list ')' attrs_opt			{DDFun $1 $3}
    | direct_declarator '(' identifier_list_opt ')' attrs_opt			{DDFunPtr $1 (unreverse $3)}

pointer :: {Pointer}
    : '*' type_qualifier_list_opt attrs_opt		{Pointer (unreverse $2) Nothing}
    | '*' type_qualifier_list_opt pointer attrs_opt	{Pointer (unreverse $2) (Just $3)}

pointer_opt ::	{Maybe Pointer}
    : {- empty -}	{Nothing}
    | pointer		{Just $1}

type_qualifier_list :: {Reversed TypeQualifier}
    : type_qualifier				{Reversed [$1]}
    | type_qualifier_list type_qualifier	{rcons $2 $1}

type_qualifier_list_opt :: {Reversed TypeQualifier}
    : {- empty -}		{rempty}
    | type_qualifier_list	{$1}

parameter_type_list :: {ParameterTypeList}
    : parameter_list			{ParameterTypeList (unreverse $1) False}
    | parameter_list ',' '...'		{ParameterTypeList (unreverse $1) True}

parameter_type_list_opt :: {Maybe ParameterTypeList}
    : {- empty -}		{Nothing}
    | parameter_type_list	{Just $1}

parameter_list :: {Reversed ParameterDeclaration}
    : parameter_declaration				{Reversed [$1]}
    | parameter_list ',' parameter_declaration		{rcons $3 $1}

parameter_declaration :: {ParameterDeclaration}
    : declaration_specifiers declarator			{PDDeclarator $1 $2}
    | declaration_specifiers abstract_declarator_opt	{PDAbstract $1 $2}

identifier_list	:: {Reversed Identifier}
    : identifier			{Reversed [$1]}
    | identifier_list ',' identifier	{rcons $3 $1}

identifier_list_opt :: {Reversed Identifier}
    : {- empty -} 	{rempty}
    | identifier_list 	{$1}

type_name :: {TypeName}
    : specifier_qualifier_list abstract_declarator_opt		{TypeName (unreverse $1) $2}

abstract_declarator :: {AbstractDeclarator}
    : pointer					{AbstractPointer $1}
    | pointer_opt direct_abstract_declarator	{AbstractDeclarator $1 $2}

abstract_declarator_opt :: {Maybe AbstractDeclarator}
    : {- empty -}		{Nothing}
    | abstract_declarator	{Just $1}

direct_abstract_declarator :: {DirectAbstractDeclarator}
    : '(' abstract_declarator ')'	{DANested $2}
    | direct_abstract_declarator_opt '[' type_qualifier_list_opt assignment_exp_opt ']'		{DAArray $1 (unreverse $3) $4 False}
    | direct_abstract_declarator_opt '[' 'static' type_qualifier_list_opt assignment_exp ']'	{DAArray $1 (unreverse $4) (Just $5) True}
    | direct_abstract_declarator_opt '[' type_qualifier_list 'static' assignment_exp ']'	{DAArray $1 (unreverse $3) (Just $5) False}
    | direct_abstract_declarator_opt '[' '*' ']'						{DAArrayStar $1}
    | direct_abstract_declarator_opt '(' parameter_type_list_opt ')'				{DAFun $1 $3}

direct_abstract_declarator_opt :: {Maybe DirectAbstractDeclarator}
    : {- empty -}			{Nothing}
    | direct_abstract_declarator	{Just $1}

typedef_name :: {Identifier}
    : typedef_name_ {case $1 of (T_TYPEDEF_NAME s pos) -> Identifier s}

initializer :: {Initializer}
    : assignment_exp			{InitAssign $1}
    | '{' initializer_list_opt '}'	{InitList (unreverse $2)}
    | '{' initializer_list ',' '}'	{InitList (unreverse $2)}

initializer_list :: {Reversed InitializerPair}
    : designation_opt initializer				{Reversed [InitializerPair $1 $2]}
    | initializer_list ',' designation_opt initializer		{rcons (InitializerPair $3 $4) $1}

initializer_list_opt :: {Reversed InitializerPair}
    : {- empty -}		{rempty}
    | initializer_list		{$1}

designation :: {[Designator]}
    : designator_list '='	{unreverse $1}

designation_opt :: {Maybe [Designator]}
    : {- empty -}		{Nothing}
    | designation		{Just $1}

designator_list :: {Reversed Designator}
    : designator			{Reversed [$1]}
    | designator_list designator	{rcons $2 $1}

designator :: {Designator}
    : '[' const_exp ']'			{SubscriptDesignator $2}
    | '.' identifier			{StructDesignator $2}

static_assert_declaration :: {Declaration}
    : 'static_assert' '(' const_exp ',' string_const ')' ';'		{
        StaticAssert
    }

-- Attr grammar cribbed from c-language lib
attr :: { [Attr] }
    : '__attribute__' '(' '(' attribute_list ')' ')'      { unreverse $4 }

attr_opt :: { Maybe [Attr] }
    : {- empty -}		{Nothing}
    | attr			{Just $1}

attrs :: { Reversed [Attr] }
    : attr		{Reversed [$1]}
    | attrs attr	{rcons $2 $1}

attrs_opt :: { Reversed [Attr] }
    : {- empty -}	{Reversed [[]]}
    | attrs		{$1}

attribute_list :: {Reversed Attr}
    : attribute                      { Reversed [$1] }
    | attribute_list ',' attribute   { rcons $3 $1 }

attribute :: {Attr}
    : {- empty -}				{Attr}
    | identifier                		{Attr}
    | 'const'                     		{Attr}
    | identifier '(' attribute_params ')' 	{Attr}
    | identifier '(' ')'        		{Attr} 

attribute_params :: {Reversed [Exp]}
    : const_exp							{rempty}
    | unary_exp assignment_op unary_exp				{rempty}
    | attribute_params ',' const_exp				{rempty}
    | attribute_params ',' unary_exp assignment_op unary_exp 	{rempty}

--------------
-- Statements
--------------

statement :: {Statement}
    : labelled_statement	{$1}
    | compound_statement	{$1}
    | expression_statement	{$1}
    | selection_statement	{$1}
    | iteration_statement	{$1}
    | jump_statement		{$1}
    | asm ';'			{AsmStatement}

labelled_statement :: {Statement}
    : identifier ':' statement				{LabelStatement $1 $3}
    | 'case' const_exp ':' statement			{CaseStatement $2 Nothing $4}
    | 'case' const_exp '...' const_exp ':' statement	{CaseStatement $2 (Just $4) $6}
    | 'default' ':' statement				{DefaultStatement $3}

label_declaration :: {[Identifier]}
    : '__label__' identifier_list ';'		{unreverse $2}

label_declarations :: {[Identifier]}
    : label_declaration				{$1}
    | label_declarations label_declaration	{$1 ++ $2}

compound_statement :: {Statement}
    : '{' block_item_list_opt '}'			{CompoundStatement [] $ unreverse $2}
    | '{' label_declarations block_item_list_opt '}'	{CompoundStatement $2 (unreverse $3)}

block_item_list :: {Reversed BlockItem}
    : block_item			{rcons $1 rempty}
    | block_item_list block_item	{rcons $2 $1}

block_item_list_opt :: {Reversed BlockItem}
    : {- empty -}		{rempty}
    | block_item_list		{$1}

block_item :: {BlockItem}
    : declaration	{BIDeclaration $1}
    | statement		{BIStatement $1}

expression_statement :: {Statement}
    : expression ';'	{ExpressionStatement $1}
    | ';'		{EmptyStatement}

selection_statement :: {Statement}
    : 'if' '(' expression ')' statement				{IfStatement $3 $5 Nothing}
    | 'if' '(' expression ')' statement 'else' statement	{IfStatement $3 $5 (Just $7)}
    | 'switch' '(' expression ')' statement			{SwitchStatement $3 $5}

iteration_statement :: {Statement}
    : 'while' '(' expression ')' statement			{WhileStatement $3 $5}
    | 'do' statement 'while' '(' expression ')' ';'		{DoStatement $2 $5}
    | 'for' '(' expression_opt ';' expression_opt ';' expression_opt ')' statement		{ForStatement Nothing $3 $5 $7 $9}
    | 'for' '(' declaration expression_opt ';' expression_opt ';' expression_opt ')' statement	{ForStatement (Just $3) $4 $6 $8 $10}

jump_statement :: {Statement}
    : 'goto' identifier ';'		{GotoStatement $2}
    | 'continue' ';'			{ContinueStatement}
    | 'break' ';'			{BreakStatement}
    | 'return' expression_opt ';'	{ReturnStatement $2}

asm_basic :: {Asm}
    : 'asm' asm_basic_qualifiers '(' asm_instructions ')'	{Asm}

asm :: {Asm}
    : 'asm' asm_qualifiers '(' string_const ')' {
        Asm
    }
    | 'asm' asm_qualifiers '(' string_const ':' asm_operands ')' {
        Asm
    }
    | 'asm' asm_qualifiers '(' string_const ':' asm_operands_opt ':' asm_operands ')' {
        Asm
    }
    | 'asm' asm_qualifiers '(' string_const ':' asm_operands_opt ':' asm_operands_opt ':' asm_clobbers ')' {
        Asm
    }
    | 'asm' asm_qualifiers '(' string_const ':' asm_operands_opt ':' asm_operands_opt ':' asm_clobbers_opt ':' asm_gotos ')' {
        Asm
    }

asm_operand :: {AsmOperand}
    : string_const '(' expression ')'	{AsmOperand $1 $3}
    | '[' identifier ']' string_const '(' expression ')'	{AsmOperand $4 $6}

asm_operands :: {Reversed AsmOperand}
    : asm_operand			{Reversed [$1]}
    | asm_operands ',' asm_operand	{rcons $3 $1}

asm_operands_opt :: {Reversed AsmOperand}
    : {- empty -}		{rempty}
    | asm_operands		{$1}

asm_clobbers :: {Reversed String}
    : string_const			{Reversed [$1]}
    | asm_clobbers ',' string_const	{rcons $3 $1}

asm_clobbers_opt :: {Reversed String}
    : {- empty -}		{rempty}
    | asm_clobbers		{$1}

asm_gotos :: {Reversed Identifier}
    : identifier			{Reversed [$1]}
    | asm_gotos ',' identifier 		{rcons $3 $1}

asm_basic_qualifiers :: {Reversed AsmQualifier}
    : {- empty -}					{rempty}
    | asm_basic_qualifiers asm_basic_qualifier		{rcons $2 $1}

asm_basic_qualifier :: {AsmQualifier}
    : 'inline'		{AsmInline}
    | 'volatile'	{AsmVolatile}

asm_qualifiers :: {Reversed AsmQualifier}
    : {- empty -}			{rempty}
    | asm_qualifiers asm_qualifier	{rcons $2 $1}

asm_qualifier :: {AsmQualifier}
    : 'inline'		{AsmInline}
    | 'volatile'	{AsmVolatile}
    | 'goto'		{AsmGoto}

asm_instructions :: {String}
    : string_const	{$1}

------------------------
-- External definitions
------------------------

translation_unit :: {TranslationUnit}
    : external_declarations			{TranslationUnit $1}

external_declarations :: {[ExternalDeclaration]}
    : {- empty -}					{[]}
    | external_declarations external_declaration	{$1 ++ $2}

external_declaration :: {[ExternalDeclaration]}
    : function_definition	{[$1]}
    | declaration		{[ExternalDeclaration $1]}
    | asm_basic ';'		{[AsmDeclaration $1]}
    | ';'			{[]}

function_definition :: {ExternalDeclaration}
    : declaration_specifiers declarator declaration_list compound_statement	{FunDef $1 $2 (unreverse $3) $4}

declaration_list :: {Reversed Declaration}
    : {- empty -}			{rempty}
    | declaration_list declaration	{rcons $2 $1}

{
parseError :: Token AlexPosn -> Alex a
parseError t = alexError $ show t

{-
"Parse error at line " ++ (show line) ++ ":" ++ (show col)
    where
        (AlexPn b line col) = getAttr t
-}

data Reversed a = Reversed [a]

rempty :: Reversed a
rempty = Reversed []

rsingleton :: a -> Reversed a
rsingleton x = Reversed [x]

rcons :: a -> Reversed a -> Reversed a
rcons x (Reversed xs) = Reversed (x:xs)

unreverse :: Reversed a -> [a]
unreverse (Reversed xs) = reverse xs

toIdentifier :: Token AlexPosn -> Identifier
toIdentifier (T_IDENTIFIER n _) = Identifier n
toIdentifier _ = error "not an identifier"

-- (Declaration [DSStorageClass Typedef,DSTypeSpecifier Int] [InitDeclarator (Declarator Nothing (DDIdentifier (Identifier "foo"))) N othing])
maybeAddTypedef :: [DeclarationSpecifier] -> [InitDeclarator] -> Alex ()
maybeAddTypedef ((DSStorageClass Typedef):_) [InitDeclarator (Declarator _ dd) Nothing] =
    insertTypedef $ extractTypedefName dd
maybeAddTypedef x y = return ()

extractTypedefName :: DirectDeclarator -> Identifier
extractTypedefName (DDIdentifier nm) = nm
extractTypedefName (DDNested (Declarator _ dd)) = extractTypedefName dd
extractTypedefName (DDArray dd _ _ _ _) = extractTypedefName dd
extractTypedefName (DDFun dd _) = extractTypedefName dd
extractTypedefName (DDFunPtr dd _) = extractTypedefName dd

unwrapString :: Token AlexPosn -> String
unwrapString (T_STRING s _) = s
unwrapString _ = error "not a string"

unwrapChar :: Token AlexPosn -> String
unwrapChar (T_CHAR_LIT s _) = s
unwrapChar _ = error "not a char"

traceIt x = trace (show x) x
}
