{
module Parser (
    expression,
    postfix_exp,
    unary_exp,
    cast_exp,
    additive_exp,
    conditional_exp
    ) where

import AST
import Control.Monad
import Token
}

%name expression expression
%name postfix_exp postfix_exp
%name unary_exp unary_exp
%name cast_exp cast_exp
%name additive_exp additive_exp
%name conditional_exp conditional_exp

%tokentype {Token}
%error { parseError }

%token

auto		{T_AUTO}
break		{T_BREAK}
case		{T_CASE}
char		{T_CHAR}
const		{T_CONST}
continue	{T_CONTINUE}
default		{T_DEFAULT}
do		{T_DO}
double		{T_DOUBLE}
else		{T_ELSE}
enum		{T_ENUM}
extern		{T_EXTERN}
float		{T_FLOAT}
for		{T_FOR}
goto		{T_GOTO}
if		{T_IF}
inline		{T_INLINE}
int		{T_INT}
long		{T_LONG}
register	{T_REGISTER}
restrict	{T_RESTRICT}
return		{T_RETURN}
short		{T_SHORT}
signed		{T_SIGNED}
sizeof		{T_SIZEOF}
static		{T_STATIC}
struct		{T_STRUCT}
switch		{T_SWITCH}
typedef		{T_TYPEDEF}
union		{T_UNION}
unsigned	{T_UNSIGNED}
void		{T_VOID}
volatile	{T_VOLATILE}
while		{T_WHILE}
alignas		{T_ALIGNAS}
alignof		{T_ALIGNOF}
atomic		{T_ATOMIC}
bool		{T_BOOL}
complex		{T_COMPLEX}
generic		{T_GENERIC}
imaginary	{T_IMAGINARY}
noreturn	{T_NORETURN}
static_assert	{T_STATIC_ASSERT}
thread_local	{T_THREAD_LOCAL}

identifier_	{T_IDENTIFIER _}
integer_const	{T_INTEGER _ _}
string_const	{T_STRING _}

'->'		{T_ARROW}
'='		{T_ASSIGN}
'&'		{T_BIT_AND}
'&='		{T_BIT_AND_ASSIGN}
'|'		{T_BIT_OR}
'|='		{T_BIT_OR_ASSIGN}
'}'		{T_CLOSE_CURLY}
')'		{T_CLOSE_PAREN}
']'		{T_CLOSE_SQUARE}
':'		{T_COLON}
','		{T_COMMA}
'--'		{T_DEC}
'/'		{T_DIV}
'/='		{T_DIV_ASSIGN}
'.'		{T_DOT}
'...'		{T_ELIPSIS}
'=='		{T_EQ}
'>'		{T_GT}
'>='		{T_GTE}
'#'		{T_HASH}
'##'		{T_HASH_HASH}
'^'		{T_HAT}
'^='		{T_HAT_ASSIGN}
'++'		{T_INC}
'&&'		{T_LOGICAL_AND}
'||'		{T_LOGICAL_OR}
'<<'		{T_LSHIFT}
'<<='		{T_LSHIFT_ASSIGN}
'<'		{T_LT}
'<='		{T_LTE}
'-'		{T_MINUS}
'-='		{T_MINUS_ASSIGN}
'%='		{T_MOD_ASSIGN}
'%'		{T_MOD}
'!='		{T_NEQ}
'!'		{T_NOT}
'{'		{T_OPEN_CURLY}
'('		{T_OPEN_PAREN}
'['		{T_OPEN_SQUARE}
'+'		{T_PLUS}
'+='		{T_PLUS_ASSIGN}
'?'		{T_QUESTION}
'>>'		{T_RSHIFT}
'>>='		{T_RSHIFT_ASSIGN}
';'		{T_SEMI}
'*'		{T_STAR}
'*='		{T_STAR_ASSIGN}
'~'		{T_TILDE}

%%

identifier :: {Identifier}
    : identifier_	{toIdentifier $1}

const_exp :: {Exp}
    : integer_const	{ConstExp $1}

primary_exp :: {Exp}
    : identifier		{VarExp $1}
    | integer_const		{ConstExp $1}
    | string_const		{StringConstExp $1}
    | '(' expression ')'		{$2}

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

{-
    | '(' type_name ')' '{' initializer_list '}'	{}
    | '(' type_name ')' '{' initializer_list ',' '}'	{}
-}

argument_exp_list :: {Reversed Exp}
    : {- empty -}	{Reversed []}
    | assignment_exp	{Reversed [$1]}
    | argument_exp_list ',' assignment_exp {rcons $3 $1}

unary_exp :: {Exp}
    : postfix_exp	{$1}
    | '++' unary_exp    {UnaryExp PRE_INC $2}
    | '--' unary_exp    {UnaryExp PRE_INC $2}
    | unary_operator cast_exp {UnaryExp $1 $2}
    | sizeof unary_exp 	{SizeofValueExp $2}
    | sizeof '(' type_name ')' {SizeofTypeExp $3}

  {-
    | alignof '(' type_name ')' {AlignofExp $3}
   -}

unary_operator :: {UnaryOp}
    : '&' {ADDRESS_OF}
    | '*' {DEREF}
    | '+' {UNARY_PLUS}
    | '-' {UNARY_MINUS}
    | '~' {BIT_NOT}
    | '!' {LOGICAL_NOT}

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

----------------
-- Declarations
----------------

declaration :: {Declaration}
    : declaration_specifiers init_declarator_list ';' {Declaration $1 (unreverse $2)}

{-
    | static_assert_declaration
-}

declaration_specifiers_star :: {Reversed DeclarationSpecifier}
    : {- empty -}		{rempty}
    | declaration_specifiers_star declaration_specifier		{rcons $2 $1}

declaration_specifiers :: {[DeclarationSpecifier]}
    : declaration_specifier declaration_specifiers_star	{$1 : unreverse $2}

declaration_specifier :: {DeclarationSpecifier}
    : storage_class_specifier	{DSStorageClass $1}
    | type_specifier		{DSTypeSpecifier $1}
    | type_qualifier		{DSTypeQualifier $1}
    | function_specifier	{DSFunctionSpecifier $1}
    | alignment_specifier	{DSAlignmentSpecifier $1}

init_declarator_list :: {Reversed InitDeclarator}
    : {- empty -}		{rempty}
    | init_declarator_list ',' init_declarator	{rcons $3 $1}

init_declarator :: {InitDeclarator}
    : declarator			{InitDeclarator $1 Nothing}
    | declarator '=' initializer	{InitDeclarator $1 (Just $3)}

storage_class_specifier :: {StorageClass}
    : typedef		{Typedef}
    | extern		{Extern}
    | static		{Static}
    | thread_local 	{ThreadLocal}
    | auto		{Auto}
    | register		{Register}

type_specifier :: {TypeSpecifier}
    : void		{Void}
    | char		{Char}
    | short		{Short}
    | int		{Int}
    | long		{Long}
    | float		{Float}
    | double		{Double}
    | signed		{Signed}
    | unsigned		{Unsigned}
    | bool		{Bool}
    | complex		{Complex}
    | struct_or_union_specifier		{$1}
    | enum_specifier			{$1}
    | typedef_name			{TSTypedefName $1}

  {-
    | atomic_type_specifier		{$1}
   -}


struct_or_union_specifier :: {TypeSpecifier}
    : struct_or_union identifier_opt '{' struct_declaration_list '}' 	{StructOrUnionSpecifier $1 $2 (Just $ unreverse $4)}
    | struct_or_union identifier					{StructOrUnionSpecifier $1 (Just $2) Nothing}

identifier_opt :: {Maybe Identifier}
    : {- empty -} 	{Nothing}
    | identifier	{Just $1}

struct_or_union :: {StructType}
    : struct	{Struct}
    | union	{Union}

struct_declaration_list :: {Reversed StructDeclaration}
    : struct_declaration				{Reversed [$1]}
    | struct_declaration_list struct_declaration	{rcons $2 $1}

struct_declaration :: {StructDeclaration}
    : specifier_qualifier_list struct_declarator_list_star ';'	{StructDeclaration (unreverse $1) (unreverse $2)}

{-
    | static_assert_declaration
-}

specifier_qualifier_list :: {Reversed SpecifierQualifier}
    : type_specifier specifier_qualifier_list_star	{rcons (SQTypeSpecifier $1) $2}
    | type_qualifier specifier_qualifier_list_star	{rcons (SQTypeQualifier $1) $2}

specifier_qualifier_list_star :: {Reversed SpecifierQualifier}
    : {- empty -}	{Reversed []}
    | specifier_qualifier_list	{$1}

struct_declarator_list :: {Reversed StructDeclarator}
    : struct_declarator					{Reversed [$1]}
    | struct_declarator_list ',' struct_declarator 	{rcons $3 $1}

struct_declarator_list_star :: {Reversed StructDeclarator}
    : {- empty -}		{rempty}
    | struct_declarator_list	{$1}

struct_declarator :: {StructDeclarator}
    : declarator			{StructDeclarator $1 Nothing}
    | declarator ':' const_exp		{StructDeclarator $1 (Just $3)}
    | ':' const_exp			{StructDeclaratorNoDecl $2}

enum_specifier :: {TypeSpecifier}
    : enum identifier_opt '{' enumerator_list '}'	{EnumDefSpecifier $2 $ unreverse $4}
    | enum identifier_opt '{' enumerator_list ',' '}'	{EnumDefSpecifier $2 $ unreverse $4}
    | enum identifier					{EnumRefSpecifier $2}

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
    : atomic '(' type_name ')'
 -}

type_qualifier :: {TypeQualifier}
    : const	{Const}
    | restrict	{Restrict}
    | volatile	{Volatile}
    | atomic	{Atomic}

function_specifier :: {FunctionSpecifier}
    : inline	{Inline}
    | noreturn	{NoReturn}

alignment_specifier :: {AlignmentSpecifier}
    : alignas '(' type_name ')'		{AlignAsType $3}
    | alignas '(' const_exp ')'		{AlignAsConst $3}

declarator :: {Declarator}
    : pointer_opt direct_declarator	 {Declarator $1 $2}

declarator_opt :: {Maybe Declarator}
    : {- empty -}	{Nothing}
    | declarator	{Just $1}

direct_declarator :: {DirectDeclarator}
    : identifier		{DDIdentifier $1}
    | '(' declarator ')'	{DDNested $2}
    | direct_declarator '[' type_qualifier_list_star assignment_exp_opt ']' 	{DDArray $1 (unreverse $3) $4 False False}
    | direct_declarator '[' static type_qualifier_list_star assignment_exp ']'	{DDArray $1 (unreverse $4) (Just $5) True False}
    | direct_declarator '[' type_qualifier_list static assignment_exp ']'	{DDArray $1 (unreverse $3) (Just $5) True False}
    | direct_declarator '[' type_qualifier_list_star '*' ']'			{DDArray $1 (unreverse $3) Nothing False True}
    | direct_declarator '(' parameter_type_list ')'				{DDFun $1 $3}
    | direct_declarator '(' identifier_list_star ')'				{DDFunOdd $1 (unreverse $3)}

pointer :: {Pointer}
    : '*' type_qualifier_list_star		{Pointer (unreverse $2) Nothing}
    | '*' type_qualifier_list_star pointer	{Pointer (unreverse $2) (Just $3)}

pointer_opt ::	{Maybe Pointer}
    : {- empty -}	{Nothing}
    | pointer		{Just $1}

type_qualifier_list :: {Reversed TypeQualifier}
    : type_qualifier				{Reversed [$1]}
    | type_qualifier_list type_qualifier	{rcons $2 $1}

type_qualifier_list_star :: {Reversed TypeQualifier}
    : {- empty -}		{Reversed []}
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

identifier_list_star :: {Reversed Identifier}
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
    | direct_abstract_declarator_opt '[' type_qualifier_list_star assignment_exp_opt ']'		{DAArray $1 (unreverse $3) $4 False}
    | direct_abstract_declarator_opt '[' static type_qualifier_list_star assignment_exp ']'		{DAArray $1 (unreverse $4) (Just $5) True}
    | direct_abstract_declarator_opt '[' type_qualifier_list static assignment_exp ']'			{DAArray $1 (unreverse $3) (Just $5) False}
    | direct_abstract_declarator_opt '[' '*' ']'							{DAArrayStar $1}
    | direct_abstract_declarator_opt '(' parameter_type_list_opt ')'					{DAFun $1 $3}

direct_abstract_declarator_opt :: {Maybe DirectAbstractDeclarator}
    : {- empty -}			{Nothing}
    | direct_abstract_declarator	{Just $1}

typedef_name :: {Identifier}
    : identifier	{$1}

initializer :: {Initializer}
    : assignment_exp			{InitAssign $1}
    | '{' initializer_list '}'		{InitList (unreverse $2)}
    | '{' initializer_list ',' '}'	{InitList (unreverse $2)}

initializer_list :: {Reversed InitializerPair}
    : designation_opt initializer				{Reversed [InitializerPair $1 $2]}
    | initializer_list ',' designation_opt initializer		{rcons (InitializerPair $3 $4) $1}

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

  {-
static_assert_declaration
    : static_assert '(' const_exp ',' string_const ')' ';'
   -}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Reversed a = Reversed [a]

rempty :: Reversed a
rempty = Reversed []

rcons :: a -> Reversed a -> Reversed a
rcons x (Reversed xs) = Reversed (x:xs)

unreverse :: Reversed a -> [a]
unreverse (Reversed xs) = reverse xs

toIdentifier :: Token -> Identifier
toIdentifier (T_IDENTIFIER n) = Identifier n
toIdentifier _ = error "not an identifier"
}
