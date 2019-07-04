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

identifier	{T_IDENTIFIER _}
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

-- FIXME: finish
type_name :: {Type}
    : identifier	{Void}

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

expression :: { Exp }
    : assignment_exp {$1}
    | expression ',' assignment_exp {CommaExp $1 $3}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Reversed a = Reversed [a]

rcons :: a -> Reversed a -> Reversed a
rcons x (Reversed xs) = Reversed (x:xs)

unreverse :: Reversed a -> [a]
unreverse (Reversed xs) = reverse xs
}
