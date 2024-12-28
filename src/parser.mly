%{
  open Ast
%}

%token <string> Id
%token <string> String
%token <string> Decimal
%token <string> Binary
%token <string> Octal
%token <string> Hexa
%token <string> FloatDecimal
%token <bool> Boolean
%token <Ast.unaire> Typeof Plus Minus
%token <Ast.cte> Cte
%token <Ast.primType> Number BooleanType StringType
%token Let Const Equals Type Var Any EOF
%token Pow Mult Div Add Sub
%token Blt Ble Gt Ge Eq Diff StrictEq StrictDiff And Or
%token Semicolon LPar RPar Colon LBrace RBrace LBracket RBracket Pipe Comma Dot
%token If While Return Else

%left Or And
%left Eq Diff StrictEq StrictDiff Equals
%left Blt Ble Gt Ge
%left Add Sub
%left Mult Div
%right Pow
%right LPar
%left Dot LBracket

%nonassoc RPar
%nonassoc Else

%nonassoc LowerThanBop
%nonassoc LowerThanAssign

%start file
%type <Ast.file> file
%type <Ast.declaration> declaration
%type <Ast.instruction> instruction
%type <Ast.type_> type_ type__
%type <Ast.type_ list> type_union
%type <Ast.unaire> uop
%type <Ast.bop> bop
%type <Ast.expression> expression
%type <Ast.cond> cond
%type <Ast.primType> primType
%type <Ast.cte> cte
%type <Ast.left_Member> left_member
%type <(Ast.id * Ast.type_ option) list> objectType_list
%type <Ast.id * Ast.type_ option> objectType
%type <(Ast.id * Ast.expression) list> objectContent_list
%type <Ast.binding list> binding_list
%type <Ast.binding> binding
%type <Ast.body list> body_list
%type <Ast.body> body
%type <Ast.expression list> expression_list

%%

file:
    body_list EOF { $1 }

body_list:
  | body { [$1] }
  | body body_list { $2 @ [$1] }

body:
  | declaration { Declaration $1 }
  | instruction { Instr $1 }

declaration:
  | Type Id Equals type_ Semicolon { Alias ($2, $4) }
  | Let binding_list Semicolon { LocalLet $2 }
  | Const binding_list Semicolon { LocalConst $2 }
  | Id LPar binding_list RPar Colon type_ LBrace body_list RBrace { Function ($1, $3, Some $6, $8)}
  | Id LPar binding_list RPar LBrace body_list RBrace { Function ($1, $3, None, $6) }

binding_list:
  | binding { [$1] }
  | binding Comma binding_list { $3 @ [$1] }

binding:
  | Id Colon type_ Equals expression { Binding ($1, Some $3, Some $5) }
  | Id Colon type_ { Binding ($1, Some $3, None) }
  | Id Equals expression { Binding ($1, None, Some $3) }
  | Id { Binding ($1, None, None) }

instruction:
  | Semicolon { Empty }
  | expression Semicolon { Expression $1 }
  | LBrace body_list RBrace { Block $2 }
  | Var binding_list { VarDeclaration $2 }
  | If LPar expression RPar instruction Else instruction { If ($3, $5, Some $7) }
  | If LPar expression RPar instruction { If ($3, $5, None) }
  | While LPar expression RPar instruction { While ($3, $5) }
  | Return expression Semicolon { Return (Some $2) }
  | Return Semicolon { Return None }

type_:
  | type__ { $1 }
  | type_union { Union $1 }

type_union:
  | type__ Pipe type_union { $1::$3 }

type__: /* Utilisé pour résoudre le problème de grammaire cyclique avec Union */
  | Id { IdT $1 }
  | Cte { Cte $1 }
  | primType { PrimitiveType $1 }
  | type__ LBracket RBracket { Array $1 }
  | Any { Any } /* TODO : voir comment fonctionne Any */
  | LBrace objectType_list RBrace { Object $2 }


objectType_list:
  | objectType { [$1] }
  | objectType Semicolon objectType_list { $3 @ [$1] }
  | objectType Comma objectType_list { $3 @ [$1] }

objectType:
  | Id Colon type_ { ($1, Some $3) }
  | Id { ($1, None) }

primType:
  | Number { Number }
  | BooleanType { Boolean }
  | StringType { String }

uop:
  | Typeof { Typeof }
  | Plus { Plus }
  | Minus { Minus }

bop:
  | Pow { Pow }
  | Mult { Mult }
  | Div { Div }
  | Add { Add }
  | Sub { Sub }

cond:
  | Blt { Blt }
  | Ble { Ble }
  | Gt { Gt }
  | Ge { Ge }
  | Eq { Eq }
  | Diff { Diff }
  | StrictEq { StrictEq }
  | StrictDiff { StrictDiff }

expression:
  | LPar expression RPar { Par $2 }
  | cte { Cte $1 }
  | left_member { Left $1 }
  | LBrace objectContent_list RBrace { Object $2 }
  | LBracket expression_list RBracket { Array $2 }
  | expression LPar expression_list RPar { Function ($1, $3) }
  | uop expression { Unaire ($1, $2) } %prec LowerThanAssign
  | expression bop expression { Bop ($1, $2, $3) } %prec LowerThanBop
  | expression cond expression { Cond ($1, $2, $3) } %prec LowerThanBop
  | expression And expression { And ($1, $3) }
  | expression Or expression { Or ($1, $3) }
  | left_member Equals expression { Affectation ($1, $3) } %prec LowerThanAssign

left_member:
  | Id { IdLM $1 } %prec LPar
  | expression LBracket expression RBracket { Table ($1, $3) }
  | expression Dot Id { SubElement ($1, $3) }

expression_list:
  | expression { [$1] }
  | expression Comma { [$1] }
  | expression Comma expression_list { $3 @ [$1] }

objectContent_list:
  | Id Colon expression { [($1, $3)] }
  | Id Colon expression Comma { [($1, $3)] }
  | Id Colon expression Comma objectContent_list { $5 @ [($1, $3)] }
  | Id Colon expression Semicolon objectContent_list { $5 @ [($1, $3)] }

cte:
  | Decimal { Decimal $1 }
  | Binary { Binary $1 }
  | Octal { Octal $1 }
  | Hexa { Hexa $1 }
  | FloatDecimal { FloatDecimal $1 }
  | Boolean { Boolean $1 }
  | String { String $1 }