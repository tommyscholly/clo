%{
  open Ast
%}

%token <float> FLOAT
%token <int> INT
%token <string> IDENT
%token <string> STRING
%token FN
%token LET
%token PRINT
%token PLUS
%token MUL
%token EOF
%token TRUE
%token FALSE
%token RETURN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token ARROW
%token COLON
%token SEMICOLON
%token EQUAL
%token DOT

%token TYPE

%token TYPE_INT 
%token TYPE_BOOL
%token TYPE_VOID

%left PLUS
%left MUL

%start <Ast.expr list> prog

%%

prog:
    | es = list(expr);  EOF { es }
    ;

type_expr:
    | TYPE_INT { TInt }
    | TYPE_BOOL { TBool }
    | TYPE_VOID { TVoid }
    | custom=IDENT; { TCustom custom }
    ;

type_annotation:
    | COLON; annot=type_expr { annot }
    ;

param:
    | param_name=IDENT; param_type=type_annotation { param_name, param_type, ($startpos, $endpos) }
    ;

params:
    | LPAREN; params=separated_list(COMMA,param); RPAREN { params }
    ;

args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN { args }
    ;

semi_terminated:
    | e=expr; SEMICOLON { e }
    ;

block_expr:
    | LBRACE; exprs=list(semi_terminated); RBRACE { exprs }
    ;

return_type_expr:
    | ARROW; ret_type=type_expr { ret_type }
    ;

function_defn:
    | FN; name=IDENT; function_params=params; ret_type = option(return_type_expr); body=block_expr { Function ((name, function_params, ret_type, body), ($startpos, $endpos)) }
    ;

struct_field:
    | name=IDENT; COLON; ty=type_expr; SEMICOLON { Field (name, ty, ($startpos, $endpos)) }
    ;

struct_expr:
    | LBRACE; fields=list(struct_field); RBRACE { fields }

type_defn:
    | TYPE; name=IDENT; body=struct_expr { Struct (name, body, ($startpos, $endpos)) }
    ;

struct_construct_field:
    | name=IDENT; COLON; e=expr; SEMICOLON { (name, e, ($startpos, $endpos)) }
    ;

struct_construct:
    | name=IDENT; LBRACE; fields=list(struct_construct_field); RBRACE { StructConstruct (name, fields, ($startpos, $endpos))}
    ;

expr:
    | LPAREN; e=expr RPAREN {e}
    | lhs=expr; op=bin_op; rhs=expr { Binop (op, lhs, rhs, ($startpos, $endpos)) }
    | LET; id=IDENT; annot=option(type_annotation); EQUAL; e=expr { Let (id, annot, e, ($startpos, $endpos)) }
    | fn=IDENT; fn_args=args { Call (fn, fn_args, ($startpos, $endpos)) }
    | PRINT; LPAREN; str=STRING; option(COMMA); args=separated_list(COMMA, expr); RPAREN; {  Print(str, args, ($startpos, $endpos)) }
    | fn_def=function_defn { fn_def }
    | type_def=type_defn { type_def }
    | sc=struct_construct { sc }
    | id=IDENT { Variable (id, ($startpos, $endpos)) }
    | i=INT { Int i }
    | f=FLOAT { Float f }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | RETURN; e=expr { Return (e, ($startpos, $endpos)) }
    | struct_name=IDENT; DOT; field_name=IDENT { FieldAccess (struct_name, field_name, ($startpos, $endpos)) }
    ;


%inline bin_op:
| PLUS { Plus }
| MUL { Mul }
