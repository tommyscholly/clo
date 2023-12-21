%token <float> NUMBER
%token <string> IDENT
%token DEF
%token EXTERN
%token ADD
%token MUL
%token EOF
%token LPAREN
%token RPAREN
%token COMMA
%token END

%left ADD
%left MUL

%start <Ast.expr option> prog
%%

prog:
    | v = expr;  EOF { Some v }
    | EOF { None }
    ;

expr: 
    | i = NUMBER { Number i }
    | id = IDENT { Variable id }
    | e1 = expr; ADD; e2 = expr { Binop (Add, e1, e2) }
    | e1 = expr; MUL; e2 = expr { Binop (Mul, e1, e2) }
    | DEF; name = IDENT; LPAREN; args = separated_list(COMMA, IDENT); RPAREN; e = expr; END { Function ( Prototype (name, args), e) }
    | id = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN; { Call (id, args) }
