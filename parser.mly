
%{
    open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> IDENT
%token PLUS MINUS TIMES OVER
%token LPAREN RPAREN
%token EQEQ
%token LET EQUALS IN
%token IF THEN ELSE
%token EOF

%left EQEQ
%left PLUS MINUS
%left TIMES OVER

%start <Ast.expr> main

%%

main:
    | e = expr; EOF { e }
    
expr:
    | n = INT 
        { Value (Int n) }
    | n = FLOAT
        { Value (Float n) }
    | b = BOOL
        { Value (Bool b) }
    | x = IDENT
        { Var x }
    | left = expr; PLUS; right = expr
        { BinOp (Add, left, right) }
    | left = expr; MINUS; right = expr
        { BinOp (Sub, left, right) }
    | left = expr; TIMES; right = expr
        { BinOp (Mul, left, right) }
    | left = expr; OVER; right = expr
        { BinOp (Div, left, right) }
    | left = expr; EQEQ; right = expr
        { BinOp (Equals, left, right) }
    | LPAREN; content = expr; RPAREN
        { content }
    | LET; name = IDENT; EQUALS; value = expr; IN; body = expr
        { Let (name, value, body) }
    | IF; cond = expr; THEN; then_expr = expr; ELSE; other_expr = expr
        { IfElse (cond, then_expr, other_expr)}
