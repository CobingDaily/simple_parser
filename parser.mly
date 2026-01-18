
%{
    open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token PLUS MINUS TIMES OVER
%token LPAREN RPAREN
%token LET EQUALS IN
%token EOF

%left PLUS MINUS
%left TIMES OVER

%start <Ast.expr> main

%%

main:
    | e = expr; EOF { e }
    
expr:
    | n = INT 
        { Int n }
    | n = FLOAT
        { Float n }
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
    | LPAREN; content = expr; RPAREN
        { content }
    | LET; name = IDENT; EQUALS; value = expr; IN; body = expr
        { Let (name, value, body) }

