
%{
    open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token <string> IDENT
%token PLUS MINUS TIMES OVER
%token LPAREN RPAREN
%token EQEQ
%token RARROW PIPE
%token LET EQUALS IN
%token IF THEN ELSE
%token EOF

%start <Ast.expr> main
%%
main:
    | e = expr; EOF { e }

expr:
    | e = pipe_expr { e }
    | p = IDENT; RARROW; right = expr
        { Func (p, right) }
    | LET; name = IDENT; EQUALS; value = expr; IN; body = expr
        { Let (name, value, body) }
    | IF; cond = expr; THEN; then_expr = expr; ELSE; other_expr = expr
        { IfElse (cond, then_expr, other_expr) }

pipe_expr:
    | e = eq_expr { e }
    | left = pipe_expr; PIPE; right = eq_expr
        { BinOp (Apply, right, left) }

eq_expr:
    | e = add_expr { e }
    | left = eq_expr; EQEQ; right = add_expr
        { BinOp (Equals, left, right) }

add_expr:
    | e = mul_expr { e }
    | left = add_expr; PLUS; right = mul_expr
        { BinOp (Add, left, right) }
    | left = add_expr; MINUS; right = mul_expr
        { BinOp (Sub, left, right) }
    | MINUS; operand = mul_expr
        { UnOp (UnaryMinus, operand) }

mul_expr:
    | e = app_expr { e }
    | left = mul_expr; TIMES; right = app_expr
        { BinOp (Mul, left, right) }
    | left = mul_expr; OVER; right = app_expr
        { BinOp (Div, left, right) }

app_expr:
    | e = atom_expr { e }
    | func = app_expr; arg = atom_expr
        { BinOp (Apply, func, arg) }

atom_expr:
    | n = INT       { Value (Int n) }
    | n = FLOAT     { Value (Float n) }
    | b = BOOL      { Value (Bool b) }
    | c = CHAR      { Value (Char c) }
    | s = STRING    { Value (String s) }
    | x = IDENT     { Var x }
    | LPAREN; content = expr; RPAREN { content }
