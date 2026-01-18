{
    open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit | '_'
let whitespace = [' ' '\t' '\n']

rule token = parse
    | whitespace+                   { token lexbuf }
    | digit+ '.' digit+ as numf     { FLOAT (float_of_string numf) }
    | digit+ as num                 { INT (int_of_string num) }
    | "let"                         { LET }
    | "in"                          { IN }
    | alpha alphanum* as id         { IDENT id }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '*'                           { TIMES }
    | '/'                           { OVER }
    | '='                           { EQUALS }
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | eof                           { EOF }
    | _ as c                        { failwith (Printf.sprintf "Unknown character %c" c) }
