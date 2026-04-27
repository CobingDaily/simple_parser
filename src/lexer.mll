{
    open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit | '_' | '?'
let whitespace = [' ' '\t' '\n']

let not_quote = [^ '"'] | "\\\""

let ident = alpha alphanum*

rule token = parse
    | whitespace+                   { token lexbuf }
    | digit+ '.' digit+ as numf     { FLOAT (float_of_string numf) }
    | digit+ as num                 { INT (int_of_string num) }
    | '\'' (_ as c) '\''            { CHAR (c)}
    | '\'' '\\' (_ as c) '\''       { CHAR (match c with
                                            | 'n' -> '\n'
                                            | 'r' -> '\r'
                                            | 't' -> '\t'
                                            | '\\' -> '\\'
                                            | _ -> failwith (Printf.sprintf 
                                                    "unknown escape \\%c" 
                                                    c))}
    | '\"' (not_quote* as str) '\"' { STRING (str) }
    | "let"                         { LET }
    | "rec"                         { REC }
    | "in"                          { IN }
    | "=="                          { EQEQ }
    | ">"                           { GT }
    | "<"                           { LT }
    | ">="                          { GE }
    | "<="                          { LE }
    | "->"                          { RARROW }
    | "|>"                          { PIPE }
    | "o"                           { COMPOSE }
    | "if"                          { IF }
    | "then"                        { THEN }
    | "else"                        { ELSE }
    | "true"                        { BOOL (true) }
    | "false"                       { BOOL (false) }
    | "::"                          { CONS }
    | ident as id                   { IDENT id }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '*'                           { TIMES }
    | '/'                           { OVER }
    | '='                           { EQUALS }
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | '['                           { LBRACKET }
    | ']'                           { RBRACKET }
    | ','                           { COMMA }
    | eof                           { EOF }
    | _ as c                        { failwith (Printf.sprintf "unknown character %c" c) }
