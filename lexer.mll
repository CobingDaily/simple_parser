{
    open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit | '_'
let whitespace = [' ' '\t' '\n']

let not_quote = [^ '"'] | "\\\""


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
    | "in"                          { IN }
    | "=="                          { EQEQ }
    | "->"                          { RARROW }
    | "|>"                          { PIPE }
    | "if"                          { IF }
    | "then"                        { THEN }
    | "else"                        { ELSE }
    | "true"                        { BOOL (true) }
    | "false"                       { BOOL (false) }
    | alpha alphanum* as id         { IDENT id }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '*'                           { TIMES }
    | '/'                           { OVER }
    | '='                           { EQUALS }
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | eof                           { EOF }
    | _ as c                        { failwith (Printf.sprintf "unknown character %c" c) }
