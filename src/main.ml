
let parse (filename: string) : Ast.expr =
    let i_channel = open_in filename in
    let lexbuf = Lexing.from_channel i_channel in
    Parser.main Lexer.token lexbuf
;;

let interpret (filename: string) =
    try
        let ast = parse filename in
        let result = Interp.interpret ast in
        print_endline result
    with e ->
        Printf.printf "Error: %s\n" (Printexc.to_string e)
;;

let () = interpret "code.example";;
