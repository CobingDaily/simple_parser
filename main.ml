
let parse (s: string) : Ast.expr =
    let lexbuf = Lexing.from_string s in
    Parser.main Lexer.token lexbuf
;;

let test input =
    Printf.printf "Input: %S\n" input;
    try
        let result = parse input in
        Printf.printf "Result: %s\n\n" (Ast.string_of_expr result)
    with e ->
        Printf.printf "Error: %s\n\n" (Printexc.to_string e)
;;

let test_inputs (list: string list) =
    List.iter (fun x -> test x) list 
;;

let () = 
    (* test_inputs ["3.14"; ".5"; "3."; "."]; *)
    test_inputs ["3.5 / 4.3"; "3 - 2.1"; "3.2 - 3 / 1"];
    test_inputs ["let x = 5 in let y = 3 in x + y * 3"]

