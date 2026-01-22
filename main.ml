
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

let interpret input =
    Printf.printf "Input: %S\n" input;
    try
        let result = Interp.interpret @@ parse input in
        print_endline result
    with e ->
        Printf.printf "Error: %s\n\n" (Printexc.to_string e)
;;

let interpret_inputs (list: string list) =
    List.iter (fun x -> interpret x) list
;;

let () = 
    (* test_inputs ["3.14"; ".5"; "3."; "."]; *)
    test_inputs ["let x = 5 in let y = 3 in x + y * 3"];

    let calculations = [
        "4 + 2 * 3 - 6 / 3 + 4";
        "2.5 + 2 * 3";
        "2 + 2 * 3";
        "(1 / 1000.0) * 500";
    ] in 
    interpret_inputs calculations
;;
