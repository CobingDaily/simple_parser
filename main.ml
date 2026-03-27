
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

let _test_inputs (list: string list) =
    List.iter (fun x -> test x) list 
;;

let interpret input =
    try
        let result = Interp.interpret @@ parse input in
        print_endline result
    with e ->
        Printf.printf "Error: %s\n\n" (Printexc.to_string e)
;;

let _interpret_inputs (list: string list) =
    List.iter (fun x -> interpret x) list
;;

let _log_inputs (list: string list) =
    List.iter (fun x -> 
        print_endline ("------------------------------");
        test x; 
        print_endline "";
        interpret x;)
    list
;;

let quit_input = function
    | "q" 
    | "Q" 
    | "quit"
    | "exit" -> true
    | _ -> false


let rec repl_loop () =
    Printf.printf ">>> ";
    let user_input = read_line () in
    if quit_input user_input then ()
    else (interpret user_input; repl_loop ();)
;;

let () = repl_loop ();;
