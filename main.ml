
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

(*
let interpret_inputs (list: string list) =
    List.iter (fun x -> interpret x) list
;;
*)

let log_inputs (list: string list) =
    List.iter (fun x -> 
        print_endline ("------------------------------");
        test x; 
        print_endline "";
        interpret x;)
    list
;;

let () = 
    (* test_inputs ["3.14"; ".5"; "3."; "."]; *)
    test_inputs ["let x = 5 in let y = 3 in x + y * 3"];
    test_inputs ["a == b"; "a == b == c"; "a==b"];

    let calculations = [
        "if (5*5 == 25) then (42 * 3.14) else 0.0";
        "if (5*5 == 24) then (42 * 3.14) else 0.0"
    ] in 
    log_inputs calculations;
;;
