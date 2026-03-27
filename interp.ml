open Ast

let rec eval = function
    | Value value -> value
    | Var _ -> failwith "var not implemented"
    | Let _ -> failwith "let not implemented"
    | IfElse (cond_expr, then_expr, other_expr) ->
            (match (eval cond_expr) with
            | Bool false
            | String ""
            | Int 0
            | Float 0.0 -> eval other_expr
            | _ -> eval then_expr)
    | BinOp (op, left, right) ->
            let left = eval left in
            let right = eval right in
            (match op with
             | Add -> (match (left, right) with
                        | Int x, Int y -> Int (x + y)
                        | Float x, Float y -> Float (x +. y)
                        | String a, String b -> String (a ^ b)
                        | _ -> failwith "cannot infer types")
             | Sub -> (match (left, right) with
                        | Int x, Int y -> Int (x - y)
                        | Float x, Float y -> Float (x -. y)
                        | _ -> failwith "cannot infer types")
             | Mul -> (match (left, right) with
                        | Int x, Int y -> Int (x * y)
                        | Float x, Float y -> Float (x *. y)
                        | Char c, Int n -> String (String.make n c)
                        | String s, Int n -> 
                                let list = List.init n (fun _ -> s) in
                                String (String.concat "" list)
                        | _ -> failwith "cannot infer types")
             | Div -> (match (left, right) with
                        | Int x, Int y -> Int (x / y)
                        | Float x, Float y -> Float (x /. y)
                        | _ -> failwith "cannot infer types")
             | Equals -> Bool (left = right))
;;
        

let interpret expr =
    let value = eval expr in
    match value with
    | Int n -> Printf.sprintf "%i" n
    | Float n -> Printf.sprintf "%f" n
    | Bool b -> if b then "true" else "false"
    | Char c -> Printf.sprintf "%C" c
    | String s -> Printf.sprintf "%S" s
;;

