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
             | Add -> eval_binop ( + ) ( +. ) left right
             | Sub -> eval_binop ( - ) ( -. ) left right
             | Mul -> eval_binop ( * ) ( *. ) left right 
             | Div -> eval_binop ( / ) ( /. ) left right
             | Equals -> Bool (left = right))


and eval_binop op_int op_float left right =
    match (left, right) with
    | Int   x, Int   y -> Int (op_int x y)
    | Float x, Float y -> Float (op_float x y)
    | _ -> failwith (Printf.sprintf "Cannot infer type")
;;

let interpret expr =
    let value = eval expr in
    match value with
    | Int n -> Printf.sprintf "%i" n
    | Float n -> Printf.sprintf "%f" n
    | Bool b -> if b then "true" else "false"
    | String s -> Printf.sprintf "%S" s
;;

