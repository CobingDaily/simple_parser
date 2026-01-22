open Ast

type value =
    | IntVal of int
    | FloatVal of float
;;

let rec eval = function
    | Int x -> IntVal x
    | Float x -> FloatVal x
    | Var _ -> failwith "var not implemented"
    | Let _ -> failwith "let not implemented"
    | BinOp (op, left, right) ->
            let left = eval left in
            let right = eval right in
            (match op with
             | Add -> eval_binop ( + ) ( +. ) left right
             | Sub -> eval_binop ( - ) ( -. ) left right
             | Mul -> eval_binop ( * ) ( *. ) left right 
             | Div -> eval_binop ( / ) ( /. ) left right)


and eval_binop op_int op_float left right =
    match (left, right) with
    | IntVal l, IntVal r -> IntVal (op_int l r)
    | IntVal l, FloatVal r -> FloatVal (op_float (float_of_int l) r)
    | FloatVal l, IntVal r -> FloatVal (op_float l (float_of_int r))
    | FloatVal l, FloatVal r -> FloatVal (op_float l r)
;;

let interpret expr =
    let value = eval expr in
    match value with
    | IntVal n -> Printf.sprintf "%i" n
    | FloatVal n -> Printf.sprintf "%f" n
;;

