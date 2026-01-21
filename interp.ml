open Ast

let rec eval = function
    | Int x -> x
    | Float _ -> failwith "floats not implemented"
    | BinOp (op, left, right) ->
            let left = eval left in
            let right = eval right in
            (match op with
             | Add -> left + right
             | Sub -> left - right
             | Mul -> left * right
             | Div -> left / right)
    | Var _ -> failwith "var not implemented"
    | Let _ -> failwith "let not implemented"
;;
