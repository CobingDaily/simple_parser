
type binop = 
    | Add
    | Sub
    | Mul
    | Div
;;

type expr =
    | Int of int
    | Float of float
    | BinOp of binop * expr * expr (* op, left, right *)
    | Var of string
    | Let of string * expr * expr (* variable name, value, body*)
;;

let rec string_of_expr = function
    | Int n -> Printf.sprintf "%d" n
    | Float n -> Printf.sprintf "%ff" n
    | BinOp (Add, left, right) ->
            let l_expr = string_of_expr left in
            let r_expr = string_of_expr right in
            Printf.sprintf "(%s + %s)" l_expr r_expr
    | BinOp (Sub, left, right) ->
            let l_expr = string_of_expr left in
            let r_expr = string_of_expr right in
            Printf.sprintf "(%s - %s)" l_expr r_expr
    | BinOp (Mul, left, right) ->
            let l_expr = string_of_expr left in
            let r_expr = string_of_expr right in
            Printf.sprintf "(%s * %s)" l_expr r_expr
    | BinOp (Div, left, right) ->
            let l_expr = string_of_expr left in
            let r_expr = string_of_expr right in
            Printf.sprintf "(%s / %s)" l_expr r_expr
    | Var x -> x
    | Let (name, value, body) ->
            let value = string_of_expr value in
            let body = string_of_expr body in
            Printf.sprintf "(let %s = %s in %s)" name value body
;;
