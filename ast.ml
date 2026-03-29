type unop =
    | UnaryMinus
;;

type binop = 
    | Add
    | Sub
    | Mul
    | Div
    | Equals
;;

type value =
    | Int of int
    | Float of float
    | Bool of bool
    | Char of char
    | String of string
;;

type expr =
    | Value of value
    | BinOp of binop * expr * expr  (* op, left, right *)
    | UnOp of unop * expr           (* op, expr *)
    | Var of string
    | Let of string * expr * expr   (* variable name, value, body*)
    | IfElse of expr * expr * expr  (* condition, then_expr, other_expr *)
;;

let rec string_of_expr = function
    | Var x -> x
    | Value v ->
        (match v with
            | Int    n -> Printf.sprintf "%d" n
            | Float  n -> Printf.sprintf "%ff" n
            | Bool   b -> if b then "true" else "false"
            | Char   c -> Printf.sprintf "%C" c
            | String s -> Printf.sprintf "%S" s)
    | UnOp (op, expr) ->
            let s = string_of_expr expr in
            (match op with
                | UnaryMinus -> Printf.sprintf "(-%s)" s)
    | BinOp (op, left, right) ->
            let l_expr = string_of_expr left in
            let r_expr = string_of_expr right in
            (match op with
                | Add    -> Printf.sprintf "(%s + %s)" l_expr r_expr
                | Sub    -> Printf.sprintf "(%s - %s)" l_expr r_expr
                | Mul    -> Printf.sprintf "(%s * %s)" l_expr r_expr
                | Div    -> Printf.sprintf "(%s / %s)" l_expr r_expr
                | Equals -> Printf.sprintf "(%s == %s)" l_expr r_expr)
    | Let (name, value, body) ->
            let value = string_of_expr value in
            let body = string_of_expr body in
            Printf.sprintf "(let %s = %s in %s)" name value body
    | IfElse (condition_expr, then_expr, other_expr) ->
            let condition = string_of_expr condition_expr in
            let then_s = string_of_expr then_expr in
            let other_s = string_of_expr other_expr in
            Printf.sprintf "(if %s then %s else %s)" condition then_s other_s
;;
