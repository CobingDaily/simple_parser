module StringMap = Map.Make(String)

type env = value StringMap.t list 

and unop =
    | UnaryMinus

and binop = 
    | Add
    | Sub
    | Mul
    | Div
    | Equals
    | GreaterThan
    | LessThan
    | GreaterEqual
    | LessEqual

and value =
    | Int of int
    | Float of float
    | Bool of bool
    | Char of char
    | String of string
    | Closure of string * expr * env ref (* (param, body, mutable env)
                                            Mutable env for tying the
                                            recursive knot in letrec. *)
    | List of value list

and expr =
    | Value of value
    | BinOp of binop * expr * expr     (* op, left, right *)
    | UnOp of unop * expr              (* op, expr *)
    | Var of string                    (* variable name *)
    | Let of string * expr * expr      (* variable name, value, body *)
    | LetRec of string * expr * expr   (* variable name, value, body *)
    | IfElse of expr * expr * expr     (* condition, then_expr, other_expr *)
    | Func of string * expr            (* param name -> expr *)
    | Apply of expr * expr             (* function_expr argument_expr *)
    | ListExpr of expr list
    | Cons of expr * expr              (* first_expr, rest_expr *)
    | First of expr                    (* list_expr *)
    | Rest of expr                     (* list_expr *)
;;

let rec define name value = function
    | [] -> failwith "no scope"
    | scope :: rest -> (StringMap.add name value scope) :: rest


and lookup_opt name = function
    | [] -> None
    | scope :: rest ->
            (match (StringMap.find_opt name scope) with
            | Some v -> Some v
            | None -> lookup_opt name rest)


and lookup name env = 
    match (lookup_opt name env) with
    | None -> failwith (Printf.sprintf "Unknown name %s" name)
    | Some v -> v


and string_of_value_list l =
        let f acc item =
          let sep = match acc with
            | "" -> ""
            | _  -> ", "
          in
          acc ^ sep ^ (string_of_value item)
        in
        let s = List.fold_left f "" l in
        "[" ^ s ^ "]"

and string_of_value = function
    | Int n -> Printf.sprintf "%i" n
    | Float n -> Printf.sprintf "%f" n
    | Bool b -> if b then "true" else "false"
    | Char c -> Printf.sprintf "%C" c
    | String s -> Printf.sprintf "%S" s
    | Closure (param_name, body, _) ->
            let s = string_of_expr body in
            Printf.sprintf "%s -> %s" param_name s
    | List l -> string_of_value_list l


and string_of_expr = function
    | Value v -> string_of_value v
    | Var name -> name
    | ListExpr le ->
        begin match le with
        | [] -> ""
        | hd :: tl -> (string_of_expr hd) ^ ", " ^ (string_of_expr (ListExpr tl))
        end

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
                | Equals       -> Printf.sprintf "(%s == %s)" l_expr r_expr
                | GreaterThan  -> Printf.sprintf "(%s > %s)" l_expr r_expr
                | LessThan     -> Printf.sprintf "(%s < %s)" l_expr r_expr
                | GreaterEqual -> Printf.sprintf "(%s >= %s)" l_expr r_expr
                | LessEqual    -> Printf.sprintf "(%s <= %s)" l_expr r_expr)
    | Let (name, value, body) ->
            let value = string_of_expr value in
            let body = string_of_expr body in
            Printf.sprintf "(let %s = %s in %s)" name value body
    | LetRec (name, value, body) ->
            let value = string_of_expr value in
            let body = string_of_expr body in
            Printf.sprintf "(let rec %s = %s in %s)" name value body
    | IfElse (condition_expr, then_expr, other_expr) ->
            let condition = string_of_expr condition_expr in
            let then_s = string_of_expr then_expr in
            let other_s = string_of_expr other_expr in
            Printf.sprintf "(if %s then %s else %s)" condition then_s other_s
    | Func (left, right) ->
            let right = string_of_expr right in
            Printf.sprintf "(%s -> %s)" left right
    | Apply (function_expr, argument_expr) ->
            let f = string_of_expr function_expr in
            let x = string_of_expr argument_expr in
            Printf.sprintf "(%s %s)" f x
    | Cons (first_expr, rest_expr) ->
        let first = string_of_expr first_expr in
        let rest  = string_of_expr rest_expr in
        Printf.sprintf "(%s :: %s)" first rest
    | First (list_expr) ->
        let list_value = string_of_expr list_expr in
        Printf.sprintf "(first %s)" list_value
    | Rest (list_expr) ->
        let list_value = string_of_expr list_expr in
        Printf.sprintf "(rest %s)" list_value
;;
