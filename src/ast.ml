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


and string_of_value = function
    | Int n -> Printf.sprintf "%i" n
    | Float n -> Printf.sprintf "%f" n
    | Bool b -> if b then "true" else "false"
    | Char c -> Printf.sprintf "%C" c
    | String s -> Printf.sprintf "%S" s
    | Closure (param_name, body, env) ->
            let s = string_of_expr env body in
            Printf.sprintf "%s -> %s" param_name s
    | List l ->
        begin match l with
        | [] -> ""
        | hd :: tl -> (string_of_value hd) ^ ", " ^ (string_of_value (List tl))
        end
          
        

and string_of_expr env = function
    | Value v -> string_of_value v
    | Var name -> 
        (match (lookup_opt name !env) with
                   | None -> name
                   | Some v -> string_of_value v)
    | ListExpr le ->
        begin match le with
        | [] -> ""
        | hd :: tl -> (string_of_expr env hd) ^ ", " ^ (string_of_expr env (ListExpr tl))
        end

    | UnOp (op, expr) ->
            let s = string_of_expr env expr in
            (match op with
                | UnaryMinus -> Printf.sprintf "(-%s)" s)
    | BinOp (op, left, right) ->
            let l_expr = string_of_expr env left in
            let r_expr = string_of_expr env right in
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
            let value = string_of_expr env value in
            let body = string_of_expr env body in
            Printf.sprintf "(let %s = %s in %s)" name value body
    | LetRec (name, value, body) ->
            let value = string_of_expr env value in
            let body = string_of_expr env body in
            Printf.sprintf "(let rec %s = %s in %s)" name value body
    | IfElse (condition_expr, then_expr, other_expr) ->
            let condition = string_of_expr env condition_expr in
            let then_s = string_of_expr env then_expr in
            let other_s = string_of_expr env other_expr in
            Printf.sprintf "(if %s then %s else %s)" condition then_s other_s
    | Func (left, right) ->
            let right = string_of_expr env right in
            Printf.sprintf "(%s -> %s)" left right
    | Apply (function_expr, argument_expr) ->
            let f = string_of_expr env function_expr in
            let x = string_of_expr env argument_expr in
            Printf.sprintf "(%s %s)" f x
;;


(* let rec print_env = function
    | [] -> ()
    | scope :: rest -> 
            let f = (fun n v -> Printf.printf "%s <-> %s\n" n (string_of_value v)) in
            StringMap.iter f scope;
            print_env rest
*)
