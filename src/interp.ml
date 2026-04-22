open Ast

let do_add = function
  | Int x, Int y -> Int (x + y)
  | Float x, Float y -> Float (x +. y)
  | String a, String b -> String (a ^ b)
  | _ -> failwith "incompatible types for addition"
;;

let do_sub = function
  | Int x, Int y -> Int (x - y)
  | Float x, Float y -> Float (x -. y)
  | _ -> failwith "incompatible types for subtraction"
;;

let do_mul = function
  | Int x, Int y -> Int (x * y)
  | Float x, Float y -> Float (x *. y)
  | Char c, Int n -> String (String.make n c)
  | String s, Int n -> 
      let list = List.init n (fun _ -> s) in
      String (String.concat "" list)
  | _ -> failwith "incompatible types for multiplication"
;;

let do_div = function
    | _, Int 0 | _, Float 0.0 -> failwith "division by zero"
    | Int x, Int y -> Int (x / y)
    | Float x, Float y -> Float (x /. y)
    | _ -> failwith "incompatible types for division"
;;

let are_values_equal = function
  | Int a, Int b       -> Bool (a = b)
  | Float a, Float b   -> Bool (a = b)
  | Bool a, Bool b     -> Bool (a = b)
  | Char a, Char b     -> Bool (a = b)
  | String a, String b -> Bool (a = b)
  | _ -> failwith "incompatible types for comparison"
;;

let is_greater_than = function
  | Int a, Int b       -> Bool (a > b)
  | Float a, Float b   -> Bool (a > b)
  | Bool a, Bool b     -> Bool (a > b)
  | Char a, Char b     -> Bool (a > b)
  | String a, String b -> Bool (a > b)
  | _ -> failwith "incompatible types for comparison"
;;

let is_less_than = function
  | Int a, Int b       -> Bool (a < b)
  | Float a, Float b   -> Bool (a < b)
  | Bool a, Bool b     -> Bool (a < b)
  | Char a, Char b     -> Bool (a < b)
  | String a, String b -> Bool (a < b)
  | _ -> failwith "incompatible types for comparison"
;;

let is_greater_equal = function
  | Int a, Int b       -> Bool (a >= b)
  | Float a, Float b   -> Bool (a >= b)
  | Bool a, Bool b     -> Bool (a >= b)
  | Char a, Char b     -> Bool (a >= b)
  | String a, String b -> Bool (a >= b)
  | _ -> failwith "incompatible types for comparison"
;;

let is_less_equal = function
  | Int a, Int b       -> Bool (a <= b)
  | Float a, Float b   -> Bool (a <= b)
  | Bool a, Bool b     -> Bool (a <= b)
  | Char a, Char b     -> Bool (a <= b)
  | String a, String b -> Bool (a <= b)
  | _ -> failwith "incompatible types for comparison"
;;

let do_negate = function
  | Int x   -> Int (-x)
  | Float x -> Float (-.x)
  | _ -> failwith "Unary Minus needs Int/Float operand"
;;

let is_true = function 
  | Bool false
    | String ""
    | Int 0
    | Float 0.0 -> false
    | _ -> true
;;

let rec do_apply = function
  | Closure (param_name, body_expr, closed_env), arg_value ->
      let env' = define param_name arg_value closed_env in
      eval env' body_expr
  | _ -> failwith "not a function"


and eval env = function
  | Value value -> value
    | Var name -> lookup name env
    | Let (name, expr, body_expr) -> 
        let value = eval env expr in
        let env' = define name value env in
        (eval env' body_expr)
    | Func (param_name, body_expr) -> 
        Closure (param_name, body_expr, env)
    | IfElse (cond_expr, then_expr, else_expr) ->
        let condition_value = eval env cond_expr in
        if (is_true condition_value)
        then eval env then_expr
                else eval env else_expr
    | UnOp (UnaryMinus, expr) ->
        let value_to_negate = eval env expr in
        do_negate value_to_negate
    | BinOp (op, left_expr, right_expr) ->
        let left_value = eval env left_expr in
        let right_value = eval env right_expr in
        let do_binop = begin match op with
             | Add    -> do_add 
             | Sub    -> do_sub 
             | Mul    -> do_mul 
             | Div    -> do_div 
             | Equals       -> are_values_equal
             | GreaterThan  -> is_greater_than
             | LessThan     -> is_less_than
             | GreaterEqual -> is_greater_equal
             | LessEqual    -> is_less_equal
        end in
        do_binop (left_value, right_value)
             | Apply (func_expr, arg_expr) -> 
                 let func_value = eval env func_expr in
                 let arg_value = eval env arg_expr in
                 do_apply (func_value, arg_value)
;;

let interpret expr =
  let empty_env = [StringMap.empty] in

  let open Typechecker in
  let _type = run_tc empty_type_env expr in

  let value = eval empty_env expr in
  string_of_value value
;;

