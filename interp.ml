open Ast

module StringMap = Map.Make(String)
let empty_env = [StringMap.empty]

let define name value = function
    | [] -> failwith "no scope"
    | scope :: rest -> (StringMap.add name value scope) :: rest
;;

let rec lookup name = function
    | [] -> failwith (Printf.sprintf "Unknown name %s" name)
    | scope :: rest ->
            (match (StringMap.find_opt name scope) with
            | Some v -> v
            | None -> lookup name rest)
;;


let rec eval env = function
    | Value value -> value
    | Var name -> lookup name env
    | Let (name, expr, body) -> 
            let value = eval env expr in
            let env' = define name value env in
            (eval env' body)
    | Func (param_name, body) -> Closure (param_name, body, env)
    | IfElse (cond_expr, then_expr, other_expr) ->
            (match (eval env cond_expr) with
            | Bool false
            | String ""
            | Int 0
            | Float 0.0 -> eval env other_expr
            | _ -> eval env then_expr)
    | UnOp (op, expr) ->
            (match op with
            | UnaryMinus -> 
                    (match (eval env expr) with
                     | Int x -> Int (-x)
                     | Float x -> Float (-.x)
                     | _ -> failwith "Unary Minus needs Int/Float operand"))
    | BinOp (op, left, right) ->
            let left = eval env left in
            let right = eval env right in
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
             | Equals -> 
                     (match (left, right) with
                       | Int a, Int b       -> Bool (a = b)
                       | Float a, Float b   -> Bool (a = b)
                       | Bool a, Bool b     -> Bool (a = b)
                       | Char a, Char b     -> Bool (a = b)
                       | String a, String b -> Bool (a = b)
                       | _ -> failwith "incompatible types for comparison")
             | Apply ->
                     let func = left in
                     let arg = right in
                     (match func with
                     | Closure (param_name, body, closed_env) ->
                             let env' = define param_name arg closed_env in
                             eval env' body
                     | _ -> failwith "Not a function"))
;;

let string_of_value = function
    | Int n -> Printf.sprintf "%i" n
    | Float n -> Printf.sprintf "%f" n
    | Bool b -> if b then "true" else "false"
    | Char c -> Printf.sprintf "%C" c
    | String s -> Printf.sprintf "%S" s
    | Closure (param_name, body, _) ->
            let s = string_of_expr body in
            Printf.sprintf "%s -> %s" param_name s
;;
        
let interpret expr =
    let value = eval empty_env expr in
    string_of_value value
;;

