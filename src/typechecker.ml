open Ast
module StringMap = Map.Make(String)


type type' =
    | IntT
    | FloatT
    | BoolT
    | CharT
    | StringT
    (* | FunctionT of type' * type' (* a function type is type' -> type' *) *)
;;


type type_env = type' StringMap.t list  
let empty_type_env: type_env = [StringMap.empty]

let define_type name (t : type') = function
    | [] -> failwith "no scope"
    | scope :: rest -> (StringMap.add name t scope) :: rest
;;


let rec lookup_type_opt name = function
    | [] -> None
    | scope :: rest ->
            begin match (StringMap.find_opt name scope) with
            | Some v -> Some v
            | None -> lookup_type_opt name rest
            end
;;

let fail_tc message = failwith ("Typechecker: " ^ message)

let type_of_value = function
    | Int _     -> IntT
    | Float _   -> FloatT
    | Bool _    -> BoolT
    | Char _    -> CharT
    | String _  -> StringT
    | Closure _ -> failwith "not implemented"
    (* | Closure _ -> FunctionT *)
;;

let tc_arithmetic = function
    | (_, IntT, IntT) -> IntT
    | (_, FloatT, FloatT) -> FloatT
    | (binop, _, _)-> 
        let operation = match binop with
        | Add -> "addition"
        | Sub -> "subtraction"
        | Mul -> "multiplication"
        | Div -> "division"
        | _ -> fail_tc "unexpected arithmetic operation"
        in
        fail_tc ("Incompatible types in " ^ operation)
;;

let tc_comparison = function
    | IntT, IntT       -> BoolT
    | FloatT, FloatT   -> BoolT
    | BoolT, BoolT     -> BoolT
    | CharT, CharT     -> BoolT
    | StringT, StringT -> BoolT
    | _ -> fail_tc "Incompatible types for comparison"
;;

let tc_binop binop left_type right_type =
    match binop with
    | Add | Sub | Mul | Div -> tc_arithmetic (binop, left_type, right_type)
    | Equals       -> tc_comparison (left_type, right_type)
    | GreaterThan  -> tc_comparison (left_type, right_type)
    | LessThan     -> tc_comparison (left_type, right_type)
    | GreaterEqual -> tc_comparison (left_type, right_type)
    | LessEqual    -> tc_comparison (left_type, right_type)
;;

let tc_unop op expr_type =
  match op with
  | UnaryMinus -> 
      begin match expr_type with
      | IntT -> IntT
      | FloatT -> FloatT
      | _ -> fail_tc "negated unexpected type"
      end

let rec run_tc t_env = function
    | Value v -> type_of_value v
    | Var name ->
        begin match lookup_type_opt name t_env with
        | Some ty -> ty
        | None -> fail_tc ("unbound variable " ^ name)
        end
    | Let (name, value_expr, body_expr) ->
        let value_type = run_tc t_env value_expr in
        let t_env' = define_type name value_type t_env in
        run_tc t_env' body_expr
    | UnOp (op, expr) ->
        let expr_type = run_tc t_env expr in
        tc_unop op expr_type 
    | BinOp (binop, left_expr, right_expr) -> 
            let left_type = run_tc t_env left_expr in
            let right_type = run_tc t_env right_expr in
            tc_binop binop left_type right_type
    | IfElse (test_expr, then_expr, else_expr) ->
            let test_type = run_tc t_env test_expr in
            let then_type = run_tc t_env then_expr in
            let else_type = run_tc t_env else_expr in
            begin match (test_type) with
            | BoolT -> 
                    if then_type = else_type then then_type
                    else fail_tc "Types in both branches of `if` must match"
            | _ -> fail_tc "Expected Bool in if (...)"
            end
    | _ -> failwith "not implemented"
  (*   | Func of string * expr *)
  (*   | Apply of expr * expr *)
