open Ast
module StringMap = Map.Make(String)

let fail_tc message = failwith ("Typechecker: " ^ message)

type type' =
    | IntT
    | FloatT
    | BoolT
    | CharT
    | StringT
    | ClosureT
    | ListT of type' option

and type_env = type' StringMap.t list  

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

let rec type_of_value = function
    | Int _     -> IntT
    | Float _   -> FloatT
    | Bool _    -> BoolT
    | Char _    -> CharT
    | String _  -> StringT
    | Closure _ -> ClosureT
    | List l    -> 
        begin match l with
        | []      -> ListT None
        | hd :: _ -> ListT (Some (type_of_value hd))
        end
;;

let tc_comparison = function
    | IntT, IntT       -> BoolT
    | FloatT, FloatT   -> BoolT
    | BoolT, BoolT     -> BoolT
    | CharT, CharT     -> BoolT
    | StringT, StringT -> BoolT
    | ListT _, ListT _ -> BoolT
    | _ -> fail_tc "incompatible types for comparison"
;;

let tc_addition = function
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    | (StringT, StringT) -> StringT
    | _ -> fail_tc "incompatible types for addition"
;;

let tc_subtraction = function
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    | _ -> fail_tc "incompatible types for subtraction"
;;

let tc_multiplication = function
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    | (CharT, IntT) | (IntT, CharT) -> StringT
    | (StringT, IntT) | (IntT, StringT) -> StringT
    | _ -> fail_tc "incompatible types for multiplication"
;;

let tc_division = function
    | (IntT, IntT) -> IntT
    | (FloatT, FloatT) -> FloatT
    | _ -> fail_tc "incompatible types for division"
;;


let tc_binop binop left_type right_type =
    match binop with
    | Add -> tc_addition       (left_type, right_type)
    | Sub -> tc_subtraction    (left_type, right_type)
    | Mul -> tc_multiplication (left_type, right_type)
    | Div -> tc_division       (left_type, right_type)
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
;;

let tc_list = function
  | [] -> ListT (None)
  | (hd :: _) as type_list ->
    let items_same_type = List.for_all (fun ty -> ty = hd) type_list in
    if items_same_type then ListT (Some hd)
    else fail_tc "list items must be of same type"
;;

let tc_cons first_type rest_type =
  begin match rest_type with
    | ListT (None) -> ListT (Some first_type)
    | ListT (Some ty) ->
        if first_type = ty then ListT (Some ty)
        else fail_tc "in cons (::) left operand must match the type of list items"
    | _ -> fail_tc "in cons (::) right operand must be a list"
  end
;;

let rec run_tc t_env = function
    | Value v -> type_of_value v
    | Var name ->
        begin match lookup_type_opt name t_env with
        | Some ty -> ty
        | None -> fail_tc ("unbound variable " ^ name)
        end
    | ListExpr expr_list ->
        let type_list = List.map (run_tc t_env) expr_list in
        tc_list type_list
    | Cons (first_expr, rest_expr) ->
        let first_type = run_tc t_env first_expr in
        let rest_type  = run_tc t_env rest_expr  in
        tc_cons first_type rest_type
    | Let (name, value_expr, body_expr) ->
        let value_type = run_tc t_env value_expr in
        let t_env' = define_type name value_type t_env in
        run_tc t_env' body_expr
    | LetRec (name, value_expr, body_expr) ->
        let t_env_ref = ref t_env in
        let value_type = run_tc !t_env_ref value_expr in
        t_env_ref := define_type name value_type !t_env_ref;
        run_tc !t_env_ref body_expr
    | Func _ ->  ClosureT (* For now skip function typechecking *)
    | Apply (func_expr, _) -> 
       (* The only "compile-time" type check we can do
          currently is to check whether the left-side expression 
          of the applicaiton is a `ClosureT` - a generic function type.

          The `Interpreter.eval` function will help us out by improving type
          safety with "run-time" checks.

          For example if an expression is:
            - ((x -> x + 1) 2)   , TC passes and `eval` correctly evalues 3
            - ((x -> x + 1) 2.0) , TC passes BUT `eval` will throw a
                                   "incompatible types for addition" Error
            - ("text" 2) , TC fails : "Typechecker: tried to apply 'not-a-function'"
          *)
       let func_type = run_tc t_env func_expr in
       begin match func_type with
       | ClosureT -> ClosureT
       | _ -> fail_tc "tried to apply 'not-a-function'"
       end
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
                    else fail_tc "types in both branches of `if` must match"
            | _ -> fail_tc "expected Bool in if (...)"
            end
;;
