type type'
type type_env

(* Determine the type of a value *)
val type_of_value : Ast.value -> type'

val empty_type_env : type_env

(* Check type correctness in a program
   Partial, yields one of:
    - a type' , the type of correct expression
    - or throws an error , if types are incorrect *)
val run_tc : type_env -> Ast.expr -> type'
