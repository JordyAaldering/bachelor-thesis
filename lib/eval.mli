open Value

type val_env

val eval_err : string -> unit

val ptr_count : int

val create_fresh_ptr : unit -> string

val st_to_str : val_env -> string

(* Creates a fresh variable and adds it to the value environment with value `v' *)
val add_fresh_value : val_env -> value -> (val_env * string)

val ptr_binary : val_env -> value -> string -> string -> value

val ptr_unary : val_env -> value -> string -> value

val eval_expr : Ast.expr -> val_env -> Env.ptr_env -> (val_env * string)

val eval_expr_lst : Ast.expr list -> val_env -> Env.ptr_env -> (val_env * string list)

val eval_prog : Ast.expr -> (val_env * string)
