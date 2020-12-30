type pv_env

val rewrite_err : string -> unit

val pv_env_to_str : pv_env -> string

val pv_env_union : pv_env -> pv_env -> pv_env

val sd : Ast.expr -> int array -> pv_env -> pv_env

val pv : Ast.expr -> pv_env -> int array

val infer_prog : Ast.expr -> pv_env
