type lvl_env

val rewrite_err : string -> unit

val rewrite_err_at : Ast.expr -> string -> unit

val rewrite : Ast.expr -> int -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_f : Ast.expr -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_s : Ast.expr -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_d : Ast.expr -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_lambda : Ast.expr -> int -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_apply : Ast.expr -> int -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_let : Ast.expr -> int -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_val : Ast.expr -> int -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_if : Ast.expr -> int -> Infer.pv_env -> lvl_env -> Ast.expr

val rewrite_prog : Ast.expr -> Infer.pv_env -> Ast.expr
