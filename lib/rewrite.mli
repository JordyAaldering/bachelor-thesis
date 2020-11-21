open Ast
open Infer

type lvl_env

val rewrite_err : string -> unit

val rewrite_err_at : expr -> string -> unit

val rewrite : expr -> int -> pv_env -> lvl_env -> expr

val rewrite_f : expr -> pv_env -> lvl_env -> expr

val rewrite_s : expr -> pv_env -> lvl_env -> expr

val rewrite_d : expr -> pv_env -> lvl_env -> expr

val rewrite_lambda : expr -> int -> pv_env -> lvl_env -> expr

val rewrite_apply : expr -> int -> pv_env -> lvl_env -> expr

val rewrite_let : expr -> int -> pv_env -> lvl_env -> expr

val rewrite_val : expr -> int -> pv_env -> lvl_env -> expr

val rewrite_if : expr -> int -> pv_env -> lvl_env -> expr

val rewrite_prog : expr -> pv_env -> expr
