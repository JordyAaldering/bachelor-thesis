type value

val value_err : string -> unit

val value_to_str : value -> string

val value_sel : value -> value -> value

val value_shape : value -> value

val value_dim : value -> value

val value_neg : value -> value

val value_add : value -> value -> value

val value_mul : value -> value -> value

val value_div : value -> value -> value

val value_is_truthy : value -> bool

val value_not : value -> value

val value_eq : value -> value -> value

val value_gt : value -> value -> value

val value_lt : value -> value -> value

val extract_value : value -> (int list * float list)

val extract_closure : value -> (string * Ast.expr * Env.ptr_env)
