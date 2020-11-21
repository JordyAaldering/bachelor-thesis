type token

val token_to_str : token -> string

val is_op : token -> bool

val op_to_binop : token -> Ast.bop
