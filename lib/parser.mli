open Lexing

val parse_err : string -> unit

val parse_expr : lexbuf -> Ast.expr

val parse_postfix : lexbuf -> Ast.expr

val parse_application : Ast.expr -> lexbuf -> Ast.expr

val parse_lambda : lexbuf -> Ast.expr option

val parse_letin : lexbuf -> Ast.expr option

val parse_ifthen : lexbuf -> Ast.expr option

val parse_binary : lexbuf -> Ast.expr option

val parse_unary : lexbuf -> Ast.expr option

val parse_shape : lexbuf -> Ast.expr option

val parse_dim : lexbuf -> Ast.expr option

val parse_prog : lexbuf -> Ast.expr
