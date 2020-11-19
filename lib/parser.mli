(*open Lexer
open Lexing

val opt_get : token option -> token

val op_prec : token -> int

val parse_err : string -> unit

(* Stack to keep tokens that we have peeked at but not consumed yet *)
val token_stack : token list

val get_token : lexbuf -> token

val unget_token : token -> unit

val peek_token : lexbuf -> token

val match_token : lexbuf -> token -> bool

val expect_id : lexbuf -> token

val expect_token : lexbuf -> token -> unit

val parse_primary : lexbuf -> token option

(* Parse non-empty comma separated list of elements
    that can be parsed by `parse_fun' *)
val parse_array : lexbuf -> (lexbuf -> token) -> Ast.expr list

val parse_postfix : lexbuf -> Ast.expr

val parse_expr : lexbuf -> Ast.expr

val parse_application : Ast.expr -> lexbuf -> Ast.expr

val parse_lambda : lexbuf -> Ast.expr option

val parse_letin : lexbuf -> Ast.expr option

val parse_ifthen : lexbuf -> Ast.expr option

val parse_shape : lexbuf -> Ast.expr option

val parse_dim : lexbuf -> Ast.expr option

val parse_binary : lexbuf -> Ast.expr option

val parse_unary : lexbuf -> Ast.expr option

val parse_prog : lexbuf -> Ast.expr
*)