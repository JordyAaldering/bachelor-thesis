open Ast
open Printf

exception ParseError of string

let parse_err msg =
    raise @@ ParseError msg

type token =
    (* variables *)
    | ID of string
    | INT of int
    | FLOAT of float
    (* expressions *)
    | LAMBDA
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | SHAPE
    | DIM
    | READ
    (* operands *)
    | ADD
    | MIN
    | MUL
    | DIV
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    (* symbols *)
    | DOT
    | COMMA
    | LPAREN
    | RPAREN
    | LSQUARE
    | RSQUARE
    | EOF

let token_to_str tok = match tok with
    (* variables *)
    | ID s      -> s
    | INT x     -> string_of_int x
    | FLOAT x   -> string_of_float x
    (* expressions *)
    | LAMBDA    -> "\\"
    | LET       -> "let"
    | IN        -> "in"
    | IF        -> "if"
    | THEN      -> "then"
    | ELSE      -> "else"
    | SHAPE     -> "shape"
    | DIM       -> "dim"
    | READ      -> "read"
    (* operands *)
    | ADD       -> "+"
    | MIN       -> "-"
    | MUL       -> "*"
    | DIV       -> "/"
    | NE        -> "!="
    | EQ        -> "=="
    | LT        -> "<"
    | LE        -> "<="
    | GT        -> ">"
    | GE        -> ">="
    (* symbols *)
    | DOT       -> "."
    | COMMA     -> ","
    | LPAREN    -> "("
    | RPAREN    -> ")"
    | LSQUARE   -> "["
    | RSQUARE   -> "]"
    | EOF       -> "EOF"

let is_op tok = match tok with
    | ADD
    | MIN
    | MUL
    | DIV
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE -> true
    | _ -> false

let op_prec tok = match tok with
    | EQ
    | NE -> 1
    | LT
    | LE
    | GT
    | GE -> 2
    | ADD
    | MIN -> 3
    | MUL
    | DIV -> 4
    | _ -> 5

let op_to_binop tok = match tok with
    | ADD  -> OpAdd
    | MIN  -> OpMin
    | MUL  -> OpMul
    | DIV  -> OpDiv
    | EQ   -> OpEq
    | NE   -> OpNe
    | LT   -> OpLt
    | LE   -> OpLe
    | GT   -> OpGt
    | GE   -> OpGe
    | _ -> parse_err @@ sprintf "token `%s' is not a binary operand" (token_to_str tok)
