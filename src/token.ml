open Ast
open Printf

exception ParseError of string

let parse_err (msg: string) =
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
    | GEN
    | WITH
    (* primitive functions *)
    | SHAPE
    | DIM
    | READ
    (* operands *)
    | CONCAT
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

let token_to_str (tok: token) : string =
    match tok with
    (* variables *)
    | ID s      -> s
    | INT x     -> string_of_int x
    | FLOAT x   -> sprintf "%g" x
    (* expressions *)
    | LAMBDA    -> "\\"
    | LET       -> "let"
    | IN        -> "in"
    | IF        -> "if"
    | THEN      -> "then"
    | ELSE      -> "else"
    | GEN       -> "gen"
    | WITH      -> "with"
    (* primitive functions *)
    | SHAPE     -> "value_shape"
    | DIM       -> "value_dim"
    | READ      -> "read"
    (* operands *)
    | CONCAT    -> "@"
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

let is_op (tok: token) : bool =
    match tok with
    | CONCAT
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

let op_prec (tok: token) : int =
    match tok with
    | EQ
    | NE -> 1
    | LT
    | LE
    | GT
    | GE -> 2
    | CONCAT -> 3
    | ADD
    | MIN -> 4
    | MUL
    | DIV -> 5
    | _ -> 6

let op_to_binop (tok: token) : bop =
    match tok with
    | CONCAT -> OpConcat
    | ADD    -> OpAdd
    | MIN    -> OpMin
    | MUL    -> OpMul
    | DIV    -> OpDiv
    | EQ     -> OpEq
    | NE     -> OpNe
    | LT     -> OpLt
    | LE     -> OpLe
    | GT     -> OpGt
    | GE     -> OpGe
    | _ -> parse_err @@ sprintf "token `%s' is not a binary operand"
            (token_to_str tok)
