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
    (* operands *)
    | APPEND
    | ADD
    | MIN
    | MUL
    | DIV
    | NOT
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    (* symbols *)
    | DOT
    | COMMA
    | TILDE
    | BAR
    | LPAREN
    | RPAREN
    | LSQUARE
    | RSQUARE
    | EOF

let token_to_str (t: token) : string =
    match t with
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
    | SHAPE     -> "shape"
    | DIM       -> "dim"
    (* operands *)
    | APPEND    -> "++"
    | ADD       -> "+"
    | MIN       -> "-"
    | MUL       -> "*"
    | DIV       -> "/"
    | NOT       -> "!"
    | EQ        -> "="
    | NE        -> "!="
    | LT        -> "<"
    | LE        -> "<="
    | GT        -> ">"
    | GE        -> ">="
    (* symbols *)
    | DOT       -> "."
    | COMMA     -> ","
    | TILDE     -> "~"
    | BAR       -> "|"
    | LPAREN    -> "("
    | RPAREN    -> ")"
    | LSQUARE   -> "["
    | RSQUARE   -> "]"
    | EOF       -> "EOF"

let is_op (t: token) : bool =
    match t with
    | APPEND
    | ADD
    | MIN
    | MUL
    | DIV
    | NOT
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    | TILDE
    | BAR -> true
    | _ -> false

let op_prec (t: token) : int =
    match t with
    | EQ
    | NE -> 1
    | LT
    | LE
    | GT
    | GE -> 2
    | APPEND -> 3
    | TILDE
    | BAR -> 4
    | ADD
    | MIN
    | NOT -> 5
    | MUL
    | DIV -> 6
    | _ -> 7

let op_to_bop (t: token) : bop =
    match t with
    | APPEND -> OpAppend
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
    | _ ->
        parse_err @@ sprintf "token `%s' is not a binary operand"
            (token_to_str t)

let op_to_uop (t: token) : uop =
    match t with
    | TILDE -> OpNeg
    | BAR   -> OpAbs
    | _ ->
        parse_err @@ sprintf "token `%s' is not a unary operand"
            (token_to_str t)
