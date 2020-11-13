{
open Ast
open Lexing

type token =
    | ID of string
    | INT of int
    | FLOAT of float
    | LAMBDA
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | SHAPE
    | DIM
    | PLUS
    | MIN
    | MULT
    | DIV
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    | DOT
    | COMMA
    | LPAREN
    | RPAREN
    | LSQUARE
    | RSQUARE
    | EOF

let token_to_str = function
    | ID x    -> x
    | INT x   -> string_of_int x
    | FLOAT x -> string_of_float x
    | LAMBDA  -> "\\"
    | LET     -> "let"
    | IN      -> "in"
    | IF      -> "if"
    | THEN    -> "then"
    | ELSE    -> "else"
    | SHAPE   -> "shape"
    | DIM     -> "dim"
    | PLUS    -> "+"
    | MIN     -> "-"
    | MULT    -> "*"
    | DIV     -> "/"
    | NE      -> "!="
    | EQ      -> "=="
    | LT      -> "<"
    | LE      -> "<="
    | GT      -> ">"
    | GE      -> ">="
    | DOT     -> "."
    | COMMA   -> ","
    | LPAREN  -> "("
    | RPAREN  -> ")"
    | LSQUARE -> "["
    | RSQUARE -> "]"
    | EOF     -> "EOF"

let is_op tok = match tok with
    | PLUS
    | MIN
    | MULT
    | DIV
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE -> true
    | _ -> false

let op_to_binop tok = match tok with
    | PLUS  -> OpPlus
    | MIN   -> OpMin
    | MULT  -> OpMult
    | DIV   -> OpDiv
    | EQ    -> OpEq
    | NE    -> OpNe
    | LT    -> OpLt
    | LE    -> OpLe
    | GT    -> OpGt
    | GE    -> OpGe
    | _ -> parse_err "not a binary operation"
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = '#' [^ '\n']*

let digit   = ['0'-'9']
let alpha   = ['A'-'Z' 'a'-'z']
let integer = '-'? digit+
let decimal = '-'? digit+ '.' digit+
let ident   = (alpha | '_') (alpha | digit | '_')*

rule token = parse
    | white      { token lexbuf }
    | newline    { new_line lexbuf; token lexbuf }
    | comment    { token lexbuf }
    | integer    { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | decimal    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | "."        { DOT }
    | ","        { COMMA }
    | "+"        { PLUS }
    | "-"        { MIN }
    | "*"        { MULT }
    | "/"        { DIV }
    | "="        { EQ }
    | "!="       { NE }
    | "<"        { LT }
    | "<="       { LE }
    | ">"        { GT }
    | ">="       { GE }
    | "("        { LPAREN }
    | ")"        { RPAREN }
    | "["        { LSQUARE }
    | "]"        { RSQUARE }
    | "\\"       { LAMBDA }
    | "let"      { LET }
    | "in"       { IN }
    | "if"       { IF }
    | "then"     { THEN }
    | "else"     { ELSE }
    | "shape"    { SHAPE }
    | "dim"      { DIM }
    | ident as i { ID i }
    | eof        { EOF }
    | _          { parse_err "lexing error" }
