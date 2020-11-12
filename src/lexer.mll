{
open Ast
open Lexing

type token =
    | ID of string
    | INT of int
    | FLOAT of float
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | PLUS
    | MINUS
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
    | LET     -> "let"
    | IN      -> "in"
    | IF      -> "if"
    | THEN    -> "then"
    | ELSE    -> "else"
    | PLUS    -> "+"
    | MINUS   -> "-"
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
    | MINUS
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
    | MINUS -> OpMinus
    | MULT  -> OpMult
    | DIV   -> OpDiv
    | EQ    -> OpEq
    | NE    -> OpNe
    | LT    -> OpLt
    | LE    -> OpLe
    | GT    -> OpGt
    | GE    -> OpGe
    | _ -> raise @@ ParseFailure "not a binary operation"
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = '#' [^ '\n']*

let digit   = ['0'-'9']
let alpha   = ['A'-'Z' 'a'-'z']
let integer = digit+
let decimal = digit+ '.' digit+
let ident   = (alpha | '_') (alpha | digit | '_')*

rule token = parse
    | "."        { DOT }
    | ","        { COMMA }
    | "+"        { PLUS }
    | "-"        { MINUS }
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
    | "let"      { LET }
    | "in"       { IN }
    | "if"       { IF }
    | "then"     { THEN }
    | "else"     { ELSE }
    | white      { token lexbuf }
    | newline    { new_line lexbuf; token lexbuf }
    | comment    { token lexbuf }
    | integer    { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | decimal    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | ident as i { ID i }
    | eof        { EOF }
    | _          { raise @@ ParseFailure "Lexing error" }
