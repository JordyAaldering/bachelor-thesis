{
open Token
open Lexing
open Printf
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = '#' [^ '\n']*

let digit   = ['0'-'9']
let alpha   = ['A'-'Z' 'a'-'z' '_']

let id      = alpha (alpha | digit)*
let int     = '-'? digit+
let float   = int '.' digit+

rule token = parse
    (* ignore *)
    | white     { token lexbuf }
    | comment   { token lexbuf }
    | newline   { new_line lexbuf; token lexbuf }
    (* expressions *)
    | "\\"      { LAMBDA }
    | "let"     { LET }
    | "in"      { IN }
    | "if"      { IF }
    | "then"    { THEN }
    | "else"    { ELSE }
    | "gen"     { GEN }
    | "with"    { WITH }
    (* primitive functions *)
    | "shape"   { SHAPE }
    | "dim"     { DIM }
    | "read"    { READ }
    (* variables *)
    | id as i   { ID i }
    | int       { INT (int_of_string @@ lexeme lexbuf) }
    | float     { FLOAT (float_of_string @@ lexeme lexbuf) }
    (* operands *)
    | "@"       { CONCAT }
    | "+"       { ADD }
    | "-"       { MIN }
    | "*"       { MUL }
    | "/"       { DIV }
    | "="       { EQ }
    | "!="      { NE }
    | "<"       { LT }
    | "<="      { LE }
    | ">"       { GT }
    | ">="      { GE }
    (* symbols *)
    | "."       { DOT }
    | ","       { COMMA }
    | "("       { LPAREN }
    | ")"       { RPAREN }
    | "["       { LSQUARE }
    | "]"       { RSQUARE }
    | eof       { EOF }
    | _         { parse_err @@ sprintf "unexpected symbol `%s'" (lexeme lexbuf) }
