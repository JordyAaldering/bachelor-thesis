{
open Lexing
open Ast

type token =
    | ID of (string)
    | INT of (int)
    | TRUE
    | FALSE
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | PLUS
    | MINUS
    | MULT
    | DIV
    | MOD
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

let tok_to_str = function
    | ID (x)  -> x
    | INT (x) -> string_of_int x
    | TRUE    -> "true"
    | FALSE   -> "false"
    | LET     -> "let"
    | IN      -> "in"
    | IF      -> "if"
    | THEN    -> "then"
    | ELSE    -> "else"
    | PLUS    -> "+"
    | MINUS   -> "-"
    | MULT    -> "*"
    | DIV     -> "/"
    | MOD     -> "%"
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
    | EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    | PLUS
    | MINUS
    | MULT
    | DIV
    | MOD -> true
    | _ -> false

let op_to_binop tok = match tok with
    | PLUS  -> OpPlus
    | MINUS -> OpMinus
    | MULT  -> OpMult
    | DIV   -> OpDiv
    | MOD   -> OpMod
    | EQ    -> OpEq
    | NE    -> OpNe
    | LT    -> OpLt
    | LE    -> OpLe
    | GT    -> OpGt
    | GE    -> OpGe
    | _ -> raise (ImapFailure "op_to_binop: not a binary operation")
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = ';' [^ '\n']*

let digit   = ['0'-'9']
let integer = digit+
let alpha   = ['A'-'Z' 'a'-'z']
let ident   = (alpha | '_') (alpha | digit | '_')*

rule token = parse
    | white      { token lexbuf }
    | newline    { new_line lexbuf; token lexbuf }
    | comment    { token lexbuf }
    | integer    { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "true"     { TRUE }
    | "false"    { FALSE }
    | "let"      { LET }
    | "in"       { IN }
    | "if"       { IF }
    | "then"     { THEN }
    | "else"     { ELSE }
    | "."        { DOT }
    | ","        { COMMA }
    | "+"        { PLUS }
    | "-"        { MINUS }
    | "*"        { MULT }
    | "/"        { DIV }
    | "%"        { MOD }
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
    | ident as i { ID i }
    | eof        { EOF }
    | _          { raise (ImapFailure "Lexing error") }
