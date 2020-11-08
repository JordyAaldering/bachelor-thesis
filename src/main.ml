open Lexer
open Parser
open Traversal
open Storage
open Valueops
open Printf

let fname = ref ""
let fname_set = ref false

let main () =
    let file = (fname := "<stdin>"; fname_set := true; stdin) in
    let open Lexing in
    let lexbuf = from_channel file in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !fname };
    let e = Parser.prog lexbuf in
    let m, e = Traversal.topdown () e in
    close_in file

let _ = main ()
