open Parser
open Eval
open Inference
open Rewrite
open Printf

let eval_orig e =
    printf "\n--- ORIGINAL ---\n";
    eval_prog e

let eval_rewrite e =
    printf "\n--- REWRITE ---\n";
    let env = infer_prog e in
    let e = rewrite_prog e env in
    eval_prog e

let () =
    let file = open_in (Sys.argv.(1) ^ ".txt") in
    let lexbuf = Lexing.from_channel file in
    let e = parse_prog lexbuf in
        eval_orig e;
        eval_rewrite e;
        close_in file
