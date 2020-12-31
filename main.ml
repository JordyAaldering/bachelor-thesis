open Src.Parser
open Src.Eval
open Src.Rewrite
open Printf

let eval_orig e =
    printf "\n--- ORIGINAL ---\n";
    eval_prog e

let eval_rewrite e =
    printf "\n--- REWRITE ---\n";
    let e = rewrite_prog e in
    eval_prog e

let () =
    let file = open_in (Sys.argv.(1) ^ ".txt") in
    let e = parse_prog file in
        eval_orig e;
        eval_rewrite e;
        close_in file
