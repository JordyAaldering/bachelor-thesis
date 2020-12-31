open Src.Parser
open Src.Eval
open Src.Rewrite
open Printf

let eval_original e =
    printf "\n--- ORIGINAL ---\n";
    eval e

let eval_rewrite e =
    printf "\n--- REWRITE ---\n";
    let e = rewrite e in
    eval e

let () =
    let e = parse Sys.argv.(1) in
        eval_original e;
        eval_rewrite e
