open Src.Parser
open Src.Eval
open Src.Rewrite
open Printf

let eval_original e =
    printf "\n--- ORIGINAL ---\n";
    let _ = eval e in ()

let eval_rewrite e =
    printf "\n--- REWRITE ---\n";
    let _ = eval @@ rewrite e in ()

let () =
    let e = parse Sys.argv.(1) in
        eval_original e;
        eval_rewrite e
