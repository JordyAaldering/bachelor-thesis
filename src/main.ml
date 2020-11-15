open Ast
open Value
open Lexer
open Printf

let eval_prog e =
    let st, env, e = Storage.st_new (), Env.env_new (), e in
        printf "--- ORIGINAL ---\n";
        printf "%s\n\n" (expr_to_str e);
        flush stdout;
        let st, p = Evaluator.eval_expr st env e in
            printf "Result:\n%s = %s\n\n" p (value_to_str (Storage.st_lookup st p))

let eval_rewrite e =
    let st, env, e = Storage.st_new (), Env.env_new (), e in
        printf "--- REWRITE ---\n";
        let inf = Inference.infer_prog e in
        let e = Rewrite.rewrite_prog e inf in
            printf "%s\n\n" (expr_to_str e);
            printf "%s\n" (Inference.dem_env_to_str inf);
            flush stdout;
            let st, p = Evaluator.eval_expr st env e in
                printf "Result:\n%s = %s\n\n" p (value_to_str (Storage.st_lookup st p))

let main () =
    let fname = Sys.argv.(1) in
    let file = open_in fname in
    let open Lexing in
    let lexbuf = from_channel file in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname=fname };
        let e = Parser.prog lexbuf in
            eval_prog e;
            eval_rewrite e;
            close_in file

let _ = main ()
