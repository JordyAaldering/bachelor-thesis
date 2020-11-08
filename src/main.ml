open Ast
open Lexer
open Printf

let fname = ref ""
let fname_set = ref false

let eval_prog e =
    let st, env, e = (Storage.st_new (), Env.env_new (), e)
    in
    printf "%s\n" (Print.expr_to_str e);
    flush stdout;
    let st, p = Eval.eval st env e in
    printf "%s\n" (Storage.st_to_str st);
    printf "res: %s = %s\n\n"  p (Print.value_to_str @@ Storage.st_lookup st p)

let main () =
    Arg.parse
        []
        (fun x -> if !fname_set then
                    raise (ImapFailure "Multiple input files found on command line")
                  else begin
                      fname := x; fname_set := true
                  end)
        "usage: ";
    let file = if !fname_set
               then open_in !fname
               else (fname := "<stdin>"; fname_set := true; stdin) in
    let open Lexing in
    let lexbuf = from_channel file in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !fname };
    let e = Parser.prog lexbuf in
    let _, e = Traversal.topdown () e in
    begin
        eval_prog e
    end;
    close_in file

let _ = main ()
