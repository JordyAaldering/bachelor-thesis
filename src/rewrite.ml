open Sd
open Ast
open Printf

module Eval_env = Map.Make(String)

exception RewriteFailure of string

let rewrite_err msg = raise @@ RewriteFailure msg

let rewrite_err_in e msg = raise @@ RewriteFailure (sprintf "%s: %s" (expr_to_str e) msg)

let rec rewrite_f: expr -> int Eval_env.t -> expr = fun e env -> match e with
    | EVar x -> e
    | EConst x -> e
    | EArray x -> e

    | _ -> e

and rewrite_s: expr -> int Eval_env.t -> expr = fun e env -> match e with
    | EVar x -> begin
        try
            let lvl = Eval_env.find x env in
            if lvl = 3 then
                EShape e
            else if lvl = 2 then
                e
            else
                rewrite_err_in e @@ sprintf "invalid eval level `%d'" lvl
        with Not_found ->
            rewrite_err_in e @@ sprintf "key `%s' was not found" x
    end

    | EConst x -> EShape e
    | EArray x -> EShape e

    | _ -> e

and rewrite_d: expr -> int Eval_env.t -> expr = fun e env -> match e with
    | EVar x -> begin
        try
            let lvl = Eval_env.find x env in
            if lvl = 3 then
                EDim e
            else if lvl = 2 then
                ESel (EShape e, EConst 0.)
            else if lvl = 1 then
                e
            else
                rewrite_err_in e @@ sprintf "invalid eval level `%d'" lvl
        with Not_found ->
            rewrite_err_in e @@ sprintf "key `%s' was not found" x
    end

    | EConst x -> EDim e
    | EArray x -> EDim e

    | _ -> e

let rewrite_prog: expr -> expr = fun e ->
    rewrite_f e Eval_env.empty
