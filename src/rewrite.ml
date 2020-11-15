open Sd
open Ast
open Printf

module Eval_env = Map.Make(String)
type eval_env = int Eval_env.t


exception RewriteFailure of string
let rewrite_err msg = raise @@ RewriteFailure msg
let rewrite_err_in e msg = raise @@ RewriteFailure (sprintf "%s: %s" (expr_to_str e) msg)


let rec rewrite: expr -> int -> int Eval_env.t -> expr = fun e lvl env -> match lvl with
    | 3 -> rewrite_f e env
    | 2 -> rewrite_s e env
    | 1 -> rewrite_d e env
    | 0 -> EConst 0.
    | _ -> rewrite_err_in e @@ sprintf "invalid eval level `%d'" lvl

and rewrite_f: expr -> int Eval_env.t -> expr = fun e env -> match e with
    | EVar x -> e
    | EConst x -> e
    | EArray x -> e

    | ELambda (x, e1) ->
        let dem = pv e Dem_env.empty in
        let lvl = Array.get (List.hd dem) 3 in
        let env' = Eval_env.add x lvl env in
        ELambda (x, rewrite_f e1 env')
    | ELetIn (x, e1, e2) ->
        let dem = pv (ELambda (x, e2)) Dem_env.empty in
        let lvl = Array.get (List.hd dem) 3 in
        if lvl = 0 then
            rewrite_f e2 env
        else
            let env' = Eval_env.add x lvl env in
            ELetIn (x, rewrite e1 lvl env, rewrite_f e2 env')

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

    | ELambda (x, e1) ->
        let dem = pv e Dem_env.empty in
        let lvl = Array.get (List.hd dem) 2 in
        let env' = Eval_env.add x lvl env in
        ELambda (x, rewrite_s e1 env')
    | ELetIn (x, e1, e2) ->
        let dem = pv (ELambda (x, e2)) Dem_env.empty in
        let lvl = Array.get (List.hd dem) 2 in
        if lvl = 0 then
            rewrite_s e2 env
        else
            let env' = Eval_env.add x lvl env in
            ELetIn (x, rewrite e1 lvl env, rewrite_s e2 env')

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

    | ELambda (x, e1) ->
        let dem = pv e Dem_env.empty in
        let lvl = Array.get (List.hd dem) 1 in
        let env' = Eval_env.add x lvl env in
        ELambda (x, rewrite_d e1 env')
    | ELetIn (x, e1, e2) ->
        let dem = pv (ELambda (x, e2)) Dem_env.empty in
        let lvl = Array.get (List.hd dem) 1 in
        if lvl = 0 then
            rewrite_d e2 env
        else
            let env' = Eval_env.add x lvl env in
            ELetIn (x, rewrite e1 lvl env, rewrite_d e2 env')

    | _ -> e

let rewrite_prog: expr -> expr = fun e ->
    rewrite_f e Eval_env.empty
