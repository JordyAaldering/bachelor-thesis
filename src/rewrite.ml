open Ast
open Inference
open Printf

module Eval_env = Map.Make(String)
type eval_env = int Eval_env.t

exception RewriteFailure of string
let rewrite_err msg = raise @@ RewriteFailure msg
let rewrite_err_in e msg = raise @@ RewriteFailure (sprintf "%s: %s" (expr_to_str e) msg)

let rec rewrite: expr -> int -> dem_env -> eval_env -> expr = fun e lvl inf env -> match lvl with
    | 3 -> rewrite_f e inf env
    | 2 -> rewrite_s e inf env
    | 1 -> rewrite_d e inf env
    | 0 -> EConst 0.
    | _ -> rewrite_err_in e @@ sprintf "invalid eval level `%d'" lvl

and rewrite_f: expr -> dem_env -> eval_env -> expr = fun e inf env -> match e with
    | EVar x -> e
    | EConst x -> e
    | EArray x -> e

    | ELambda (x, e1) -> rewrite_lambda e 3 inf env
    | EApply (x, e1) -> rewrite_apply e 3 inf env
    | ELetIn (x, e1, e2) -> rewrite_let e 3 inf env
    | EIfThen _ -> rewrite_if e 3 inf env

    | EBinary (op, e1, e2) -> EBinary (op, rewrite_f e1 inf env, rewrite_f e2 inf env)
    | EUnary (op, e1) -> EUnary (op, rewrite_f e1 inf env)
    | ESel (e1, e2) -> ESel (rewrite_f e1 inf env, rewrite_f e2 inf env)
    | EShape e1 -> rewrite_s e1 inf env
    | EDim e1 -> rewrite_d e1 inf env

and rewrite_s: expr -> dem_env -> eval_env -> expr = fun e inf env -> match e with
    | EVar x -> begin try
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

    | ELambda (x, e1) -> rewrite_lambda e 2 inf env
    | EApply (x, e1) -> rewrite_apply e 2 inf env
    | ELetIn (x, e1, e2) -> rewrite_let e 2 inf env
    | EIfThen _ -> rewrite_if e 2 inf env

    | EBinary (op, e1, e2) -> begin match op with
        | OpPlus | OpMin | OpMult | OpDiv
            -> rewrite_s e1 inf env
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
            -> EArray []
    end
    | EUnary (op, e1) -> begin match op with
        | OpNeg -> rewrite_s e1 inf env
        | OpNot -> EArray []
    end
    | ESel _ -> EArray []
    | EShape e1 -> EArray [rewrite_d e1 inf env]
    | EDim _ -> EConst 1.

and rewrite_d: expr -> dem_env -> eval_env -> expr = fun e inf env -> match e with
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

    | ELambda (x, e1) -> rewrite_lambda e 1 inf env
    | EApply (x, e1) -> rewrite_apply e 1 inf env
    | ELetIn (x, e1, e2) -> rewrite_let e 1 inf env
    | EIfThen _ -> rewrite_if e 1 inf env

    | EBinary (op, e1, e2) -> begin match op with
        | OpPlus | OpMin | OpMult | OpDiv
            -> rewrite_d e1 inf env
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
            -> EConst 0.
    end
    | EUnary (op, e1) -> begin match op with
        | OpNeg -> rewrite_d e1 inf env
        | OpNot -> EConst 0.
    end
    | ESel _ -> EConst 0.
    | EShape _ -> EConst 1.
    | EDim _ -> EConst 0.

and rewrite_lambda: expr -> int -> dem_env -> eval_env -> expr = fun e lvl inf env -> match e with
    | ELambda (x, e1) ->
        let dem = pv e inf in
        let lvl' = Array.get (List.hd dem) lvl in
        let env' = Eval_env.add x lvl' env in
        ELambda (x, rewrite_d e1 inf env')
    | _ -> assert false

and rewrite_apply: expr -> int -> dem_env -> eval_env -> expr = fun e lvl inf env -> match e with
    | EApply (ELambda (x, e1), e2) ->
        let dem = pv (ELambda (x, e1)) inf in
        let lvl' = Array.get (List.hd dem) lvl in
        if lvl' = 0 then
            rewrite e1 lvl inf env
        else
            EApply (rewrite (ELambda (x, e1)) lvl inf env, rewrite e2 lvl' inf env)
    | EApply (e1, e2) -> (* TODO: temporary implementation *)
        EApply (rewrite_f e1 inf env, rewrite_f e2 inf env)
    | _ -> assert false

and rewrite_let: expr -> int -> dem_env -> eval_env -> expr = fun e lvl inf env -> match e with
    | ELetIn (x, e1, e2) ->
        let dem = pv (ELambda (x, e2)) inf in
        let lvl' = Array.get (List.hd dem) lvl in
        if lvl' = 0 then
            rewrite e2 lvl inf env
        else
            let env' = Eval_env.add x lvl' env in
            ELetIn (x, rewrite e1 lvl' inf env, rewrite e2 lvl inf env')
    | _ -> assert false

and rewrite_if: expr -> int -> dem_env -> eval_env -> expr = fun e lvl inf env -> match e with
    | EIfThen (ec, et, ef) -> EIfThen (rewrite_f ec inf env, rewrite et lvl inf env, rewrite ef lvl inf env)
    | _ -> assert false

let rewrite_prog: expr -> expr = fun e ->
    let inf = infer_prog e in
    rewrite_f e inf Eval_env.empty
