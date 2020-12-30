open Ast
open Env
open Infer
open Printf

let rec rewrite e lvl inf env = match lvl with
    | 3 -> rewrite_f e inf env
    | 2 -> rewrite_s e inf env
    | 1 -> rewrite_d e inf env
    | 0 -> EFloat 0.
    | _ -> rewrite_err @@ sprintf "at %s: invalid eval level `%d'" (expr_to_str e) lvl

and rewrite_f e inf env = match e with
    | EVar x ->
        if try let _ = Env.find x inf in true with Not_found -> false; then
            EVar (x ^ "_f") (* The variable is the name of a function *)
        else EVar x
    | EFloat _x -> e
    | EArray _xs -> e

    | ELambda _ -> rewrite_lambda e 3 inf env
    | EApply _ -> rewrite_apply e 3 inf env
    | ELetIn _ -> rewrite_let e 3 inf env
    | EIfThen _ -> rewrite_if e 3 inf env

    | EBinary (op, e1, e2) -> EBinary (op, rewrite_f e1 inf env, rewrite_f e2 inf env)
    | EUnary (op, e1) -> EUnary (op, rewrite_f e1 inf env)
    | ESel (e1, e2) -> ESel (rewrite_f e1 inf env, rewrite_f e2 inf env)
    | EShape e1 -> rewrite_s e1 inf env
    | EDim e1 -> rewrite_d e1 inf env
    | ERead -> ERead

and rewrite_s e inf env = match e with
    | EVar x ->
        if try let _ = Env.find x inf in true with Not_found -> false; then
            EVar (x ^ "_s") (* The variable is the name of a function *)
        else
        begin try
            let lvl = Env.find x env in
            if lvl = 3 then EShape e
            else if lvl = 2 then e
            else rewrite_err @@ sprintf "at %s: invalid eval level `%d'" (expr_to_str e) lvl
        with Not_found ->
            rewrite_err @@ sprintf "at %s: key `%s' was not found" (expr_to_str e) x
    end
    | EFloat _x -> EArray []
    | EArray xs -> EArray [EFloat (float_of_int @@ List.length xs)]

    | ELambda _ -> rewrite_lambda e 2 inf env
    | EApply _ -> rewrite_apply e 2 inf env
    | ELetIn _ -> rewrite_let e 2 inf env
    | EIfThen _ -> rewrite_if e 2 inf env

    | EBinary (op, e1, _e2) -> begin match op with
        | OpAdd | OpMin | OpMul | OpDiv
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
    | EDim _ -> EFloat 1.
    | ERead -> ERead

and rewrite_d e inf env = match e with
    | EVar x ->
        if try let _ = Env.find x inf in true with Not_found -> false; then
            EVar (x ^ "_d") (* The variable is the name of a function *)
        else
        begin try
            let lvl = Env.find x env in
            if lvl = 3 then EDim e
            else if lvl = 2 then ESel (EShape e, EFloat 0.)
            else if lvl = 1 then e
            else EFloat 0.
        with Not_found ->
            rewrite_err @@ sprintf "at %s: key `%s' was not found" (expr_to_str e) x
    end
    | EFloat _x -> EFloat 0.
    | EArray _xs -> EFloat 1.

    | ELambda _ -> rewrite_lambda e 1 inf env
    | EApply _ -> rewrite_apply e 1 inf env
    | ELetIn _ -> rewrite_let e 1 inf env
    | EIfThen _ -> rewrite_if e 1 inf env

    | EBinary (op, e1, _e2) -> begin match op with
        | OpAdd | OpMin | OpMul | OpDiv
            -> rewrite_d e1 inf env
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
            -> EFloat 0.
    end
    | EUnary (op, e1) -> begin match op with
        | OpNeg -> rewrite_d e1 inf env
        | OpNot -> EFloat 0.
    end
    | ESel _ -> EFloat 0.
    | EShape _ -> EFloat 1.
    | EDim _ -> EFloat 0.
    | ERead -> ERead

and rewrite_lambda e lvl inf env = match e with
    | ELambda (x, e1) ->
        let dem = pv e inf in
        let lvl' = Array.get dem lvl in
        let env' = Env.add x lvl' env in
        ELambda (x, rewrite e1 lvl inf env')
    | _ -> assert false

and rewrite_apply e lvl inf env = match e with
    | EApply (ELambda (x, e1), e2) ->
        let dem = pv (ELambda (x, e1)) inf in
        let lvl' = Array.get dem lvl in
        if lvl' = 0 then
            rewrite e1 lvl inf env
        else
            let env' = Env.add x lvl' env in
            EApply (ELambda (x, rewrite e1 lvl inf env'), rewrite e2 lvl' inf env)
    | EApply (EVar x, e2) -> (* function call *)
        let dem = Env.find x inf in
        let lvl' = Array.get dem lvl in
        let fid = x ^ if lvl = 3 then "_f"
            else if lvl = 2 then "_s"
            else if lvl = 1 then "_d"
            else "_d"
        in
        EApply (EVar fid, rewrite e2 lvl' inf env)
    | EApply (EApply (EVar x, e1), e2) ->
        let dem = Env.find x inf in
        let lvl' = Array.get dem lvl in
        let fid = x ^ if lvl' = 3 then "_f"
            else if lvl' = 2 then "_s"
            else if lvl' = 1 then "_d"
            else "_d"
        in
        EApply (EApply (EVar fid, rewrite e1 lvl' inf env), rewrite e2 lvl' inf env)
    | EApply (e1, e2) ->
        let dem = pv e1 inf in
        let lvl' = Array.get dem lvl in
        EApply (rewrite e1 lvl' inf env, rewrite e2 lvl' inf env)
    | _ -> assert false

and rewrite_let e lvl inf env = match e with
    | ELetIn (fid, ELambda (var, e1), e2) ->
        let dem = pv (ELambda (fid, e2)) inf in
        let lvl' = Array.get dem lvl in
        let env' = Env.add fid lvl' env in
        if lvl' = 0 then
            rewrite e2 lvl inf env
        else
            let lmb = ELambda (var, e1) in
            ELetIn (fid ^ "_f", rewrite_f lmb inf env, 
            ELetIn (fid ^ "_s", rewrite_s lmb inf env,
            ELetIn (fid ^ "_d", rewrite_d lmb inf env,
                rewrite e2 lvl inf env')))
    | ELetIn (x, e1, e2) ->
        let dem = pv (ELambda (x, e2)) inf in
        let lvl' = Array.get dem lvl in
        let env' = Env.add x lvl' env in
        if lvl' = 0 then
            rewrite e2 lvl inf env'
        else
            ELetIn (x, rewrite e1 lvl' inf env, rewrite e2 lvl inf env')
    | _ -> assert false

and rewrite_if e lvl inf env = match e with
    | EIfThen (ec, et, ef) ->
        EIfThen (rewrite_f ec inf env,
            rewrite et lvl inf env,
            rewrite ef lvl inf env)
    | _ -> assert false

let rewrite_prog e inf =
    let e = rewrite_f e inf Env.empty in
    printf "Rewrite:\n%s\n\n" (expr_to_str ~newline:true e);
    e
