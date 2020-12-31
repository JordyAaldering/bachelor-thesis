open Ast
open Env
open Infer
open Printf

let rec rewrite_lvl e lvl inf env = match lvl with
    | 3 -> rewrite_f e inf env
    | 2 -> rewrite_s e inf env
    | 1 -> rewrite_d e inf env
    | 0 -> EFloat 0.
    | _ -> rewrite_err @@ sprintf "at %s: invalid eval level `%d'"
        (expr_to_str e) lvl

and rewrite_f e inf env = match e with
    (* variables *)
    | EVar s -> (try
            let _ = Env.find s inf in
            (* The variable is the name of a function *)
            EVar (s ^ "_f")
        with Not_found -> EVar s
    )
    | EFloat _ -> e
    | EArray _ -> e
    (* expressions *)
    | ELambda (s, e1) -> rewrite_lambda s e1 3 inf env
    | EApply (e1, e2) -> rewrite_apply e1 e2 3 inf env
    | ELet (s, e1, e2) -> rewrite_let s e1 e2 3 inf env
    | ECond (e1, e2, e3) -> rewrite_if e1 e2 e3 3 inf env
    (* operands *)
    | EBinary (op, e1, e2) -> EBinary (op, rewrite_f e1 inf env, rewrite_f e2 inf env)
    | EUnary (op, e1) -> EUnary (op, rewrite_f e1 inf env)
    (* primitive functions *)
    | ESel (e1, e2) -> ESel (rewrite_f e1 inf env, rewrite_f e2 inf env)
    | EShape e1 -> rewrite_s e1 inf env
    | EDim e1 -> rewrite_d e1 inf env
    | ERead -> ERead

and rewrite_s e inf env = match e with
    | EVar s -> (try
            let _ = Env.find s inf in
            (* The variable is the name of a function *)
            EVar (s ^ "_s")
        with Not_found -> (try
                let lvl = Env.find s env in
                if lvl = 3 then EShape e
                else if lvl = 2 then e
                else rewrite_err @@ sprintf "at %s: invalid eval level `%d'"
                        (expr_to_str e) lvl
            with Not_found ->
                rewrite_err @@ sprintf "at %s: key `%s' was not found"
                    (expr_to_str e) s
        )
    )
    | EFloat _ -> EArray []
    | EArray xs -> EArray [EFloat (float_of_int @@ List.length xs)]
    (* expressions *)
    | ELambda (s, e1) -> rewrite_lambda s e1 2 inf env
    | EApply (e1, e2) -> rewrite_apply e1 e2 2 inf env
    | ELet (s, e1, e2) -> rewrite_let s e1 e2 2 inf env
    | ECond (e1, e2, e3) -> rewrite_if e1 e2 e3 2 inf env
    (* operands *)
    | EBinary (op, e1, _) -> (match op with
        | OpAppend | OpAdd | OpMin | OpMul | OpDiv 
            -> rewrite_s e1 inf env
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
            -> EArray []
    )
    | EUnary (op, e1) -> (match op with
        | OpNeg -> rewrite_s e1 inf env
        | OpNot -> EArray []
    )
    (* primitive functions *)
    | ESel _ -> EArray []
    | EShape e1 -> EArray [rewrite_d e1 inf env]
    | EDim _ -> EFloat 1.
    | ERead -> ERead

and rewrite_d e inf env = match e with
    | EVar s -> (try
            let _ = Env.find s inf in
            (* The variable is the name of a function *)
            EVar (s ^ "_d")
        with Not_found -> (try
                let lvl = Env.find s env in
                if lvl = 3 then EDim e
                else if lvl = 2 then ESel (EShape e, EFloat 0.)
                else if lvl = 1 then e
                else EFloat 0.
            with Not_found ->
                rewrite_err @@ sprintf "at %s: key `%s' was not found"
                    (expr_to_str e) s
        )
    )
    | EFloat _ -> EFloat 0.
    | EArray _ -> EFloat 1.
    (* expressions *)
    | ELambda (s, e1) -> rewrite_lambda s e1 1 inf env
    | EApply (e1, e2) -> rewrite_apply e1 e2 1 inf env
    | ELet (s, e1, e2) -> rewrite_let s e1 e2 1 inf env
    | ECond (e1, e2, e3) -> rewrite_if e1 e2 e3 1 inf env
    (* operands *)
    | EBinary (op, e1, _) -> (match op with
        | OpAppend | OpAdd | OpMin | OpMul | OpDiv
            -> rewrite_d e1 inf env
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
            -> EFloat 0.
    )
    | EUnary (op, e1) -> (match op with
        | OpNeg -> rewrite_d e1 inf env
        | OpNot -> EFloat 0.
    )
    (* primitive functions *)
    | ESel _ -> EFloat 0.
    | EShape _ -> EFloat 1.
    | EDim _ -> EFloat 0.
    | ERead -> ERead

and rewrite_lambda s e1 lvl inf env =
    let dem = pv (ELambda (s, e1)) inf in
    let lvl' = Array.get dem lvl in
    let env' = Env.add s lvl' env in
    ELambda (s, rewrite_lvl e1 lvl inf env')

and rewrite_apply e1 e2 lvl inf env = match e1 with
    | ELambda (s, e') ->
        let dem = pv e1 inf in
        let lvl' = Array.get dem lvl in
        if lvl' = 0 then
            rewrite_lvl e' lvl inf env
        else
            let env' = Env.add s lvl' env in
            EApply (ELambda (s, rewrite_lvl e' lvl inf env'), rewrite_lvl e2 lvl' inf env)
    | EVar s -> (* function call *)
        let dem = Env.find s inf in
        let lvl' = Array.get dem lvl in
        let fid = s ^ if lvl = 3 then "_f"
            else if lvl = 2 then "_s"
            else if lvl = 1 then "_d"
            else "_d"
        in
        EApply (EVar fid, rewrite_lvl e2 lvl' inf env)
    | EApply (EVar s, e') ->
        let dem = Env.find s inf in
        let lvl' = Array.get dem lvl in
        let fid = s ^ if lvl' = 3 then "_f"
            else if lvl' = 2 then "_s"
            else if lvl' = 1 then "_d"
            else "_d"
        in
        EApply (EApply (EVar fid, rewrite_lvl e' lvl' inf env), rewrite_lvl e2 lvl' inf env)
    | _ ->
        let dem = pv e1 inf in
        let lvl' = Array.get dem lvl in
        EApply (rewrite_lvl e1 lvl' inf env, rewrite_lvl e2 lvl' inf env)

and rewrite_let s e1 e2 lvl inf env =
    let dem = pv (ELambda (s, e2)) inf in
    let lvl' = Array.get dem lvl in
    let env' = Env.add s lvl' env in
    if lvl' = 0 then
        rewrite_lvl e2 lvl inf env'
    else (match e1 with
        | ELambda _ ->
            ELet (s ^ "_f", rewrite_f e1 inf env, 
            ELet (s ^ "_s", rewrite_s e1 inf env,
            ELet (s ^ "_d", rewrite_d e1 inf env,
                rewrite_lvl e2 lvl inf env')))
        | _ ->
            ELet (s, rewrite_lvl e1 lvl' inf env,
                rewrite_lvl e2 lvl inf env')
    )

and rewrite_if e1 e2 e3 lvl inf env =
    ECond (rewrite_f e1 inf env,
        rewrite_lvl e2 lvl inf env,
        rewrite_lvl e3 lvl inf env)

let rewrite e =
    let inf = infer e in
    let e = rewrite_f e inf Env.empty in
    printf "Rewrite:\n%s\n\n" (expr_to_str e);
    e
