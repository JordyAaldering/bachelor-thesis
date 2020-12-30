open Ast
open Env
open Printf

exception RewriteError of string

let rewrite_err msg =
    raise @@ RewriteError msg

let rec sd e dem env = match e with
    (* variables *)
    | EVar s -> Env.add s dem env
    | EFloat _x -> env
    | EArray _xs -> env
    (* expressions *)
    | ELambda (x, e1) ->
        let env' = sd e1 dem env in
        let dem' = try Env.find x env'
            with Not_found -> [|0; 0; 0; 0|] in
        Env.add x dem' env'
    | EApply (EVar fun_id, e2) ->
        let dem' = try Env.find fun_id env
            with Not_found -> [|0; 1; 2; 3|] in
        let dem' = Array.map (Array.get dem') dem in
        sd e2 dem' env
    | EApply (e1, e2) ->
        (* e1 is a lambda- or primitive expression *)
        let dem' = pv e1 env in
        let dem' = Array.map (Array.get dem') dem in
        sd e2 dem' env
    | ELetIn (fun_id, ELambda(s, e1), e2) ->
        let dem' = pv (ELambda (s, e1)) env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = Env.add fun_id dem' env in
        let env2 = sd e2 dem env1 in
        let env2 = Env.remove s env2 in
        pv_env_union env1 env2
    | ELetIn (s, e1, e2) ->
        let dem' = pv (ELambda (s, e2)) env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = Env.remove s @@ sd e2 dem env in
        pv_env_union env1 env2
    | EIfThen (e1, e2, e3) ->
        let env1 = sd e1 [|0; 3; 3; 3|] env in
        let env2 = sd e2 dem env in
        let env3 = sd e3 dem env in
        pv_env_union env1 (pv_env_union env2 env3)
    (* operands *)
    | EBinary (_op, e1, e2) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = sd e2 dem' env in
        pv_env_union env1 env2
    | EUnary (_op, e1) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get dem') dem in
        sd e1 dem' env
    (* primitive functions *)
    | ESel (e1, e2) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = sd e2 dem' env in
        pv_env_union env1 env2
    | EShape e1
    | EDim e1 ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get dem') dem in
        sd e1 dem' env
    | ERead -> env

and pv e env = match e with
    (* expressions *)
    | ELambda (s, e1)
    | EApply (EVar s, e1) -> (try
            let env' = sd e1 [|0; 1; 2; 3|] env in
            Env.find s env'
        with Not_found ->
            [|0; 1; 2; 3|]
    )
    | EApply (e1, e2) ->
        let env' = sd e2 [|0; 1; 2; 3|] env in
        pv e1 env'
    (* operands *)
    | EBinary (op, _, _) -> (match op with
        | OpAdd | OpMin | OpMul | OpDiv -> [|0; 1; 2; 3|]
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe -> [|0; 0; 0; 3|]
    )
    | EUnary (op, _) -> (match op with
        | OpNeg -> [|0; 1; 2; 3|]
        | OpNot -> [|0; 0; 0; 3|]
    )
    (* primitive functions *)
    | ESel _ -> [|0; 2; 2; 3|]
    | EShape _ -> [|0; 0; 1; 2|]
    | EDim _ -> [|0; 0; 0; 1|]
    | _ -> rewrite_err @@ sprintf "invalid PV argument `%s'"
            (expr_to_str e)

let infer e =
    let env = sd e [|0; 1; 2; 3|] Env.empty in
    printf "Demand environment:\n%s\n\n" (pv_env_to_str env);
    env
