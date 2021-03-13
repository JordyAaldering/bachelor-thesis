open Ast
open Env
open Printf

exception RewriteError of string

let rewrite_err (msg: string) =
    raise @@ RewriteError msg

(** returns an environment containing demands for all free variables *)
let rec sd (e: expr) (dem: int Array.t) (env: pv_env) : pv_env =
    match e with
    (* variables *)
    | EVar s -> Env.add s dem env
    | EFloat _ -> env
    | EArray _ -> env
    (* expressions *)
    | ELambda (x, e1) ->
        let env' = sd e1 dem env in
        let dem' = try Env.find x env'
            with Not_found -> [|0; 0; 0; 0|] in
        Env.add x dem' env'
    | EApply (EVar fid, e2) ->
        let dem' = try Env.find fid env with Not_found -> [|0; 1; 2; 3|] in
        let dem' = Array.map (Array.get dem') dem in
        sd e2 dem' env
    | EApply (_e1, e2) -> (* e1 is a lambda- or primitive expression *)
        let dem' = pv e env in
        let dem' = Array.map (Array.get dem') dem in
        sd e2 dem' env
    | ELet (fid, ELambda(s, e1), e2) ->
        let dem' = pv (ELambda (s, e1)) env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = Env.add fid dem' env in
        let env2 = Env.remove s @@ sd e2 dem env1 in
        pv_env_union env1 env2
    | ELet (s, e1, e2) ->
        let dem' = pv (ELambda (s, e2)) env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = Env.remove s @@ sd e2 dem env in
        pv_env_union env1 env2
    | ECond (e1, e2, e3) ->
        let env1 = sd e1 [|0; 3; 3; 3|] env in
        let env2 = sd e2 dem env in
        let env3 = sd e3 dem env in
        pv_env_union env1 (pv_env_union env2 env3)
    | EWith (e_gen, e_def, e_min, s, e_max, e) ->
        let dem_shp = Array.map (Array.get [|0; 2; 3; 3|]) dem in
        let dem_idx = pv (ELambda (s, e)) env in
        let dem_idx = Array.map (Array.get dem_idx) dem in
        let env_gen = sd e_gen dem_shp env in
        let env_def = sd e_def dem env in
        let env_min = sd e_min dem_idx env in
        let env_max = sd e_max dem_idx env in
        let env_e = Env.remove s @@ sd e dem env in
        pv_env_union env_gen (pv_env_union env_def (pv_env_union env_min (pv_env_union env_max env_e)))
    (* operands, and *)
    (* primitive functions *)
    | EBinary (_, e1, e2)
    | ESel (e1, e2) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = sd e2 dem' env in
        pv_env_union env1 env2
    | EUnary (_, e1)
    | EShape e1
    | EDim e1 ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get dem') dem in
        sd e1 dem' env

(** returns the demand array for the given expression *)
and pv (e: expr) (env: pv_env) : int Array.t =
    match e with
    (* expressions *)
    | ELambda (s, e1) -> (try
            let env' = sd e1 [|0; 1; 2; 3|] env in
            Env.find s env'
        with Not_found -> [|0; 1; 2; 3|]
    )
    | EApply (EVar s, e2) -> (try
            let env' = sd e2 [|0; 1; 2; 3|] env in
            Env.find s env'
        with Not_found -> [|0; 0; 0; 0|]
    )
    | EApply (e1, e2) ->
        let env' = sd e2 [|0; 1; 2; 3|] env in
        pv e1 env'
    (* operands *)
    | EBinary (op, _, _) ->
        if is_equality_bop op
        then [|0; 0; 0; 3|]
        else [|0; 1; 2; 3|]
    | EUnary (op, _) ->
        if is_equality_uop op
        then [|0; 0; 0; 3|]
        else [|0; 1; 2; 3|]
    (* primitive functions *)
    | ESel _   -> [|0; 2; 2; 3|]
    | EShape _ -> [|0; 0; 1; 2|]
    | EDim _   -> [|0; 0; 0; 1|]
    | _ ->
        rewrite_err @@ sprintf "invalid pv argument `%s'"
            (expr_to_str e)

let infer (e: expr) : pv_env =
    let env = Debug.time "Inference" (fun () ->
        sd e [|0; 1; 2; 3|] Env.empty
    ) in
    Debug.print @@ sprintf "%s\n" (pv_env_to_str env);
    env
