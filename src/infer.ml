open Ast
open Env
open Printf

exception InferenceFailure of string

type pv_env = int array Env.t

let infer_err msg =
    raise @@ InferenceFailure msg

let pv_env_to_str env =
    if Env.is_empty env then "[]"
    else
        Env.fold (fun k v tail ->
            sprintf "%s -> %s%s" k
                (sprintf "[%s]" (String.concat ", " @@ Array.to_list @@ Array.map string_of_int v))
                (if tail = "" then "" else ", " ^ tail)
        ) env ""

let pv_env_union =
    Env.union (fun _key x y ->
        Some (Array.map2 max x y)
    )

let rec sd e dem env = match e with
    | EVar x -> Env.add x dem env
    | ENum _x -> env
    | EArray _xs -> env

    | ELambda (x, e1) ->
        let env' = sd e1 dem env in
        let demx = try Env.find x env'
            with Not_found -> [|0; 0; 0; 0|]
        in
        Env.add x demx env'
    | EApply (EVar fun_id, e2) ->
        let dem' = try Env.find fun_id env
            with Not_found -> [|0; 1; 2; 3|]
        in
        let dem' = Array.map (Array.get dem') dem in
        sd e2 dem' env
    | EApply (e1, e2) -> (* e1 is a lambda- or primitive expression *)
        let dem' = pv e1 env in
        let dem' = Array.map (Array.get dem') dem in
        sd e2 dem' env
    | ELetIn (fun_id, ELambda(x, e1), e2) ->
        let dem' = pv (ELambda (x, e1)) env in
        let dem' = Array.map (Array.get dem') dem in
        let env' = Env.add fun_id dem' env in
        let env2 = sd e2 dem env' in
        let env2' = Env.remove x env2 in
        pv_env_union env' env2'
    | ELetIn (x, e1, e2) ->
        let dem' = pv (ELambda (x, e2)) env in
        let dem' = Array.map (Array.get dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = Env.remove x @@ sd e2 dem env in
        pv_env_union env1 env2
    | EIfThen (e1, e2, e3) ->
        let env1 = sd e1 [|0; 3; 3; 3|] env in
        let env2 = sd e2 dem env in
        let env3 = sd e3 dem env in
        pv_env_union env1 (pv_env_union env2 env3)

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

and pv e env = match e with
    | ELambda (x, e1)
    | EApply (EVar x, e1) -> begin try
            let env' = sd e1 [|0; 1; 2; 3|] env in
            Env.find x env'
        with Not_found ->
            [|0; 1; 2; 3|]
        end
    | EApply (e1, e2) ->
        let env' = sd e2 [|0; 1; 2; 3|] env in
        pv e1 env'
    
    | EBinary (op, _e1, _e2) -> begin match op with
        | OpAdd | OpMin | OpMul | OpDiv ->
            [|0; 1; 2; 3|]
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe ->
            [|0; 0; 0; 3|]
    end
    | EUnary (op, _e1) -> begin match op with
        | OpNeg -> [|0; 1; 2; 3|]
        | OpNot -> [|0; 0; 0; 3|]
    end
    | ESel (_e1, _e2) -> [|0; 2; 2; 3|]
    | EShape _e1 -> [|0; 0; 1; 2|]
    | EDim _e1   -> [|0; 0; 0; 1|]

    | _ -> infer_err @@ sprintf "invalid PV argument `%s'"
            (expr_to_str e)

let infer_prog e =
    let env = sd e [|0; 1; 2; 3|] Env.empty in
    printf "Demand environment:\n%s\n\n" (pv_env_to_str env);
    env
