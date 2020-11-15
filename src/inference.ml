open Ast
open Printf

module Dem_env = Map.Make(String)
type demand = int array
type dem_env = demand list Dem_env.t

exception InferenceFailure of string
let infer_err msg = raise @@ InferenceFailure msg

let rec sd: expr -> demand -> dem_env -> dem_env = fun e dem env -> match e with
    | EVar x -> printf "adding %s\n" x; Dem_env.add x [dem] env
    | EConst _x -> env
    | EArray _xs -> env

    | ELambda (x, e1) ->
        let env' = sd e1 dem env in
        let demx = Dem_env.find x env' in
        Dem_env.add x demx env'
    | EApply (EVar fun_id, e2) -> begin try
            let dem' = Dem_env.find fun_id env in
            let dem' = Array.map (Array.get @@ List.hd dem') dem in
            sd e2 dem' env
        with Not_found ->
            printf "Could not find `%s' in env\n" fun_id;
            sd e2 [|0; 1; 2; 3|] env
    end
    | EApply (e1, e2) -> (* e1 is a lambda- or primitive expression *)
        let dem' = pv e1 env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        sd e2 dem' env
    | ELetIn (x, e1, e2) ->
        let dem' = pv (ELambda (x, e2)) env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        let env' = Dem_env.add x [dem] env in
        let env1 = sd e1 dem' env' in
        let env2 = Dem_env.remove x @@ sd e2 dem env' in
        Dem_env.union (fun key x y ->
            Some (List.map2 max x y)
        ) env1 env2
    | EIfThen (ec, et, ef) ->
        let envc = sd ec [|0; 3; 3; 3|] env in
        let envt = sd et dem env in
        let envf = sd ef dem env in
        Dem_env.union (fun key x y ->
            Some (List.map2 max x y)
        ) envc @@
            Dem_env.union (fun key x y ->
                Some (List.map2 max x y)
            ) envt envf

    | EBinary (op, e1, e2) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = sd e2 dem' env in
        Dem_env.union (fun key x y ->
            Some (List.map2 max x y)
        ) env1 env2
    | EUnary (op, e1) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        sd e1 dem' env
    | ESel (e1, e2) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = sd e2 dem' env in
        Dem_env.union (fun key x y ->
            Some (List.map2 max x y)
        ) env1 env2
    | EShape e1
    | EDim e1 ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        sd e1 dem' env

and pv: expr -> dem_env -> demand list = fun e env -> match e with
    | ELambda (x, e1) ->
        let env' = sd e1 [|0; 1; 2; 3|] env in
        Dem_env.find x env'
    
    | EBinary (op, _e1, _e2) -> begin match op with
        | OpPlus | OpMin | OpMult | OpDiv ->
            [ [|0; 1; 2; 3|]; [|0; 1; 2; 3|] ]
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe ->
            [ [|0; 0; 0; 3|]; [|0; 0; 0; 3|] ]
    end
    | EUnary (op, _e1) -> begin match op with
        | OpNeg -> [ [|0; 1; 2; 3|] ]
        | OpNot -> [ [|0; 0; 0; 3|] ]
    end
    | ESel _   -> [ [|0; 2; 2; 3|]; [|0; 1; 2; 3|] ]
    | EShape _ -> [ [|0; 0; 1; 2|] ]
    | EDim _   -> [ [|0; 0; 0; 1|] ]

    | _ -> infer_err @@ sprintf "invalid PV argument `%s'" (expr_to_str e)

let dem_env_to_str: dem_env -> string = fun env ->
    Dem_env.fold (fun k v tail ->
        sprintf "%s -> [%s]\n%s" k (String.concat ", " @@ Array.to_list (Array.map string_of_int @@ List.hd v)) tail
    ) env ""

let infer_prog: expr -> dem_env = fun e ->
    sd e [|0; 1; 2; 3|] Dem_env.empty
