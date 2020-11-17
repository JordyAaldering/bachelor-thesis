open Ast
open Env
open Printf

type demand = int array
type pv_env = demand list Env.t

exception InferenceFailure of string
let infer_err msg = raise @@ InferenceFailure msg

let pv_to_str: demand list -> string = fun pv ->
    sprintf "[%s]" @@ String.concat ", " @@ List.map (fun dem ->
        sprintf "[%d, %d, %d, %d]" dem.(0) dem.(1) dem.(2) dem.(3)
    ) pv

let pv_env_to_str: pv_env -> string = fun env ->
    Env.fold (fun k v tail ->
        sprintf "%s -> %s\n%s" k (pv_to_str v) tail
    ) env ""

let rec sd: expr -> demand -> pv_env -> pv_env = fun e dem env -> match e with
    | EVar x -> Env.add x [dem] env
    | ENum _x -> env
    | EArray _xs -> env

    | ELambda (x, e1) -> begin try
            let env' = sd e1 dem env in
            let demx = Env.find x env' in
            Env.add x demx env'
        with Not_found ->
            infer_err @@ sprintf "could not find lambda variable `%s' in environment" x
    end
    | EApply (EVar fun_id, e2) -> begin try
            let dem' = Env.find fun_id env in
            let dem' = Array.map (Array.get @@ List.hd dem') dem in
            sd e2 dem' env
        with Not_found ->
            infer_err @@ sprintf "could not find function `%s' in environment" fun_id
    end
    | EApply (e1, e2) -> (* e1 is a lambda- or primitive expression *)
        let dem' = pv e1 env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        sd e2 dem' env
    | ELetIn (fun_id, ELambda(x, e1), e2) ->
        let dem' = pv (ELambda (x, e1)) env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        let env' = Env.add fun_id [dem'] env in
        let env2 = sd e2 dem env' in
        let env2' = Env.remove x env2 in
        Env.union (fun key x y ->
            Some (List.map2 max x y)
        ) env' env2'
    | ELetIn (x, e1, e2) ->
        let dem' = pv (ELambda (x, e2)) env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = Env.remove x @@ sd e2 dem env in
        Env.union (fun key x y ->
            Some (List.map2 max x y)
        ) env1 env2
    | EIfThen (ec, et, ef) ->
        let envc = sd ec [|0; 3; 3; 3|] env in
        let envt = sd et dem env in
        let envf = sd ef dem env in
        Env.union (fun key x y ->
            Some (List.map2 max x y)
        ) envc @@
            Env.union (fun key x y ->
                Some (List.map2 max x y)
            ) envt envf

    | EBinary (op, e1, e2) ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        let env1 = sd e1 dem' env in
        let env2 = sd e2 dem' env in
        Env.union (fun key x y ->
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
        Env.union (fun key x y ->
            Some (List.map2 max x y)
        ) env1 env2
    | EShape e1
    | EDim e1 ->
        let dem' = pv e env in
        let dem' = Array.map (Array.get @@ List.hd dem') dem in
        sd e1 dem' env

and pv: expr -> pv_env -> demand list = fun e env -> match e with
    | ELambda (x, e1) ->
        let env' = sd e1 [|0; 1; 2; 3|] env in
        begin try
            Env.find x env'
        with Not_found -> (* variable x does not occur in e1, thus there is no demand *)
            [[|0; 3; 3; 3|]]
        end
    
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

let infer_prog: expr -> pv_env = fun e ->
    let env = sd e [|0; 1; 2; 3|] Env.empty in
    printf "Demand environment:\n%s\n" (pv_env_to_str env);
    env