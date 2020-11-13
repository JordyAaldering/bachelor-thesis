open Ast
open Pv
open Demenv
open Printf

exception RewriteFailure of string

let rewrite_err msg = raise @@ RewriteFailure msg


let pv_get: (int array) list -> int -> int array -> int array = fun pv i dem_ivs ->
    let dem = List.nth pv i in
    Array.map (fun i ->
        Array.get dem i
    ) dem_ivs

let rec sd: expr -> int array -> dem_env -> dem_env = fun e dem f -> match e with
    | EVar x -> dem_env_set f x dem; f
    | EConst _x -> f
    | EArray _xs -> f

    | EApply (e1, e2) -> (match e1 with
        | EVar x -> (* e2 is applied to a var, e.g. the name of a lambda function *)
            let x_dem = dem_env_lookup f x in
            let dem' = pv_get [x_dem] 0 dem in
            sd e2 dem' f
        | ELambda (x, el) -> (* e1 is a local lambda, which e2 is applied to *)
            let lambda_pv = pv e1 f in
            dem_env_set f x (List.hd lambda_pv);
            let dem' = pv_get lambda_pv 0 dem in
            let env_lambda = sd el dem' f in
            let f' = sd e2 dem' env_lambda in
            dem_env_remove f' x;
            f'
        | _ -> rewrite_err @@ sprintf "Invalid SD Apply arguments `%s' and `%s'"
                (expr_to_str e1) (expr_to_str e2)
    )
    | ELetIn (x, e1, e2) -> (match e1 with
        | ELambda (var, lambda) -> (
            let lambda_pv = pv e1 f in
            dem_env_set f x (List.hd lambda_pv);
            let env_e1 = sd lambda (pv_get lambda_pv 0 dem) f in
            let env_e2 = sd e2 dem f in
            dem_env_remove env_e1 var;
            let f' = dem_env_combine env_e1 env_e2 in
            dem_env_remove f' var;
            f'
        )
        | _ -> (
            let dem_pv = pv (ELambda (x, e2)) f in
            let env_e1 = sd e1 (pv_get dem_pv 0 dem) f in
            let env_e2 = sd e2 dem f in
            dem_env_remove env_e2 x;
            dem_env_combine env_e1 env_e2
        )
    )
    | EIfThen (ec, et, ef) ->
        let env_ec = sd ec [|0;3;3;3|] f in
        let env_et = sd et dem f in
        let env_ef = sd ef dem f in
        dem_env_combine (dem_env_combine env_ec env_et) env_ef

    | EBinary (op, el, er) ->
        let dem_pv = pv e f in
        let env_el = sd el (pv_get dem_pv 0 dem) f in
        let env_er = sd er (pv_get dem_pv 1 dem) f in
        dem_env_combine env_el env_er
    | EUnary (op, er) ->
        let dem_pv = pv e f in
        sd er (pv_get dem_pv 0 dem) f

    | ESel (iv, v) ->
        let dem_pv = pv e f in
        let env_iv = sd iv (pv_get dem_pv 0 dem) f in
        let env_v = sd v (pv_get dem_pv 1 dem) f in
        dem_env_combine env_iv env_v
    | EShape x
    | EDim x ->
        let dem_pv = pv e f in
        sd x (pv_get dem_pv 0 dem) f

    | _ -> rewrite_err @@ sprintf "Invalid SD argument `%s'" (expr_to_str e)

and pv: expr -> dem_env -> (int array) list = fun e f -> match e with
    | ELambda (x, e) ->
        let env = sd e [|0;1;2;3|] f in
        let dem = dem_env_lookup env x in
        [ dem ]
    | EBinary (op, _l, _r) -> (match op with
        | OpPlus
        | OpMin
        | OpMult
        | OpDiv -> [ [|0;1;2;3|]; [|0;1;2;3|] ]
        | OpEq
        | OpNe
        | OpLt
        | OpLe
        | OpGt
        | OpGe -> [ [|0;0;0;3|]; [|0;0;0;3|] ]
    )
    | EUnary (op, _r) -> (match op with
        | OpNeg -> [ [|0;1;2;3|] ]
        | OpNot -> [ [|0;0;0;3|] ]
    )
    | ESel _   -> [ [|0;2;2;3|]; [|0;1;2;3|] ]
    | EShape _ -> [ [|0;0;1;2|] ]
    | EDim _   -> [ [|0;0;0;1|] ]

    | _ -> rewrite_err @@ sprintf "Invalid PV argument `%s'" (expr_to_str e)

let sd_prog: expr -> dem_env = fun e ->
    sd e [|0;1;2;3|] (mk_dem_env ())
