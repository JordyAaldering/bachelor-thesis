open Ast
open Pv
open Demenv
open Printf

let pv_get: (int array) list -> int -> int array -> int array = fun pv i dem_ivs ->
    let dem = List.nth pv i in
    Array.map (fun i ->
        Array.get dem i
    ) dem_ivs

let rec sd: expr -> int array -> dem_env = fun e dem -> match e with
    | EVar x -> dem_env_new x dem
    | EConst _x -> dem_env_empty ()
    | EArray _xs -> dem_env_empty ()

    | EApply (e1, e2) -> dem_env_empty ()
    | ELambda (x, e) -> dem_env_empty ()
    | ELetIn (x, e1, e2) ->
        let pv = pv @@ ELambda (x, e2) in
        let env_e1 = sd e1 (pv_get pv 0 dem) in
        let env_e2 = sd e2 dem in
        let env_e2 = dem_env_remove env_e2 x in
        dem_env_combine env_e1 env_e2
    | EIfThen (e1, e2, e3) -> dem_env_empty ()

    | EBinary (op, e1, e2) -> dem_env_empty ()
    | EUnary (op, e) -> dem_env_empty ()
    | ESel (e1, e2) -> dem_env_empty ()
    | EShape e -> dem_env_empty ()
    | EDim e -> dem_env_empty ()

and pv: expr -> (int array) list = fun e -> match e with
    | ELambda (x, e) ->
        let env = sd e [|0;1;2;3|] in
        let dem = dem_env_lookup env x in
        [ dem ]
    | EBinary (op, _e1, _e2) -> (match op with
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
    | EUnary (op, _e) -> (match op with
        | OpNeg -> [ [|0;1;2;3|] ]
        | OpNot -> [ [|0;0;0;3|] ]
    )
    | ESel _ -> [ [|0;2;2;3|]; [|0;1;2;3|] ]
    | EShape _ -> [ [|0;0;1;2|] ]
    | EDim _ -> [ [|0;0;0;1|] ]

    | _ -> [ [|0;1;2;3|] ]
