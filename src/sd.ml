open Ast
open Pv
open Demenv
open Printf

module Dem_env = Map.Make(String)
type dem_env = ((int array) list) Dem_env.t


exception SdFailure of string
let sd_err msg = raise @@ SdFailure msg


let pv_get: (int array) list -> int -> int array -> int array = fun pv i iv ->
    let dem = List.nth pv i in
    Array.map (Array.get dem) iv

let rec sd: expr -> int array -> dem_env -> dem_env = fun e dem env -> match e with
    | EVar x -> Dem_env.add x [dem] env
    | EConst _x -> env
    | EArray _xs -> env

    | _ -> sd_err @@ sprintf "Invalid SD argument `%s'" (expr_to_str e)

and pv: expr -> dem_env -> (int array) list = fun e env -> match e with
    | ELambda (x, e) ->
        let env' = sd e [|0;1;2;3|] env in
        Dem_env.find x env'
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

    | _ -> sd_err @@ sprintf "Invalid PV argument `%s'" (expr_to_str e)

let sd_prog: expr -> dem_env = fun e ->
    sd e [|0;1;2;3|] Dem_env.empty
