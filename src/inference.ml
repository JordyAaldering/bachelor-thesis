open Ast
open Printf

module Dem_env = Map.Make(String)
type demand = int array
type dem_env = demand list Dem_env.t

exception InferenceFailure of string
let infer_err msg = raise @@ InferenceFailure msg

let pv_get: demand list -> int -> demand -> demand = fun pv i iv ->
    let dem = List.nth pv i in
    Array.map (Array.get dem) iv

let rec sd: expr -> demand -> dem_env -> dem_env = fun e dem env -> match e with
    | EVar x -> Dem_env.add x [dem] env
    | EConst _x -> env
    | EArray _xs -> env

    | _ -> infer_err @@ sprintf "invalid SD argument `%s'" (expr_to_str e)

and pv: expr -> dem_env -> demand list = fun e env -> match e with
    | ELambda (x, e1) ->
        let env' = sd e1 [|0; 1; 2; 3|] env in
        Dem_env.find x env'
    
    | EBinary (op, _l, _r) -> begin match op with
        | OpPlus | OpMin | OpMult | OpDiv ->
            [ [|0; 1; 2; 3|]; [|0; 1; 2; 3|] ]
        | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe ->
            [ [|0; 0; 0; 3|]; [|0; 0; 0; 3|] ]
    end
    | EUnary (op, _r) -> begin match op with
        | OpNeg -> [ [|0; 1; 2; 3|] ]
        | OpNot -> [ [|0; 0; 0; 3|] ]
    end
    | ESel _   -> [ [|0; 2; 2; 3|]; [|0; 1; 2; 3|] ]
    | EShape _ -> [ [|0; 0; 1; 2|] ]
    | EDim _   -> [ [|0; 0; 0; 1|] ]

    | _ -> infer_err @@ sprintf "invalid PV argument `%s'" (expr_to_str e)

let sd_prog: expr -> dem_env = fun e ->
    sd e [|0; 1; 2; 3|] Dem_env.empty
