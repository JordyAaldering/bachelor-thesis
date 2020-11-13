open Ast
open Demenv
open Printf

let rec sd: expr_kind -> int array -> dem_env = fun e dem -> match e with
    | EVar x -> dem_env_new x dem
    | EConst _x -> dem_env_empty ()
    | EArray _xs -> dem_env_empty ()

    | EApply (e1, e2) -> []
    | ELambda (x, e) -> []
    | ELetIn (x, e1, e2) -> []
    | EIfThen (e1, e2, e3) -> []

    | EBinary (op, e1, e2) -> []
    | EUnary (op, e) -> []
    | ESel (e1, e2) -> []
    | EShape e -> []
    | EDim e -> []
