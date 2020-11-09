open Loc
open Ast
open Env
open Value
open Storage
open Printf

exception EvalFailure of string

let eval_err msg =
    raise @@ EvalFailure msg

let eval_err_loc loc msg =
    raise @@ EvalFailure (sprintf "%s: error: %s" (loc_to_str loc) msg)


type lexi_iterator =
    | Next of value list
    | Done

let get_iterator_idx it = match it with
    | Next lst -> lst
    | _ -> failwith "get_iterator_idx"


(* A global variable to generate unique names of pointers *)
let ptr_count = ref 0

let fresh_ptr_name () =
    ptr_count := !ptr_count + 1;
    sprintf "p%d" !ptr_count

let add_fresh_val_as_result st v =
    let p = fresh_ptr_name () in
    let st = st_add st p v in
    (st, p)


(** Evaluation **)

let rec eval st env e = match e with
    | { kind=EVar x } ->
        (st, e)

    | { kind=EConst x } ->
        (st, e)

    | { kind=EArray lst } ->
        (st, e)

    | { kind = EApply (e1, e2) } ->
        (st, e)

    | { kind=EIfThen (e1, e2, e3) } ->
        (st, e)

    | { kind=ELetIn (var, e1, e2) } ->
        (st, e)

    | { kind=EBinary (op, e1, e2) } ->
        (st, e)

    | { kind=EUnary (op, e1) } ->
        (st, e)

    | { kind=ESel (iv, v) } ->
        (st, e)

    | { kind=EShape v } ->
        (st, e)

    | { kind=EDim v } ->
        (st, e)
