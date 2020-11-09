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

let create_fresh_ptr () =
    ptr_count := !ptr_count + 1;
    sprintf "p%d" !ptr_count

(* Creates a fresh variable and adds it to storage `st' with value `v' *)
let add_fresh_value st v =
    let p = create_fresh_ptr () in
    let st = st_add st p (mk_value_const v) in
    (st, p)


(** Evaluation **)

let rec eval st env e = match e with
    | { kind=EVar x } ->
        (st, (env_lookup env x))

    | { kind=EConst x } ->
        add_fresh_value st x

    | { kind=EArray lst } ->
        (st, "")

    | { kind = EApply (e1, e2) } ->
        (st, "")

    | { kind=EIfThen (e1, e2, e3) } ->
        (st, "")

    | { kind=ELetIn (var, e1, e2) } ->
        (st, "")

    | { kind=EBinary (op, e1, e2) } ->
        (st, "")

    | { kind=EUnary (op, e1) } ->
        (st, "")

    | { kind=ESel (iv, v) } ->
        (st, "")

    | { kind=EShape v } ->
        (st, "")

    | { kind=EDim v } ->
        (st, "")
