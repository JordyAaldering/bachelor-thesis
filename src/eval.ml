open Loc
open Ast
open Env
open Value
open Storage
open Printf

exception EvalFailure of string

let eval_err msg =
    raise @@ EvalFailure msg

let eval_err_loc msg loc =
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
    let st = st_add st p v in
    (st, p)


(** Evaluation **)

let ptr_binary st op p1 p2 =
    let v1 = st_lookup st p1 in
    let v2 = st_lookup st p2 in
    match op with
        | OpPlus -> Vect ([], [0.])
        | OpMinus -> Vect ([], [0.])
        | OpMult -> Vect ([], [0.])
        | OpDiv -> Vect ([], [0.])
        | OpMod -> Vect ([], [0.])
        | OpEq -> Vect ([], [0.])
        | OpNe -> Vect ([], [0.])
        | OpLt -> Vect ([], [0.])
        | OpLe -> Vect ([], [0.])
        | OpGt -> Vect ([], [0.])
        | OpGe -> Vect ([], [0.])

let ptr_unary st env op p =
    let v = st_lookup st p in
    match op with
        | OpNeg -> value_neg v
        | OpNot -> value_not v

let rec eval st env e = match e with
    | { kind=EVar x } ->
        (st, (env_lookup env x))

    | { kind=EConst x } ->
        add_fresh_value st (mk_value_const x)

    | { kind=EArray lst } ->
        let st, ptr_lst = eval_expr_lst st env lst in
        let shp = [List.length ptr_lst] in
        let data = List.fold_right (fun ptr val_lst ->
                let ptr_val = st_lookup st ptr in
                match ptr_val with
                    | Vect (_, d) -> List.append d val_lst
            ) ptr_lst []
        in
        add_fresh_value st (mk_value_vect shp data)

    | { kind = EApply (e1, e2) } ->
        let (st, p1) = eval st env e1 in
        let (st, p2) = eval st env e2 in
        eval st env e1

    | { kind=EIfThen (e1, e2, e3) } ->
        (st, "")

    | { kind=ELetIn (var, e1, e2) } ->
        let pname = create_fresh_ptr () in
        let (st, p1) = eval st (env_add env var pname) e1 in
        eval st (env_add env var p1) e2

    | { kind=EBinary (op, e1, e2) } ->
        let (st, p1) = eval st env e1 in
        let (st, p2) = eval st env e2 in
        let v = ptr_binary st op p1 p2 in
        add_fresh_value st v

    | { kind=EUnary (op, e1) } ->
        let (st, p) = eval st env e1 in
        let v = ptr_unary st env op p in
        add_fresh_value st v

    | { kind=ESel (e1, e2) } ->
        let (st, iv) = eval st env e1 in
        let (st, v1) = eval st env e2 in
        let v2 = value_sel (st_lookup st iv) (st_lookup st v1) in
        add_fresh_value st v2

    | { kind=EShape e1 } ->
        let (st, p) = eval st env e1 in
        let v = value_shape @@ st_lookup st p in
        add_fresh_value st v

    | { kind=EDim e1 } ->
        let (st, p) = eval st env e1 in
        let v = value_dim @@ st_lookup st p in
        add_fresh_value st v

and eval_expr_lst st env lst = match lst with
    | [] -> (st, [])
    | e::tl ->
        let st, p = eval st env e in
        let st, res = eval_expr_lst st env tl in
        (st, p::res)
