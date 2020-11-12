open Ast
open Env
open Value
open Storage
open Printf

exception EvalFailure of string

let eval_err msg = raise @@ EvalFailure msg

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
        | OpPlus -> value_add v1 v2
        | OpMinus -> value_add v1 (value_neg v2)
        | OpMult -> value_mul v1 v2
        | OpDiv -> value_div v1 v2
        | OpEq -> value_eq v1 v2
        | OpNe -> value_not @@ value_eq v1 v2
        | OpLt -> value_lt v1 v2
        | OpLe -> value_not @@ value_gt v1 v2
        | OpGt -> value_gt v1 v2
        | OpGe -> value_not @@ value_lt v1 v2

let ptr_unary st env op p =
    let v = st_lookup st p in
    match op with
        | OpNeg -> value_neg v
        | OpNot -> value_not v

let rec eval st env e = match e with
    | EVar x ->
        (st, (env_lookup env x))

    | EConst x ->
        add_fresh_value st (Const x)

    | EArray lst ->
        let st, ptr_lst = eval_expr_lst st env lst in
        let shp = [Const (float_of_int @@ List.length ptr_lst)] in
        let data = List.fold_right (fun ptr val_lst ->
                let ptr_val = st_lookup st ptr in
                ptr_val::val_lst
            ) ptr_lst []
        in
        add_fresh_value st (Vect (shp, data))

    | EApply (e1, e2) ->
        let (st, p1) = eval st env e1 in
        let (st, p2) = eval st env e2 in
        let x, body, env' = closure_to_triple (st_lookup st p1) in
        eval st (env_add env' x p2) body

    | ELambda (_x, _e) ->
        add_fresh_value st (Closure (e, env))

    | EIfThen (ec, et, ef) ->
        let (st, p1) = eval st env ec in
        let v = st_lookup st p1 in
        eval st env (if value_is_truthy v then et else ef)

    | ELetIn (var, e1, e2) ->
        let pname = create_fresh_ptr () in
        let (st, p1) = eval st (env_add env var pname) e1 in
        eval st (env_add env var p1) e2

    | EBinary (op, e1, e2) ->
        let (st, p1) = eval st env e1 in
        let (st, p2) = eval st env e2 in
        let v = ptr_binary st op p1 p2 in
        add_fresh_value st v

    | EUnary (op, e1) ->
        let (st, p) = eval st env e1 in
        let v = ptr_unary st env op p in
        add_fresh_value st v

    | ESel (e1, e2) ->
        let (st, iv) = eval st env e1 in
        let (st, v1) = eval st env e2 in
        let v2 = value_sel (st_lookup st iv) (st_lookup st v1) in
        add_fresh_value st v2

    | EShape e1 ->
        let (st, p) = eval st env e1 in
        let v = value_shape @@ st_lookup st p in
        add_fresh_value st v

    | EDim e1 ->
        let (st, p) = eval st env e1 in
        let v = value_dim @@ st_lookup st p in
        add_fresh_value st v

and eval_expr_lst st env lst = match lst with
    | [] -> (st, [])
    | x::xs ->
        let st, y = eval st env x in
        let st, ys = eval_expr_lst st env xs in
        (st, y::ys)
