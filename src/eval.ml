open Ast
open Eval_env
open Value
open Printf

module Storage = Map.Make(String)
type storage = value Storage.t

exception EvalFailure of string
let eval_err msg = raise @@ EvalFailure msg

let ptr_count = ref 0
let create_fresh_ptr () =
    ptr_count := !ptr_count + 1;
    sprintf "p%d" !ptr_count

(* Creates a fresh variable and adds it to the storage with value `v' *)
let add_fresh_value st v =
    let p = create_fresh_ptr () in
    let st = Storage.add p v st in
    (st, p)

let ptr_binary st op p1 p2 =
    let v1 = Storage.find p1 st in
    let v2 = Storage.find p2 st in
    match op with
        | OpPlus -> value_add v1 v2
        | OpMin  -> value_add v1 (value_neg v2)
        | OpMult -> value_mul v1 v2
        | OpDiv  -> value_div v1 v2
        | OpEq -> value_eq v1 v2
        | OpNe -> value_not @@ value_eq v1 v2
        | OpLt -> value_lt v1 v2
        | OpLe -> value_not @@ value_gt v1 v2
        | OpGt -> value_gt v1 v2
        | OpGe -> value_not @@ value_lt v1 v2

let ptr_unary st op p =
    let v = Storage.find p st in
    match op with
        | OpNeg -> value_neg v
        | OpNot -> value_not v

let rec eval_expr e st env = match e with
    | EVar x -> (st, env_lookup env x)
    | EConst x -> add_fresh_value st (Const x)
    | EArray es ->
        let st, ptr_lst = eval_expr_lst es st env in
        let shp = [Const (float_of_int @@ List.length ptr_lst)] in
        let data = List.fold_right (fun ptr val_lst ->
                let ptr_val = Storage.find ptr st in
                ptr_val :: val_lst
            ) ptr_lst []
        in
        add_fresh_value st (Vect (shp, data))

    | EApply (e1, e2) ->
        let st, p1 = eval_expr e1 st env in
        let st, p2 = eval_expr e2 st env in
        let x, body, env' = closure_to_triple (Storage.find p1 st) in
        eval_expr body st (env_add env' x p2)
    | ELambda (_x, _e) ->
        add_fresh_value st (Closure (e, env))
    | EIfThen (ec, et, ef) ->
        let st, p1 = eval_expr ec st env in
        let v = Storage.find p1 st in
        eval_expr (if value_is_truthy v then et else ef) st env
    | ELetIn (var, e1, e2) ->
        let pname = sprintf "tmp%d" (!ptr_count + 1) in
        let st, p1 = eval_expr e1 st (env_add env var pname) in
        eval_expr e2 st (env_add env var p1)

    | EBinary (op, e1, e2) ->
        let st, p1 = eval_expr e1 st env in
        let st, p2 = eval_expr e2 st env in
        let v = ptr_binary st op p1 p2 in
        add_fresh_value st v
    | EUnary (op, e1) ->
        let st, p = eval_expr e1 st env in
        let v = ptr_unary st op p in
        add_fresh_value st v
    | ESel (e1, e2) ->
        let st, iv = eval_expr e1 st env in
        let st, v1 = eval_expr e2 st env in
        let v2 = value_sel (Storage.find iv st) (Storage.find v1 st) in
        add_fresh_value st v2
    | EShape e1 ->
        let st, p = eval_expr e1 st env in
        let v = value_shape @@ Storage.find p st in
        add_fresh_value st v
    | EDim e1 ->
        let st, p = eval_expr e1 st env in
        let v = value_dim @@ Storage.find p st in
        add_fresh_value st v

and eval_expr_lst es st env = match es with
    | [] -> (st, [])
    | x :: xs ->
        let st, y = eval_expr x st env in
        let st, ys = eval_expr_lst xs st env in
        (st, y :: ys)

let st_to_str: storage -> string = fun st ->
    Storage.fold (fun k v tail ->
        sprintf "%s -> %s\n%s" k (value_to_str v) tail
    ) st ""

let eval_prog: expr -> unit = fun e ->
    let st, p = eval_expr e Storage.empty [] in
    printf "Storage:\n%s\n" (st_to_str st);
    printf "Result:\n%s = %s\n" p (value_to_str (Storage.find p st))
