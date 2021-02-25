open Ast
open Env
open Value
open Printf

exception EvalError of string

(** an environment mapping variables to their corresponding values *)
type val_env = value Env.t

let eval_err (msg: string) =
    raise @@ EvalError msg

let add_fresh_value (st: val_env) (v: value) : (val_env * string) =
    let p = create_fresh_ptr () in
    let st = Env.add p v st in
    (st, p)

let update_let_ptr (st: val_env) (p_old: string) (p_new: string) : val_env =
    let value_updater v =
        match v with
            | VClosure (s, e, env) ->
                let env' = Env.map (fun p ->
                    if p = p_old then p_new else p
                ) env in
                VClosure (s, e, env')
            | _ -> v
    in Env.map value_updater st

let ptr_list_shape (st: val_env) (ptr_lst: string list) : int list =
    if ptr_lst = [] then
        []
    else
        let vec = Env.find (List.hd ptr_lst) st in
        let shp = value_shape vec in
        let _, shp = extract_value shp in
        List.map int_of_float shp

let ptr_binary (st: val_env) (op: bop) (p1: string) (p2: string) : value =
    let v1 = Env.find p1 st in
    let v2 = Env.find p2 st in
    match op with
    | OpAppend -> value_concat v1 v2
    | OpAdd -> value_add v1 v2
    | OpMin -> value_sub v1 v2
    | OpMul -> value_mul v1 v2
    | OpDiv -> value_div v1 v2
    | OpEq -> value_eq v1 v2
    | OpNe -> value_ne v1 v2
    | OpLt -> value_lt v1 v2
    | OpLe -> value_le v1 v2
    | OpGt -> value_gt v1 v2
    | OpGe -> value_ge v1 v2

let ptr_unary (st: val_env) (op: uop) (p: string) : value =
    let v = Env.find p st in
    match op with
    | OpNeg -> value_neg v
    | OpAbs -> value_abs v
    | OpNot -> value_not v

let rec eval_expr (e: expr) (st: val_env) (env: ptr_env) : (val_env * string) =
    match e with
    (* variables *)
    | EVar x -> (st, Env.find x env)
    | EFloat x -> add_fresh_value st (VArray ([], [x]))
    | EArray es ->
        let st, ptrs = eval_expr_lst es st env in
        let shp = List.length ptrs :: ptr_list_shape st ptrs in
        let data = List.fold_right (fun ptr val_lst ->
                let ptr_val = Env.find ptr st in
                let floats = (match ptr_val with
                    | VArray (_, xs) -> xs
                    | _ -> eval_err @@ sprintf "invalid value in list `%s'"
                            (value_to_str ptr_val)
                ) in
                floats @ val_lst
            ) ptrs []
        in
        add_fresh_value st (VArray (shp, data))
    (* expressions *)
    | EApply (e1, e2) ->
        let st, p1 = eval_expr e1 st env in
        let st, p2 = eval_expr e2 st env in
        let x, body, env' = extract_closure (Env.find p1 st) in
        eval_expr body st (Env.add x p2 env')
    | ELambda (s, e1) ->
        add_fresh_value st (VClosure (s, e1, env))
    | ELet (s, e1, e2) ->
        let pname = create_fresh_ptr () in
        let st, p1 = eval_expr e1 st (Env.add s pname env) in
        let st = update_let_ptr st pname p1 in
        eval_expr e2 st (Env.add s p1 env)
    | ECond (e1, e2, e3) ->
        let st, p1 = eval_expr e1 st env in
        let v = Env.find p1 st in
        eval_expr (if value_is_truthy v then e2 else e3) st env
    | EWith (e_gen, e_def, e_min, s, e_max, e) ->
        let st, p_gen = eval_expr e_gen st env in
        let st, p_def = eval_expr e_def st env in
        let st, p_min = eval_expr e_min st env in
        let st, p_max = eval_expr e_max st env in
        let v_gen = Env.find p_gen st in
        let v_def = Env.find p_def st in
        let v_min = Env.find p_min st in
        let v_max = Env.find p_max st in
        assert_shape_eq v_min v_max;

        let _, v_shp = extract_value v_gen in
        let def_shp, def = extract_value v_def in
        let gen_sub = List.filteri (fun i _ -> i >= List.length def_shp) v_shp in
        let v_gen_sub = VArray ([List.length gen_sub], gen_sub) in
        let v_shp = List.map int_of_float v_shp in

        let rec eval_with iv_cur st env =
            if (value_is_truthy @@ value_lt iv_cur v_max) then (
                let v_res = if value_is_truthy @@ value_ge iv_cur v_min then (
                    let st, p = add_fresh_value st iv_cur in
                    let st, p_cur = eval_expr e st (Env.add s p env) in
                    let v_res = Env.find p_cur st in
                    assert_shape_eq v_def v_res;
                    v_res
                ) else v_def
                in
                let _, data = extract_value v_res in
                let shp_shp, shp_cur = extract_value iv_cur in
                let st_ref = ref st in
                let data_ref = ref data in
                (* increase every shape index by one, recursively *)
                (* from right to left *)
                for i = 0 to List.length shp_cur - 1 do
                    let iv_next = VArray (shp_shp, List.mapi (fun j y -> if i = List.length shp_cur - j - 1 then y +. 1. else y) shp_cur) in
                    let st, data_next = eval_with iv_next !st_ref env in
                    st_ref := st;
                    data_ref := !data_ref @ data_next;
                done;
                (!st_ref, !data_ref)
            ) else if (value_is_truthy @@ value_lt iv_cur v_gen_sub) then
                (st, def)
            else
                (st, [])
        in
        let v_zeros = value_mul v_min (VArray ([], [0.])) in
        let st, data = eval_with v_zeros st env in
        add_fresh_value st (VArray(v_shp, data))
    (* operands *)
    | EBinary (op, e1, e2) ->
        let st, p1 = eval_expr e1 st env in
        let st, p2 = eval_expr e2 st env in
        let v = ptr_binary st op p1 p2 in
        add_fresh_value st v
    | EUnary (op, e1) ->
        let st, p = eval_expr e1 st env in
        let v = ptr_unary st op p in
        add_fresh_value st v
    (* primitive functions *)
    | ESel (e1, e2) ->
        let st, v1 = eval_expr e1 st env in
        let st, iv = eval_expr e2 st env in
        let v2 = value_sel (Env.find v1 st) (Env.find iv st) in
        add_fresh_value st v2
    | EShape e1 ->
        let st, p = eval_expr e1 st env in
        let v = value_shape (Env.find p st) in
        add_fresh_value st v
    | EDim e1 ->
        let st, p = eval_expr e1 st env in
        let v = value_dim (Env.find p st) in
        add_fresh_value st v
    | ERead ->
        printf "> ";
        let x = float_of_string @@ read_line () in
        add_fresh_value st (VArray ([], [x]))

and eval_expr_lst (es: expr list) (st: val_env) (env: ptr_env) : (val_env * string list) =
    match es with
    | [] -> (st, [])
    | x :: xs ->
        let st, p = eval_expr x st env in
        let st, ps = eval_expr_lst xs st env in
        (st, p :: ps)

let eval (e: expr) =
    ptr_count := 0;
    let st, p = Debug.time "Evaluation" (fun () ->
        eval_expr e Env.empty Env.empty
    ) in
    printf "%s\n" (value_to_str (Env.find p st))
