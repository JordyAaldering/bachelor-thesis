open Ast
open Storage
open Env
open Value
open Valueops
open Print
open Printf

exception EvalFailure of string

type lexi_iterator =
    | Nxt of value list
    | Done

let get_iterator_idx it = match it with
    | Nxt (lst) -> lst
    | _ -> failwith "get_iterator_idx"

(* A global variable to generate unique names of pointers *)
let ptr_count = ref 0

(* Generate a fresh pointer name *)
let fresh_ptr_name () =
    ptr_count := !ptr_count + 1;
    sprintf "p%d" !ptr_count

let rec list_split lst n = match n with
    | 0 -> ([], lst)
    | x when x > 0 ->
            begin match lst with
            | [] -> failwith "list_split"
            | h :: t ->
                    let l, r = list_split t (x-1) in
                    (h::l, r)
            end
    | _ -> failwith "list_split"

let eval_err msg =
    raise (EvalFailure msg)

let eval_err_loc loc msg =
    raise (EvalFailure (sprintf "%s: error: %s" (Loc.to_str loc) msg))

let eval_warn msg =
    printf "warning: %s\n" msg


(* Index-vector to offset.  SHP and IDX are value vectors.  *)
let idx_to_offset shp idx =
    let rec rowmajor sprod res shp idx =
        match (shp,idx) with
        | [],[] ->
                res
        | (sh::shptl), (i::idxtl) ->
                rowmajor (sprod*sh) (res + i*sprod) shptl idxtl
        | _ -> failwith "different length"
    in
    let shp_numvec = List.rev @@ List.map value_num_to_int shp in
    let idx_numvec = List.rev @@ List.map value_num_to_int idx in
    rowmajor 1 0 shp_numvec idx_numvec


(*let rec array_element_type_finite st ptrlst =
    match ptrlst with
    | [] -> true
    | p :: tl -> let v = st_lookup st p in
                 match v with
                 | VImap (_, _, _, _) -> false
                 | VFilter (_, _, _) -> false
                 | _ -> array_element_type_finite st tl*)


let ptr_binop st op p1 p2 loc1 loc2=
    let v1 = st_lookup st p1 in
    let v2 = st_lookup st p2 in
    if not @@ value_is_num v1 then
        eval_err_loc loc1 @@ sprintf "attempt to perform binary operation `%s' on non-number lhs `%s'"
                            (bop_to_str op) (value_to_str v1);
    if not @@ value_is_num v2 then
        eval_err_loc loc2 @@ sprintf "attempt to perform binary operation `%s' on non-number rhs `%s'"
                            (bop_to_str op) (value_to_str v2);
    let o1 = value_num_to_int v1 in
    let o2 = value_num_to_int v2 in
    match op with
    | OpPlus -> VNum (o1 + o2)
    | OpMult -> VNum (o1 * o2)
    | OpMinus -> VNum (o1 - o2)
    | OpDiv -> VNum (o1 / o2)
    | OpMod -> VNum (o1 mod o2)
    | OpEq -> if o1 == o2 then VTrue else VFalse
    | OpNe -> if o1 <> o2 then VTrue else VFalse
    | OpLt -> if o1 < o2 then VTrue else VFalse
    | OpLe -> if o1 <= o2 then VTrue else VFalse
    | OpGt -> if o1 > o2 then VTrue else VFalse
    | OpGe -> if o1 >= o2 then VTrue else VFalse


(* update all the enclosed environments of the storage replacing bindings of
   form x |-> pold with x |-> pnew.  *)
let update_letrec_ptr st pold pnew =
    let rec env_updater env =
        match env with
        | [] -> []
        | (x, p) :: tl -> (if p = pold then (x, pnew) else (x, p))
                          :: env_updater tl
    in let value_updater k v = Some (v)
    in Hashtbl.filter_map_inplace value_updater st;
    st

(* Extract lb-ub pairs from the list of partitions as it is stored in
   in the VImap.  The `lb' and `ub' are lower bound and upper bound of
   a partition.  The type of `lb' and `ub' is a list of values.  *)
let vparts_to_pairs parts =
    List.map (fun p ->
          let ((alb, _, aub), _) = p in
          let _, lb = value_array_to_pair alb in
          let _, ub = value_array_to_pair aub in
          (lb, ub))
         parts


(* Check if partition (plb, pub) is within (lb, ub) bounds.  *)
let part_within_bounds lb ub plb pub =
    value_num_vec_le lb plb && value_num_vec_lt plb ub
    && value_num_vec_le lb pub && value_num_vec_le pub ub


(* Check if two partitons (p1_lb, p1_ub) and (p2_lb, p2_ub) intersect.
   The type of p1_lb, p1_ub, p2_lb, p2_ub is list of values.
   The function returns option type with the intersection area in case
   partitions do intersect.  *)
let part_intersect p1_lb p1_ub p2_lb p2_ub =
    let max a b = if value_num_compare a b = 1 then a else b
    in
    let min a b = if value_num_compare a b = -1 then a else b
    in
    let vec_elem_max v1 v2 = List.map2 (fun x y -> max x y) v1 v2
    in
    let vec_elem_min v1 v2 = List.map2 (fun x y -> min x y) v1 v2
    in

    let maxlb = vec_elem_max p1_lb p2_lb in
    let minub = vec_elem_min p1_ub p2_ub in
    (*printf "--[part_intersect] maxlb = [%s], minub = [%s]\n"
           (val_lst_to_str maxlb) (val_lst_to_str minub) ;
    printf "--[part_intersect] ([%s], [%s]) and ([%s], [%s]) "
           (val_lst_to_str p1_lb) (val_lst_to_str p1_ub)
           (val_lst_to_str p2_lb) (val_lst_to_str p2_ub);*)
    if value_num_vec_lt maxlb minub
       (* If the intersected area lies within both partitions.  *)
       && part_within_bounds p1_lb p1_ub maxlb minub
       && part_within_bounds p2_lb p2_ub maxlb minub
    then
    begin
        (*printf "do intersect\n";*)
        Some ((maxlb, minub))
    end
    else
    begin
        (*printf "do not intersect\n";*)
        None
    end


(* cut (part_lb, part_ub) from the (lb_vec, ub_vec) *)
let part_split lb_vec ub_vec part_lb part_ub =
    (* take elements at position < pos from l1 and the rest from l2.  *)
    let takedrop x y pos =
        let xl, xr = list_split x pos in
        let yl, yr = list_split y pos in
        List.append xl yr
    in
    let upd_vec vec pos v =
        let l, r = list_split vec pos in
        List.append l (v :: (List.tl r))
    in
    let chk_empty s e =
        let chk = value_num_vec_lt s e in
        (* printf "---[chk_empty] [%s] < [%s] = [%s]\n"
               (val_lst_to_str s) (val_lst_to_str e) (if chk then "true" else "false");*)
        if chk then
            [(s, e)]
        else
            []
    in
    let splitpos lb_vec ub_vec part_lb part_ub pos =
        let s1 = takedrop part_lb lb_vec pos in
        let e1 = takedrop part_ub (takedrop part_lb ub_vec (pos+1)) pos in
        let s2 = upd_vec (takedrop part_lb lb_vec (pos+1)) pos (List.nth part_ub pos) in
        let e2 = takedrop part_ub ub_vec pos in
        (*let pb = (s1, e1) :: (s2, e2) :: [] in
        debug @@
        sprintf "--[update_imap_part] splitpos returns %s\n"
                @@ String.concat ", " @@ List.map (fun x -> let l, b = x in
                                                   sprintf "([%s], [%s])"
                                                   (val_lst_to_str l) (val_lst_to_str b))
                                         pb;
        pb*)

        List.append (chk_empty s1 e1) (chk_empty s2 e2)
    in
    List.flatten @@
    List.mapi (fun i v -> splitpos lb_vec ub_vec part_lb part_ub i) lb_vec


(* Check that partitions of an imap partition the entire shape of the imap.  *)
let check_parts_form_partition glb gub parts =
    let rec _intersect parts_to_cover parts =
        match parts with
        | [] ->
                parts_to_cover
        | (lb, ub) :: tl ->
                if part_within_bounds glb gub lb ub then
                    let parts_to_cover =
                    List.flatten @@
                    List.map (fun p ->
                              let plb, pub = p in
                              match part_intersect plb pub lb ub with
                              | None ->
                                      [p]
                              | Some ((intlb, intub)) ->
                                      part_split plb pub intlb intub)
                             parts_to_cover
                    in
                    (*printf "--[_intersect] prt = ([%s], [%s]), parts_to_cover = %s\n"
                           (val_lst_to_str lb) (val_lst_to_str ub)
                           (String.concat ", " @@ List.map (fun x -> let xlb, xub = x in
                                                            sprintf "([%s], [%s])"
                                                                    (val_lst_to_str xlb)
                                                                    (val_lst_to_str xub))
                                                           parts_to_cover);*)
                    _intersect parts_to_cover tl
                else
                    (* FIXME this error message needs location *)
                    eval_err @@ sprintf "partition ([%s], [%s]) is not within ([%s], [%s])"
                                (val_lst_to_str lb) (val_lst_to_str ub)
                                (val_lst_to_str glb) (val_lst_to_str gub)
    in
    let remaining_parts =
        (* For imaps that generate arrays of zero shape, we don't need
           to add (glb, gub) as it will be empty partition.  *)
        let parts_to_cover =
            if value_num_vec_lt glb gub then [(glb, gub)] else []
        in
            _intersect parts_to_cover @@ vparts_to_pairs parts
    in remaining_parts = []

(* Check whether partitions in parts intersect amongst each other.
   The type of parts is (vgen * expr_or_ptr) list.  *)
let check_parts_intersect parts =
    let rec _check parts =
        match parts with
        | [] -> false
        | (lb, ub) :: tl ->
                let local_intersect =
                List.fold_left (fun res p ->
                                if res then
                                    res
                                else
                                    let plb, pub = p in
                                    match part_intersect lb ub plb pub with
                                    | None -> false
                                    | Some (_) -> true)
                               false
                               tl

                in
                if local_intersect then
                    local_intersect
                else
                    _check tl
    in
    _check @@ vparts_to_pairs parts

let add_fresh_val_as_result st v =
    let p = fresh_ptr_name () in
    let st = st_add st p v in
    (st, p)


let lexi_next v lb ub =
    let rec _upd v lb ub carry = match v, lb, ub with
        | [], [], [] ->
            []
        | (v::vt), (l::lt), (u::ut) ->
            let v' = value_num_add v carry in
            if v' = u then
                l::(_upd vt lt ut (mk_int_value 1))
            else
                v'::vt
        | _ -> failwith "lexi_next"
    in
    match v with
        | Nxt (lst) ->
            if lst = [] then
                Done
            (* For the "last" index (ub-1) we just increase the last element by one.  *)
            else if List.map (fun x -> value_num_add x (mk_int_value 1)) lst = ub then
                Done
            else
                Nxt(List.rev @@
                    _upd (List.rev lst) (List.rev lb) (List.rev ub) (mk_int_value 1))
        | Done ->
            failwith "calling lexi_next with Done iterator"

let rec shape st env p =
    match st_lookup st p with
    | VFalse ->
            (st, mk_empty_vector ())
    | VTrue ->
            (st, mk_empty_vector ())
    | VNum _ ->
            (st, mk_empty_vector ())
    | VArray (shp, _) ->
            (st, mk_array_value [mk_int_value (List.length shp)] shp)

(* Number of dimensions in the array.
   We assume that the value is always finite (< w), but we use ordinal type to return
   the result. *)
and dimension st env pa =
    let st, shp = shape st env pa in
    let _, data = value_array_to_pair shp in
    let d = List.length data in
    (st, d)

(* Number of elements.  *)
and element_count st env pa =
    let st, shp = shape st env pa in
    let _, data = value_array_to_pair shp in
    let vres = List.fold_left value_num_mult (VNum 1) @@ List.rev data in
    (st, value_num_to_int vres)

and array_element_shape_valid st env ptrlst =
    if (List.length ptrlst) <= 1 then
        (st, true)
    else
        let rec cmp st shp_first lst =
            match lst with
            | [] -> (st, true)
            | p :: tl ->
                    let st, v = shape st env p in
                    if v <> shp_first then
                        (st, false)
                    else
                        cmp st shp_first tl
        in
        let st, shp = shape st env (List.hd ptrlst) in
        cmp st shp (List.tl ptrlst)

(* return the shape vector of the first element of the pointer list.  *)
and ptr_list_fst_shape st env ptrlst =
    if ptrlst = [] then
        (st, [])
    else
        let st, shp = shape st env @@ List.hd ptrlst in
        let _, shp_vec = value_array_to_pair shp in
        (st, shp_vec)

and ptr_unary st env op p1 loc =
    let v1 = st_lookup st p1 in
    match op with
        | OpNeg ->
            (st, VNum (-1))
        | OpNot ->
            (st, VFalse)

and eval st env e =
    match e with
    | { expr_kind = EFalse } ->
            add_fresh_val_as_result st @@ mk_false_value

    | { expr_kind = ETrue } ->
            add_fresh_val_as_result st @@ mk_true_value

    | { expr_kind = ENum (o) } ->
            add_fresh_val_as_result st @@ mk_int_value o

    | { expr_kind = EVar (x) } ->
            (* FIXME add location information *)
            (st, (env_lookup env x))

    | { expr_kind = EArray (lst) } ->
            (* evaluate all the elements in the list sequentially.  *)
            let st, ptrlst = eval_expr_lst st env lst in
            (* check that the shape of the elements is the same.  *)
            let st, shp_valid_p = array_element_shape_valid st env ptrlst in
            if not shp_valid_p then
                eval_err_loc e.loc "array elements are of different shapes";
            let st = List.fold_left (fun st p ->
                                     force_obj_to_array st env p e.loc)
                                    st ptrlst in
            (* get the data vector of the shape of the first element.  *)
            let st, shp_vec = ptr_list_fst_shape st env ptrlst in
            let shp = List.append [mk_int_value (List.length ptrlst)] shp_vec in
            let data = List.fold_right
                            (fun ptr val_lst ->
                             let ptr_val = st_lookup st ptr in
                             match ptr_val with
                             | VArray (_, d) -> List.append d val_lst
                             | _ -> List.append [ptr_val] val_lst)

                            ptrlst [] in
            add_fresh_val_as_result st @@ mk_array_value shp data

    | { expr_kind = EBinary (op, e1, e2) } ->
            let (st, p1) = eval st env e1 in
            let (st, p2) = eval st env e2 in
            add_fresh_val_as_result st (ptr_binop st op p1 p2 e1.loc e2.loc)

    | { expr_kind = EUnary (op, e1) } ->
            let (st, p1) = eval st env e1 in
            let st, v = ptr_unary st env op p1 e1.loc in
            add_fresh_val_as_result st v

    | { expr_kind = EIfThen (e1, e2, e3) } ->
            let (st, p1) = eval st env e1 in
            let v = st_lookup st p1 in
            begin
                match v with
                | VTrue -> eval st env e2
                | VFalse -> eval st env e3
                | _ -> eval_err_loc e2.loc
                                @@ sprintf "condition predicate evaluates to `%s' (true/false expected)"
                                           (value_to_str v)
            end

    | { expr_kind = ELetIn (var, e1, e2) } ->
            let pname = fresh_ptr_name () in
            let (st, p1) = eval st (env_add env var pname) e1 in
            let st = update_letrec_ptr st pname p1 in
            eval st (env_add env var p1) e2

and eval_bin_app st env p_func p_arg1 p_arg2 msg =
    try
        eval st
             (env_add (env_add (env_add env "__x0" p_arg1) "__x1" p_arg2) "__func" p_func)
             (mk_eapply (mk_eapply (mk_evar "__func") (mk_evar "__x0")) (mk_evar "__x1"))
    with
        EvalFailure _ ->
            eval_err msg

and eval_unary_app st env p_func p_arg1 msg =
    try
        eval st
             (env_add (env_add env "__x0" p_arg1) "__func" p_func)
             (mk_eapply (mk_evar "__func") (mk_evar "__x0"))
    with
        EvalFailure _ ->
            eval_err msg


(* Evaluate selection p_obj at p_idx and emit msg in case
   evaluation throws an exception.  *)
and eval_obj_sel st env p_obj p_idx msg =
    try
        eval st
             (env_add (env_add env "__idx" p_idx) "__obj" p_obj)
             (mk_esel (mk_evar "__obj") (mk_evar "__idx"))
    with
        EvalFailure m ->
            (*printf "error: `%s'\n" m;*)
            eval_err msg

(* Make actual selection assuming that shapes of the object and index match.  *)
and eval_selection st env p1 p2 =
    let v1 = st_lookup st p1 in
    let idx_shp_vec, idx_data_vec = value_array_to_pair (st_lookup st p2) in
    match v1 with
    | VTrue
    | VFalse
    | VNum (_) ->
            (st, p1)
    | VArray (shp_vec, data_vec) ->
            let offset = idx_to_offset shp_vec idx_data_vec in
            let v = List.nth data_vec offset in
            add_fresh_val_as_result st v

(* evaluate a list of expressions propagating storage at every recursive call;
   return a tuple: (last storage, list of pointers)  *)
and eval_expr_lst st env lst =
    match lst with
    | [] ->
            (st, [])
    | e :: tl ->
            let st, p = eval st env e in
            let st, res = eval_expr_lst st env tl in
            (st, p :: res)


(* FIXME this is a generic function that can be used instead
         of force_imap and force_filter.  *)
and force_obj_to_array st env p loc =
    let st, shp_p = shape st env p in
    let _, shp_vec = value_array_to_pair shp_p in
    let v = st_lookup st p in
    if value_is_array v
       || value_is_num v
       || value_is_true v
       || value_is_false v
    then
        st
    else
        let lb = List.map (fun x -> mk_int_value 0) shp_vec in
        let rec _force st idx_it lb ub p res =
            if idx_it = Done then
                (st, res)
            else
                let idx = get_iterator_idx idx_it in
                let p_idx = fresh_ptr_name () in
                let st = st_add st p_idx @@ mk_vector idx in
                (* FIXME pass location throught the error message.  *)
                let st, p_el = eval_obj_sel st env p p_idx
                               @@ sprintf "force_obj_to_array (%s).[%s] failed"
                                  (value_to_str @@ st_lookup st p) (val_lst_to_str idx) in
                _force st (lexi_next idx_it lb ub) lb ub p ((st_lookup st p_el) :: res)
        in

        let idx_it = if value_num_vec_lt lb shp_vec then (Nxt (lb)) else Done in
        let st, res = _force st idx_it lb shp_vec p [] in
        st_update st p (mk_array_value shp_vec (List.rev res))


(* evaluate gen-expr list into vgen-expr list  *)
and eval_gen_expr_lst st env idx_shp_vec ge_lst =
    match ge_lst with
    | [] ->
            (st, [])
    | ((lb, var, ub), e) :: tl ->
            let st, p1 = eval st env lb in
            let st, p2 = eval st env ub in
            let st, shp_p1 = shape st env p1 in
            let st, shp_p2 = shape st env p2 in
            let _, shp_p1_vec = value_array_to_pair shp_p1 in
            let _, shp_p2_vec = value_array_to_pair shp_p2 in
            let v1 = st_lookup st p1 in
            let v2 = st_lookup st p2 in

            if shp_p1 <> mk_vector idx_shp_vec then
                eval_err_loc lb.loc @@ sprintf "wrong shape for the lower bound: %s-element vector expected"
                                   (val_lst_to_str idx_shp_vec);

            if shp_p2 <> mk_vector idx_shp_vec then
                eval_err_loc ub.loc @@ sprintf "wrong shape for the upper bound: %s-element vector expected"
                                   (val_lst_to_str idx_shp_vec);

            (* Extract these values again, as we might have updated the pointers.  *)
            let v1 = st_lookup st p1 in
            let v2 = st_lookup st p2 in

            if not @@ value_is_array v1 then
                eval_err_loc lb.loc "lower bound is not array";
            if not (value_is_array v2) then
                eval_err_loc ub.loc "upper bound is not array";
            let st, res = eval_gen_expr_lst st env idx_shp_vec tl in
            (st, ((v1, var, v2), EPexpr (e)) :: res)
