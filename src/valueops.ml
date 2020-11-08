open Value
open Print
open Printf

(** Predicates **)

let value_is_true v = match v with
    | VTrue -> true
    | _ -> false

let value_is_false v = match v with
    | VFalse -> true
    | _ -> false

let value_is_num v = match v with
    | VNum (o) -> true
    | _ -> false

let value_is_array v = match v with
    | VArray (_, _) -> true
    | _ -> false

let value_is_selectable v = match v with
    | VTrue
    | VFalse
    | VNum (_)
    | VArray (_, _) -> true


(** Constructors **)

let mk_true_value = VTrue

let mk_false_value = VFalse

let mk_int_value n = VNum (n)

let mk_array_value shp_vec data_vec =
    if not @@ List.for_all (fun x -> value_is_num x) shp_vec then
        value_err @@ sprintf "mk_array: invalid shape vector [%s]" @@ val_lst_to_str shp_vec;
    let elcount = List.fold_left (fun res x -> match x with
            | VNum o -> res * o
            | _ -> failwith "non-number found in shape"
        ) 1 shp_vec
    in
    if elcount <> List.length data_vec then
        value_err @@ sprintf "mk_array: shape [%s] does not match data [%s]"
            (val_lst_to_str shp_vec) (val_lst_to_str data_vec);
    if shp_vec = [] then
        List.hd data_vec
    else
        VArray (shp_vec, data_vec)

let mk_empty_vector () = VArray ([mk_int_value 0], [])

let mk_vector value_vec = VArray ([mk_int_value @@ List.length value_vec], value_vec)

let value_num_to_int v = match v with
    | VNum x -> x
    | _ -> value_err @@ sprintf "value_num_to_int called with `%s'" (value_to_str v)

let value_num_add v1 v2 = match v1, v2 with
    | VNum x, VNum y -> VNum (x + y)
    | _ -> value_err @@ sprintf "value_num_add invalid parameters"

let value_num_mult v1 v2 = match v1, v2 with
    | VNum x, VNum y -> VNum (x * y)
    | _ -> value_err @@ sprintf "value_num_mult invalid parameters"

(* This function returns an integer. *)
let value_num_compare v1 v2 = if v1 = v2 then 1 else 0

let value_array_to_pair v = match v with
    | VArray (s, d) -> (s, d)
    | _ -> value_err @@ sprintf "value_array_to_pair called with `%s'" (value_to_str v)

let value_num_vec_lt l r =
    List.fold_left2 (fun r x y ->
            if not r then
                r
            else
                value_num_compare x y = -1
        ) true l r

let value_num_vec_le l r =
    List.fold_left2 (fun r x y ->
            if not r then
                r
            else
                let cmp = value_num_compare x y in
                cmp = -1 || cmp = 0
        ) true l r

let value_num_vec_in_vgen vec vgen =
    let lb, x, ub = vgen in
    let _, lb_data_vec = value_array_to_pair lb in
    let _, ub_data_vec = value_array_to_pair ub in
    value_num_vec_le lb_data_vec vec && value_num_vec_lt vec ub_data_vec
