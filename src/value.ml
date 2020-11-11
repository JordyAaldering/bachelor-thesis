open Printf

exception ValueFailure of string

type value =
    | Const of float
    | Vect of value list * value list

let rec value_to_str v = match v with
    | Const x -> string_of_float x
    | Vect (shp, data) -> sprintf "<[%s], [%s]>"
        (value_lst_to_str shp) (value_lst_to_str data)

and value_lst_to_str v =
    String.concat ", " (List.map value_to_str v)

(* A shortcut for raising an exception *)
let value_err msg = raise (ValueFailure msg)


(** Primitive Functions **)

let value_sel iv v = match iv, v with
    | Const i, Vect (shp, data) -> List.nth data (int_of_float i)
    | _ -> value_err "Can only select in a 1d vector"

let value_shape v = match v with
    | Const _x -> Const 0.
    | Vect (shp, _data) -> Vect ([Const (float_of_int @@ List.length shp)], shp)

let value_dim v = match v with
    | Const _x -> Const 0.
    | Vect (shp, _data) -> Const (float_of_int @@ List.length shp)


(** Predicates **)

let rec value_neg v = match v with
    | Const x -> Const (-.x)
    | Vect (shp, data) -> Vect (shp, List.map value_neg data)

and value_add v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x +. y)
    | Vect (shp1, xs), Vect (shp2, ys) -> Vect (shp1, List.map2 value_add xs ys)
    | _ -> value_err "Can only operate on two constants or two vectors"

and value_mul v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x *. y)
    | Vect (shp1, xs), Vect (shp2, ys) -> Vect (shp1, List.map2 value_mul xs ys)
    | _ -> value_err "Can only operate on two constants or two vectors"

and value_div v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x /. y)
    | Vect (shp1, xs), Vect (shp2, ys) -> Vect (shp1, List.map2 value_div xs ys)
    | _ -> value_err "Can only operate on two constants or two vectors"


let rec value_is_truthy v = match v with
    | Const x -> x <> 0.
    | Vect (_shp, data) -> List.for_all value_is_truthy data

and value_not v =
    Const (if value_is_truthy v then 0. else 1.)

and value_eq v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (if x = y then 1. else 0.)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        List.fold_left2 (fun res x y ->
                value_mul res (value_eq x y)
            ) (Const 1.) xs ys
    | _ -> value_err "Can only operate on two constants or two vectors"

and value_gt v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (if x > y then 1. else 0.)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        List.fold_left2 (fun res x y ->
                value_mul res (value_gt x y)
            ) (Const 1.) xs ys
    | _ -> value_err "Can only operate on two constants or two vectors"

and value_lt v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (if x < y then 1. else 0.)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        List.fold_left2 (fun res x y ->
                value_mul res (value_lt x y)
            ) (Const 1.) xs ys
    | _ -> value_err "Can only operate on two constants or two vectors"


(** Conversions **)

let value_to_pair v = match v with
    | Const x -> ([Const 0.], [Const x])
    | Vect (shp, data) -> (shp, data)
