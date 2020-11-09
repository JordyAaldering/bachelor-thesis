open Printf

exception ValueFailure of string

type value =
    | Const of float
    | Vect of int list * float list

type expr_or_ptr =
    | EPptr of string
    | EPexpr of Ast.expr

(* A shortcut for raising an exception *)
let value_err msg = raise (ValueFailure msg)

let shp_to_str shp = String.concat ", " (List.map string_of_int shp)

let data_to_str data = String.concat ", " (List.map string_of_float data)

let value_to_str v = match v with
    | Const x -> string_of_float x
    | Vect (shp, data) -> sprintf "<[%s], [%s]>"
        (shp_to_str shp) (data_to_str data)


(** Constructors **)

let mk_value_true = Const 1.

let mk_value_false = Const 0.

let mk_value_const x = Const x

let mk_value_vect shp data =
    if List.length shp = 0 then
        value_err @@ sprintf "Vector cannot have empty shape";
    let el_count = List.fold_left (fun res x -> res * x) 1 shp in
    if List.length data <> el_count then
        value_err @@ sprintf "Shape [%s] does not match data [%s]"
            (shp_to_str shp) (data_to_str data);
    Vect (shp, data)


(** Predicates **)

let value_is_const v = match v with
    | Const _ -> true
    | Vect _ -> false

let value_is_vect v = match v with
    | Const _ -> false
    | Vect _ -> true

let value_to_int v = match v with
    | Const x -> int_of_float x
    | Vect _ -> value_err @@ sprintf "Can only get int value of constant"

let value_to_float v = match v with
    | Const x -> x
    | Vect _ -> value_err @@ sprintf "Can only get int value of constant"

let value_add v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x +. y)
    | _ -> value_err @@ sprintf "Can only add two constants"

let value_mult v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x *. y)
    | _ -> value_err @@ sprintf "Can only multiply two constants"

let value_eq v1 v2 = match v1, v2 with
    | Const x, Const y -> x = y
    | Vect (_shp1, data1), Vect (_shp2, data2) ->
        List.fold_left2 (fun res x y ->
            if not res then res
            else x = y
        ) true data1 data2
    | _ -> value_err @@ sprintf "Can only compare two constants or two vectors"

let value_lt v1 v2 = match v1, v2 with
    | Const x, Const y -> x < y
    | Vect (_shp1, data1), Vect (_shp2, data2) ->
        List.fold_left2 (fun res x y ->
            if not res then res
            else x < y
        ) true data1 data2
    | _ -> value_err @@ sprintf "Can only compare two constants or two vectors"

let value_le v1 v2 = match v1, v2 with
    | Const x, Const y -> x = y || x < y
    | Vect (_shp1, data1), Vect (_shp2, data2) ->
        List.fold_left2 (fun res x y ->
            if not res then res
            else x = y || x < y
        ) true data1 data2
    | _ -> value_err @@ sprintf "Can only compare two constants or two vectors"

let value_to_pair v = match v with
    | Vect (shp, data) -> (shp, data)
    | _ -> value_err @@ sprintf "Can only make pair of vector"
