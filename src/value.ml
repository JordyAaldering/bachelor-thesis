open Printf

exception ValueFailure of string

type value =
    | Vect of int list * float list

type expr_or_ptr =
    | EPptr of string
    | EPexpr of Ast.expr


(* A shortcut for raising an exception *)
let value_err msg = raise (ValueFailure msg)

let shp_to_str shp = String.concat ", " (List.map string_of_int shp)

let data_to_str data = String.concat ", " (List.map string_of_float data)

let value_to_str v = match v with
    | Vect (shp, data) -> sprintf "<[%s], [%s]>"
        (shp_to_str shp) (data_to_str data)


let value_to_float v = match v with
    | Vect  ([], [x]) -> x
    | _ -> value_err "Can only get float value of const"

let value_to_int v = match v with
    | Vect  ([], [x]) -> int_of_float x
    | _ -> value_err "Can only get float value of const"


(** Constructors **)

let mk_value_const x =
    Vect ([], [x])

let mk_value_vect shp data =
    if List.length shp = 0 then
        value_err @@ sprintf "Vector must have non-empty shape";
    Vect (shp, data)

(** Predicates **)

let value_neg v = match v with
    | Vect (shp, data) -> Vect (shp, List.map (fun x -> -.x) data)

let value_not v = match v with
    | Vect (shp, data) -> Vect (shp, List.map (fun x -> if x = 0. then 0. else 1.) data)

let value_sel iv v = match v with
    | Vect (shp, data) -> Vect ([], [1.])

let value_shape v = match v with
    | Vect (shp, _data) -> Vect ([List.length shp], List.map (fun x -> float_of_int x) shp)

let value_dim v = match v with
    | Vect (shp, _data) -> Vect ([], [float_of_int @@ List.length shp])

let value_to_pair v = match v with
    | Vect (shp, data) -> (shp, data)
