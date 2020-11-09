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

let mk_value_const x = Const x

let mk_value_vect shp data =
    if List.length shp = 0 then
        value_err @@ sprintf "Vector cannot have empty shape";
    let el_count = List.fold_left (fun res x -> res * x) 1 shp in
    if List.length data <> el_count then
        value_err @@ sprintf "Shape [%s] does not match data [%s]"
            (shp_to_str shp) (data_to_str data);
    Vect (shp, data)


(** Rewrites **)

let value_to_pair v = match v with
    | Const x -> ([], [x])
    | Vect (shp, data) -> (shp, data)
