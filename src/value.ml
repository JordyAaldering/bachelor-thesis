exception ValueFailure of string

(* Represents a single value as (shape, data) *)
type value = int list * float list

and expr_or_ptr =
    | EPptr of string
    | EPexpr of Ast.expr

(* A shortcut for raising an exception *)
let value_err msg = raise (ValueFailure msg)

let rec value_to_str v = match v with
    | { [], [x] }   -> string_of_float x
    | { shp, data } -> sprintf "<[%s], [%s]>" (list_to_str shp) (list_to_str data)

and list_to_str lst =
    String.concat ", " (List.map value_to_str lst)
