exception ValueFailure of string

type value =
    | VTrue
    | VFalse
    | VNum of int
    | VArray of value list * value list

and vgen = value * string * value

and expr_or_ptr =
    | EPptr of string
    | EPexpr of Ast.expr

(* A shortcut for raising an exception *)
let value_err msg = raise (ValueFailure msg)
