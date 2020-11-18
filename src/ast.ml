open Printf

exception ParseFailure of string
let parse_err msg = raise @@ ParseFailure msg

type expr =
    | EVar of string
    | ENum of float
    | EArray of expr list

    | EApply of expr * expr
    | ELambda of string * expr
    | ELetIn of string * expr * expr
    | EIfThen of expr * expr * expr

    | EBinary of binary_op * expr * expr
    | EUnary of unary_op * expr
    | ESel of expr * expr
    | EShape of expr
    | EDim of expr

and binary_op =
    | OpPlus
    | OpMin
    | OpMult
    | OpDiv
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe

and unary_op =
    | OpNeg
    | OpNot

let expr_get_var_name e = match e with
    | EVar x -> Some x
    | _ -> None

let rec expr_to_str ?(sep=" ") e = match e with
    | EVar x -> x
    | ENum x -> string_of_float x
    | EArray xs -> sprintf "[%s]"
        (String.concat ", " (List.map expr_to_str xs))

    | EApply (e1, e2) -> sprintf "(%s) (%s)"
        (expr_to_str ~sep:sep e1) (expr_to_str ~sep:sep e2)
    | ELambda (x, e) -> sprintf "\\%s. %s"
        x (expr_to_str ~sep:sep e)
    | ELetIn (x, e1, e2) -> sprintf "let %s = %s in%s%s"
        x (expr_to_str ~sep:sep e1) sep (expr_to_str ~sep:sep e2)
    | EIfThen (e1, e2, e3) -> sprintf "if %s then %s else %s"
        (expr_to_str ~sep:sep e1) (expr_to_str ~sep:sep e2) (expr_to_str ~sep:sep e3)

    | EBinary (bop, e1, e2) -> sprintf "((%s) %s (%s))"
        (expr_to_str ~sep:sep e1) (bop_to_str bop) (expr_to_str ~sep:sep e2)
    | EUnary (uop, e1) -> sprintf "(%s (%s))"
        (uop_to_str uop) (expr_to_str ~sep:sep e1)
    | ESel (e1, e2) -> sprintf "sel (%s) (%s)"
        (expr_to_str ~sep:sep e1) (expr_to_str ~sep:sep e2)
    | EShape e1 -> sprintf "shape (%s)"
        (expr_to_str ~sep:sep e1)
    | EDim e1 -> sprintf "dim (%s)"
        (expr_to_str ~sep:sep e1)

and bop_to_str bop = match bop with
    | OpPlus -> "+"
    | OpMin  -> "-"
    | OpMult -> "*"
    | OpDiv  -> "/"
    | OpEq -> "="
    | OpNe -> "!="
    | OpLt -> "<"
    | OpLe -> "<="
    | OpGt -> ">"
    | OpGe -> ">="

and uop_to_str uop = match uop with
    | OpNeg -> "-"
    | OpNot -> "!"
