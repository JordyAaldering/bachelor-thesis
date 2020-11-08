open Ast
open Value
open Printf

let rec val_to_str v = match v with
    | VTrue -> "vtrue"
    | VFalse -> "vfalse"
    | VNum i -> string_of_int i
    | VArray (shp, data) ->
        sprintf "<[%s], [%s]>" (vals_to_str shp) (vals_to_str data)

and vals_to_str lst =
    String.concat ", " (List.map val_to_str lst)

and array_to_str e =
    String.concat ", " (List.map expr_to_str e)

and expr_to_str e = match e with
    | { expr_kind = ETrue } -> "true"
    | { expr_kind = EFalse } -> "false"
    | { expr_kind = ENum x } -> string_of_int x
    | { expr_kind = EVar x } -> sprintf "%s" x
    | { expr_kind = EArray x } -> sprintf "[%s]" (array_to_str x)

    | { expr_kind = EApply (e1, e2) } ->
        sprintf "((%s) (%s))" (expr_to_str e1) (expr_to_str e2)
    | { expr_kind = ELetIn (x, e1, e2) } ->
        sprintf "let %s = %s in %s" x (expr_to_str e1) (expr_to_str e2)
    | { expr_kind = EIfThen (e1, e2, e3) } ->
        sprintf "if %s then %s else %s" (expr_to_str e1) (expr_to_str e2) (expr_to_str e3)

    | { expr_kind = EBinary (bop, e1, e2) } ->
        sprintf "(%s) %s (%s)" (expr_to_str e1) (bop_to_str bop) (expr_to_str e2)
    | { expr_kind = EUnary (uop, e1) } ->
        sprintf "%s(%s)" (uop_to_str uop) (expr_to_str e1)

    | { expr_kind = ESel (e1, e2) } ->
        sprintf "sel(%s, %s)" (expr_to_str e1) (expr_to_str e2)
    | { expr_kind = EShape e1 } ->
        sprintf "shape(%s)" (expr_to_str e1)
    | { expr_kind = EDim e1 } ->
        sprintf "dim(%s)" (expr_to_str e1)

and bop_to_str bop = match bop with
    | OpPlus -> "+"
    | OpMinus -> "-"
    | OpMult -> "*"
    | OpDiv -> "/"
    | OpMod -> "%"
    | OpEq -> "="
    | OpNe -> "!="
    | OpLt -> "<"
    | OpLe -> "<="
    | OpGt -> ">"
    | OpGe -> ">="

and uop_to_str uop = match uop with
    | OpNeg -> "-"
    | OpNot -> "!"
