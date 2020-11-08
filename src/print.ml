open Ast
open Value
open Printf

let rec value_to_str v = match v with
    | VFalse -> "vfalse"
    | VTrue -> "vtrue"
    | VNum o -> string_of_int o
    | VArray (shp, data) ->
        sprintf "<[%s], [%s]>" (val_lst_to_str shp) (val_lst_to_str data)

and val_lst_to_str lst =
    String.concat ", " (List.map value_to_str lst)

and expr_to_str e = match e with
    | { expr_kind = ETrue } -> "true"
    | { expr_kind = EFalse } -> "false"
    | { expr_kind = ENum (e1) } -> string_of_int e1
    | { expr_kind = EVar (x) } -> sprintf "%s" x
    | { expr_kind = EArray (e1) } -> sprintf "[%s]" (array_to_str e1)

    | { expr_kind = EApply (e1, e2) } ->
        sprintf "((%s) (%s))" (expr_to_str e1) (expr_to_str e2)
    | { expr_kind = ELetIn (x, e1, e2) } ->
        sprintf "let %s = %s in %s"
            x (expr_to_str e1) (expr_to_str e2)
    | { expr_kind = EIfThen (e1, e2, e3) } ->
        sprintf "if %s then %s else %s"
            (expr_to_str e1) (expr_to_str e2) (expr_to_str e3)

    | { expr_kind = EUnary (uop, e) } -> (
        match uop with
        | OpNeg -> sprintf "-(%s)" (expr_to_str e)
        | OpNot -> sprintf "!(%s)" (expr_to_str e)
    )
    | { expr_kind = EBinary (bop, e1, e2) } ->
        sprintf "%s %s %s" (expr_to_str e1) (bop_to_str bop) (expr_to_str e2)

    | { expr_kind = ESel (e1, e2) } ->
        sprintf "(sel(%s, %s)" (expr_to_str e1) (expr_to_str e2)
    | { expr_kind = EShape e } ->
        sprintf "(shape(%s)" (expr_to_str e)
    | { expr_kind = EDim e } ->
        sprintf "(dim(%s)" (expr_to_str e)

and array_to_str e =
    String.concat ", " (List.map expr_to_str e)

and gen_expr_list_to_str gelst =
    String.concat ", "
        (List.map (fun ge ->
            let (g, e1) = ge in
            sprintf "%s: (%s)" (gen_to_str g) (expr_to_str e1)) gelst)

and gen_to_str g =
    let (e1, x, e2) = g in
    sprintf "%s <= %s < %s" (expr_to_str e1) x (expr_to_str e2)

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
