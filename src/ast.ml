open Printf

exception ParseFailure of string

let parse_err msg = raise @@ ParseFailure msg

type expr = {
    kind: expr_kind;
    dem_env: (string * int array) list
}

and expr_kind =
    | EVar of string
    | EConst of float
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


let mk_expr_var x = { kind=EVar x; dem_env=[] }
let mk_expr_const x = { kind=EConst x; dem_env=[] }
let mk_expr_array xs = { kind=EArray xs; dem_env=[] }
let mk_expr_apply e1 e2 = { kind=EApply (e1, e2); dem_env=[] }
let mk_expr_lambda x e = { kind=ELambda (x, e); dem_env=[] }
let mk_expr_letin x e1 e2 = { kind=ELetIn (x, e1, e2); dem_env=[] }
let mk_expr_ifthen e1 e2 e3 = { kind=EIfThen (e1, e2, e3); dem_env=[] }
let mk_expr_binary op e1 e2 = { kind=EBinary (op, e1, e2); dem_env=[] }
let mk_expr_unary op e = { kind=EUnary (op, e); dem_env=[] }
let mk_expr_sel e1 e2 = { kind=ESel (e1, e2); dem_env=[] }
let mk_expr_shape e = { kind=EShape e; dem_env=[] }
let mk_expr_dim e = { kind=EDim e; dem_env=[] }


let expr_get_var_name e = match e with
    | { kind=EVar x } -> Some x
    | _ -> None

let rec expr_to_str e = match e with
    | { kind=EVar x } -> x
    | { kind=EConst x } -> string_of_float x
    | { kind=EArray xs } -> sprintf "[%s]"
        (String.concat ", " (List.map expr_to_str xs))

    | { kind=EApply (e1, e2) } ->
        sprintf "((%s) (%s))" (expr_to_str e1) (expr_to_str e2)
    | { kind=ELambda (x, e) } ->
        sprintf "\\%s.(%s)" x (expr_to_str e)
    | { kind=ELetIn (x, e1, e2) } ->
        sprintf "let %s = %s in\n%s" x (expr_to_str e1) (expr_to_str e2)
    | { kind=EIfThen (e1, e2, e3) } ->
        sprintf "if %s then %s else %s" (expr_to_str e1) (expr_to_str e2) (expr_to_str e3)

    | { kind=EBinary (bop, e1, e2) } ->
        sprintf "(%s) %s (%s)" (expr_to_str e1) (bop_to_str bop) (expr_to_str e2)
    | { kind=EUnary (uop, e1) } ->
        sprintf "%s(%s)" (uop_to_str uop) (expr_to_str e1)
    | { kind=ESel (e1, e2) } ->
        sprintf "sel (%s) (%s)" (expr_to_str e1) (expr_to_str e2)
    | { kind=EShape e1 } ->
        sprintf "shape (%s)" (expr_to_str e1)
    | { kind=EDim e1 } ->
        sprintf "dim (%s)" (expr_to_str e1)

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
