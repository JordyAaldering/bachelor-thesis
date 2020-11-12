open Printf

exception ParseFailure of string

type expr = {
    kind: expr_kind
}

and expr_kind =
    | EVar of string
    | EConst of float
    | EVect of expr list

    | EApply of expr * expr
    | ELetIn of string * expr * expr
    | EIfThen of expr * expr * expr

    | EBinary of binary_op * expr * expr
    | EUnary of unary_op * expr
    | ESel of expr * expr
    | EShape of expr
    | EDim of expr

and binary_op =
    | OpPlus
    | OpMinus
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


(** Constructors  **)

let mk_expr_var x =
    { kind=EVar x }

let mk_expr_const x =
    { kind=EConst x }

let mk_expr_array xs =
    { kind=EVect xs }

let mk_expr_apply lhs rhs =
    { kind=EApply (lhs, rhs) }

let mk_expr_letin x e1 e2 =
    { kind=ELetIn (x, e1, e2) }

let mk_expr_ifthen p t f =
    { kind=EIfThen (p, t, f) }

let mk_expr_binary op lhs rhs =
    { kind=EBinary (op, lhs, rhs) }

let mk_expr_unary op arg =
    { kind=EUnary (op, arg) }

let mk_expr_sel iv v =
    { kind=ESel (iv, v) }

let mk_expr_shape v =
    { kind=EShape v }

let mk_expr_dim v =
    { kind=EDim v }


(** Predicates **)

let expr_get_var_name e = match e with
    | { kind=EVar x } -> Some x
    | _ -> None

let rec ast_cmp e1 e2 = match e1, e2 with
    | { kind=EVar x }, { kind=EVar y } -> x = y
    | { kind=EConst x }, { kind=EConst y } -> x = y
    | { kind=EVect xs }, { kind=EVect ys } ->
        List.length xs = List.length ys
        && (List.fold_left2 (fun res x y -> res && ast_cmp x y) true xs ys)

    | { kind=EApply (x1, y1) }, { kind=EApply (x2, y2) } ->
        ast_cmp x1 x2
        && ast_cmp y1 y2
    | { kind=ELetIn (v1, x1, y1) }, { kind=ELetIn (v2, x2, y2) } ->
        v1 = v2
        && ast_cmp x1 x2
        && ast_cmp y1 y2
    | { kind=EIfThen (x1, y1, z1) }, { kind=EIfThen (x2, y2, z2) } ->
        ast_cmp x1 x2
        && ast_cmp y1 y2
        && ast_cmp z1 z2

    | { kind=EBinary (op1, x1, y1) }, { kind=EBinary (op2, x2, y2) } ->
        op1 = op2
        && ast_cmp x1 x1
        && ast_cmp y1 y2
    | { kind=EUnary (op1, x1) }, { kind=EUnary (op2, x2) } ->
        op1 = op2
        && ast_cmp x1 x2
    | { kind=ESel (x1, y1) }, { kind=ESel (x2, y2) } ->
        ast_cmp x1 x2
        && ast_cmp y1 y2
    | { kind=EShape x }, { kind=EShape y } ->
        ast_cmp x y
    | { kind=EDim x }, { kind=EDim y } ->
        ast_cmp x y
    
    | _ -> false


(** Printing **)

let rec expr_to_str e = match e with
    | { kind=EVar x } -> x
    | { kind=EConst x } -> string_of_float x
    | { kind=EVect xs } -> sprintf "[%s]"
        (String.concat ", " (List.map expr_to_str xs))

    | { kind=EApply (e1, e2) } ->
        sprintf "((%s) (%s))" (expr_to_str e1) (expr_to_str e2)
    | { kind=ELetIn (x, e1, e2) } ->
        sprintf "let %s = %s in %s" x (expr_to_str e1) (expr_to_str e2)
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
    | OpMinus -> "-"
    | OpMult -> "*"
    | OpDiv -> "/"
    | OpEq -> "="
    | OpNe -> "!="
    | OpLt -> "<"
    | OpLe -> "<="
    | OpGt -> ">"
    | OpGe -> ">="

and uop_to_str uop = match uop with
    | OpNeg -> "-"
    | OpNot -> "!"
