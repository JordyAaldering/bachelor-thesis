exception ImapFailure of string

type expr = {
    loc: Loc.t;
    expr_kind: expr_kind
}

and expr_kind =
    | ETrue
    | EFalse
    | ENum of float
    | EVar of string
    | EArray of expr list

    | EBinary of binop * expr * expr
    | EUnary of unaryop * expr

    | ELambda of string * expr
    | EApply of expr * expr
    | ELetIn of string * expr * expr

    | ESel of expr * expr
    | EShape of expr
    | EDim of expr

and binop =
    | OpPlus
    | OpMinus
    | OpMult
    | OpDiv
    | OpMod
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe

and unaryop =
    | OpNegative
    | OpNot

and generator = expr * string * expr


(** Constructors  **)

let mk_etrue  ?(loc=Loc.internal) () = { loc=loc; expr_kind=ETrue }
let mk_efalse ?(loc=Loc.internal) () = { loc=loc; expr_kind=EFalse }
let mk_enum   ?(loc=Loc.internal) c  = { loc=loc; expr_kind=ENum c }
let mk_evar   ?(loc=Loc.internal) x  = { loc=loc; expr_kind=EVar x }
let mk_earray ?(loc=Loc.internal) xs = { loc=loc; expr_kind=EArray xs }

let mk_ebinary op lhs rhs = let {loc=l} = lhs in { loc=l; expr_kind=EBinary (op, lhs, rhs) }
let mk_eunary ?(loc=Loc.internal) op arg = { loc=loc; expr_kind=EUnary (op, arg) }

let mk_elambda ?(loc=Loc.internal) x body = { loc=loc; expr_kind=ELambda (x, body) }
let mk_eapply lhs rhs = let {loc=l} = lhs in { loc=l; expr_kind=EApply (lhs, rhs) }

let mk_esel iv v = let {loc=l} = iv in { loc=l; expr_kind=ESel (iv, v) }
let mk_eshape ?(loc=Loc.internal) v = { loc=loc; expr_kind=EShape v }
let mk_edim ?(loc=Loc.internal) v = { loc=loc; expr_kind=EDim v }

let mk_eletin ?(loc=Loc.internal) x e1 e2 = { loc=loc; expr_kind=ELetIn (x, e1, e2) }


(** Predicates **)

let expr_is_lambda e =
    match e with
    | { expr_kind=ELambda (_, _) } -> true
    | _ -> false

let expr_get_var_name e =
    match e with
    | { expr_kind=EVar (x) } -> Some (x)
    | _ -> None


let rec cmp_ast_noloc e1 e2 =
    match e1, e2 with
    | { expr_kind=ETrue }, { expr_kind=ETrue } -> true
    | { expr_kind=EFalse }, { expr_kind=EFalse } -> true
    | { expr_kind=ENum x }, { expr_kind=ENum y } -> x = y
    | { expr_kind=EVar x }, { expr_kind=EVar y } -> x = y
    | { expr_kind=EArray xs }, { expr_kind=EArray ys } ->
        List.length xs = List.length ys
        && (List.fold_left2 (fun res x y -> res && cmp_ast_noloc x y) true xs ys)

    | { expr_kind=EBinary (op1, x1, y1) }, { expr_kind=EBinary (op2, x2, y2) } ->
        op1 = op2
        && cmp_ast_noloc x1 x1
        && cmp_ast_noloc y1 y2
    | { expr_kind=EUnary (op1, x1) }, { expr_kind=EUnary (op2, x2) } ->
        op1 = op2
        && cmp_ast_noloc x1 x2

    | { expr_kind=ESel (x1, y1) }, { expr_kind=ESel (x2, y2) } ->
        cmp_ast_noloc x1 x2
        && cmp_ast_noloc y1 y2
    | { expr_kind=EShape x }, { expr_kind=EShape y } ->
        cmp_ast_noloc x y
    | { expr_kind=EDim x }, { expr_kind=EDim y } ->
        cmp_ast_noloc x y

    | { expr_kind=ELambda (v1, e1) }, { expr_kind=ELambda (v2, e2) } ->
        v1 = v2
        && cmp_ast_noloc e1 e2
    | { expr_kind=EApply (x1, y1) }, { expr_kind=EApply (x2, y2) } ->
        cmp_ast_noloc x1 x2
        && cmp_ast_noloc y1 y2
    | { expr_kind=ELetIn (v1, x1, y1) }, { expr_kind=ELetIn (v2, x2, y2) } ->
        v1 = v2
        && cmp_ast_noloc x1 x2
        && cmp_ast_noloc y1 y2

    | _ -> false
