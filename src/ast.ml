open Printf

exception ParseFailure of string

type expr =
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


(** Predicates **)

let expr_get_var_name e = match e with
    | EVar x -> Some x
    | _ -> None

let rec ast_cmp e1 e2 = match e1, e2 with
    | EVar x, EVar y -> x = y
    | EConst x, EConst y -> x = y
    | EArray xs, EArray ys ->
        List.length xs = List.length ys
        && (List.fold_left2 (fun res x y -> res && ast_cmp x y) true xs ys)

    | EApply (x1, y1), EApply (x2, y2) ->
        ast_cmp x1 x2
        && ast_cmp y1 y2
    | ELambda (v1, e1), ELambda (v2, e2) ->
        v1 = v2
        && ast_cmp e1 e2        
    | ELetIn (v1, x1, y1), ELetIn (v2, x2, y2) ->
        v1 = v2
        && ast_cmp x1 x2
        && ast_cmp y1 y2
    | EIfThen (x1, y1, z1), EIfThen (x2, y2, z2) ->
        ast_cmp x1 x2
        && ast_cmp y1 y2
        && ast_cmp z1 z2

    | EBinary (op1, x1, y1), EBinary (op2, x2, y2) ->
        op1 = op2
        && ast_cmp x1 x1
        && ast_cmp y1 y2
    | EUnary (op1, x1), EUnary (op2, x2) ->
        op1 = op2
        && ast_cmp x1 x2
    | ESel (x1, y1), ESel (x2, y2) ->
        ast_cmp x1 x2
        && ast_cmp y1 y2
    | EShape x, EShape y ->
        ast_cmp x y
    | EDim x, EDim y ->
        ast_cmp x y
    
    | _ -> false


(** Printing **)

let rec expr_to_str e = match e with
    | EVar x -> x
    | EConst x -> string_of_float x
    | EArray xs -> sprintf "[%s]"
        (String.concat ", " (List.map expr_to_str xs))

    | EApply (e1, e2) ->
        sprintf "((%s) (%s))" (expr_to_str e1) (expr_to_str e2)
    | ELambda (x, e) ->
        sprintf "\\%s.(%s)" x (expr_to_str e)
    | ELetIn (x, e1, e2) ->
        sprintf "let %s = %s in\n%s" x (expr_to_str e1) (expr_to_str e2)
    | EIfThen (e1, e2, e3) ->
        sprintf "if %s then %s else %s" (expr_to_str e1) (expr_to_str e2) (expr_to_str e3)

    | EBinary (bop, e1, e2) ->
        sprintf "(%s) %s (%s)" (expr_to_str e1) (bop_to_str bop) (expr_to_str e2)
    | EUnary (uop, e1) ->
        sprintf "%s(%s)" (uop_to_str uop) (expr_to_str e1)
    | ESel (e1, e2) ->
        sprintf "sel (%s) (%s)" (expr_to_str e1) (expr_to_str e2)
    | EShape e1 ->
        sprintf "shape (%s)" (expr_to_str e1)
    | EDim e1 ->
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
