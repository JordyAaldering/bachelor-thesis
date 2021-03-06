open Printf

type expr =
    (* variables *)
    | EVar of string
    | EFloat of float
    | EArray of expr list
    (* expressions *)
    | EApply of expr * expr
    | ELambda of string * expr
    | ELet of string * expr * expr
    | ECond of expr * expr * expr
    | EWith of expr * expr * expr * string * expr * expr
    (* operands *)
    | EBinary of bop * expr * expr
    | EUnary of uop * expr
    (* primitive functions *)
    | ESel of expr * expr
    | EShape of expr
    | EDim of expr

and bop =
    | OpAppend
    | OpAdd
    | OpMin
    | OpMul
    | OpDiv
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe

and uop =
    | OpNeg
    | OpAbs
    | OpNot

let is_equality_bop (op: bop) : bool =
    match op with
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe -> true
    | _ -> false

let is_equality_uop (op: uop) : bool =
    match op with
    | OpNot -> true
    | _ -> false

let bop_to_str (op: bop) : string =
    match op with
    | OpAppend -> "++"
    | OpAdd    -> "+"
    | OpMin    -> "-"
    | OpMul    -> "*"
    | OpDiv    -> "/"
    | OpEq     -> "="
    | OpNe     -> "!="
    | OpLt     -> "<"
    | OpLe     -> "<="
    | OpGt     -> ">"
    | OpGe     -> ">="

let uop_to_str (op: uop) : string =
    match op with
    | OpNeg -> "~"
    | OpAbs -> "|"
    | OpNot -> "!"

let rec expr_to_str (e: expr) : string =
    match e with
    (* variables *)
    | EVar s -> s
    | EFloat x -> sprintf "%g" x
    | EArray xs ->
        sprintf "[%s]"
            (String.concat ", " (List.map expr_to_str xs))
    (* expressions *)
    | EApply (e1, e2) ->
        sprintf "%s %s"
            (decide_paren e1) (decide_paren e2)
    | ELambda (s, e) ->
        sprintf "\\%s. %s"
            s (expr_to_str e)
    | ELet (s, e1, e2) ->
        sprintf "let %s = %s in\n%s"
            s (expr_to_str e1) (expr_to_str e2)
    | ECond (e1, e2, e3) ->
        sprintf "if %s then %s else %s"
            (expr_to_str e1) (expr_to_str e2) (expr_to_str e3)
    | EWith (e1, e2, e3, s, e4, e5) ->
        sprintf "gen %s %s with %s <= %s < %s in %s"
            (expr_to_str e1) (expr_to_str e2) (expr_to_str e3) s (expr_to_str e4) (expr_to_str e5)
    (* operands *)
    | EBinary (op, e1, e2) ->
        sprintf "%s %s %s"
            (decide_paren e1) (bop_to_str op) (decide_paren e2)
    | EUnary (op, e1) ->
        sprintf "%s%s"
            (uop_to_str op) (decide_paren e1)
    (* primitive functions *)
    | ESel (e1, e2) ->
        sprintf "%s.(%s)"
            (decide_paren e1) (expr_to_str e2)
    | EShape e1 ->
        sprintf "shape %s"
            (decide_paren e1)
    | EDim e1 ->
        sprintf "dim %s"
            (decide_paren e1)

and decide_paren (e: expr) : string =
    match e with
    | EVar _
    | EFloat _
    | EArray _ -> expr_to_str e
    | _ -> sprintf "(%s)" (expr_to_str e)
