open Printf

type expr =
    | EVar of string
    | ENum of float
    | EArray of expr list
    | EApply of expr * expr
    | ELambda of string * expr
    | ELetIn of string * expr * expr
    | EIfThen of expr * expr * expr
    | EBinary of bop * expr * expr
    | EUnary of uop * expr
    | ESel of expr * expr
    | EShape of expr
    | EDim of expr

and bop =
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
    | OpNot

let rec expr_to_str ?(newline=false) e = match e with
    | EVar x -> x
    | ENum x -> string_of_float x
    | EArray xs -> sprintf "[%s]"
        (String.concat ", " (List.map expr_to_str xs))

    | EApply (e1, e2) -> sprintf "(%s) (%s)"
        (expr_to_str ~newline:newline e1) (expr_to_str ~newline:newline e2)
    | ELambda (x, e) -> sprintf "\\%s. %s"
        x (expr_to_str ~newline:newline e)
    | ELetIn (x, e1, e2) -> sprintf "let %s = %s in%s%s"
        x (expr_to_str ~newline:newline e1) (if newline then "\n" else " ") (expr_to_str ~newline:newline e2)
    | EIfThen (e1, e2, e3) -> sprintf "if %s then %s else %s"
        (expr_to_str ~newline:newline e1) (expr_to_str ~newline:newline e2) (expr_to_str ~newline:newline e3)

    | EBinary (op, e1, e2) -> sprintf "(%s) %s (%s)"
        (expr_to_str ~newline:newline e1) (bop_to_str op) (expr_to_str ~newline:newline e2)
    | EUnary (op, e1) -> sprintf "%s(%s)"
        (uop_to_str op) (expr_to_str ~newline:newline e1)
    | ESel (e1, e2) -> sprintf "sel (%s) (%s)"
        (expr_to_str ~newline:newline e1) (expr_to_str ~newline:newline e2)
    | EShape e1 -> sprintf "shape (%s)"
        (expr_to_str ~newline:newline e1)
    | EDim e1 -> sprintf "dim (%s)"
        (expr_to_str ~newline:newline e1)

and bop_to_str bop = match bop with
    | OpAdd -> "+"
    | OpMin -> "-"
    | OpMul -> "*"
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
