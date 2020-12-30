open Printf

type expr =
    (* variables *)
    | EVar of string
    | EFloat of float
    | EArray of expr list
    (* expressions *)
    | EApply of expr * expr
    | ELambda of string * expr
    | ELetIn of string * expr * expr
    | EIfThen of expr * expr * expr
    (* operands *)
    | EBinary of bop * expr * expr
    | EUnary of uop * expr
    (* primitive functions *)
    | ESel of expr * expr
    | EShape of expr
    | EDim of expr
    | ERead

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

let bop_to_str op = match op with
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

let uop_to_str op = match op with
    | OpNeg -> "-"
    | OpNot -> "!"

let rec expr_to_str ?(newline=false) e = match e with
    (* variables *)
    | EVar s -> s
    | EFloat x -> sprintf "%g" x
    | EArray xs -> sprintf "[%s]"
        (String.concat ", " (List.map expr_to_str xs))
    (* expressions *)
    | EApply (e1, e2) -> sprintf "%s %s"
        (decide_paren ~newline:newline e1) (decide_paren ~newline:newline e2)
    | ELambda (x, e) -> sprintf "\\%s. %s"
        x (expr_to_str ~newline:newline e)
    | ELetIn (x, e1, e2) -> sprintf "let %s = %s in%s%s"
        x (expr_to_str ~newline:newline e1) (if newline then "\n" else " ") (expr_to_str ~newline:newline e2)
    | EIfThen (e1, e2, e3) -> sprintf "if %s then %s else %s"
        (expr_to_str ~newline:newline e1) (expr_to_str ~newline:newline e2) (expr_to_str ~newline:newline e3)
    (* operands *)
    | EBinary (op, e1, e2) -> sprintf "%s %s %s"
        (decide_paren ~newline:newline e1) (bop_to_str op) (decide_paren ~newline:newline e2)
    | EUnary (op, e1) -> sprintf "%s%s"
        (uop_to_str op) (decide_paren ~newline:newline e1)
    (* primitive functions *)
    | ESel (e1, e2) -> sprintf "%s.(%s)"
        (decide_paren ~newline:newline e1) (expr_to_str ~newline:newline e2)
    | EShape e1 -> sprintf "shape %s"
        (decide_paren ~newline:newline e1)
    | EDim e1 -> sprintf "dim %s"
        (decide_paren ~newline:newline e1)
    | ERead -> "read"

and decide_paren ?(newline=false) e = match e with
    | EVar _
    | EFloat _
    | EArray _
    | ERead -> expr_to_str ~newline:newline e
    | _ -> sprintf "(%s)" (expr_to_str ~newline:newline e)
