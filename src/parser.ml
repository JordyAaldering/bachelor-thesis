open Ast
open Loc
open Lexer
open Printf

let opt_get x = match x with
    | Some x -> x
    | None -> raise (Invalid_argument "opt_get")

let op_prec tok = match tok with
    | EQ
    | NE -> 1
    | LT
    | LE
    | GT
    | GE -> 2
    | PLUS
    | MINUS -> 3
    | MULT
    | DIV -> 4
    | _ -> 5

(* Stack to keep tokens that we have peeked at but not consumed yet.  *)
let tok_stack = ref []

(* A shortcut for raising an exception *)
let parse_err msg =
    raise @@ ParseFailure (sprintf "Error: %s" msg)

(* Puts the token back on the top of the stack *)
let unget_tok tok =
    tok_stack := tok::!tok_stack

let get_token lexbuf = match !tok_stack with
    | [] -> token lexbuf
    | h::t ->
        tok_stack := t;
        h

let peek_token lexbuf =
    let t = get_token lexbuf in
    unget_tok t;
    t

let expect_id lexbuf =
    let t = get_token lexbuf in
    match t with
        | ID x -> t
        | _ -> parse_err
            @@ sprintf "expected identifier, found `%s' instead"
                (token_to_str t)

let expect_tok lexbuf t_exp =
    let t = get_token lexbuf in
    if t <> t_exp then
        parse_err
            @@ sprintf "expected token `%s', found `%s' instead"
                (token_to_str t_exp)
                (token_to_str t);
    t

(* Parse non-empty comma separated list of elements that
   can be parsed by `parse_fun' function *)
let rec parse_generic_list ?(msg="expression expected") lexbuf parse_fun =
    let e = parse_fun lexbuf in
    if e = None then
        parse_err msg;
    let t = peek_token lexbuf in
    if t = COMMA then
        let _ = get_token lexbuf in
        let l = parse_generic_list ~msg:msg lexbuf parse_fun in
        e :: l
    else [e]

let rec parse_primary lexbuf =
    let t = get_token lexbuf in
    match t with
        | ID x ->  Some (EVar x)
        | INT x -> Some (EConst (float_of_int x))
        | FLOAT x -> Some (EConst x)

        | IF -> unget_tok t; parse_ifthen lexbuf
        | LET -> unget_tok t; parse_letin lexbuf

        | LSQUARE ->
            let lst = if (peek_token lexbuf) = RSQUARE then
                    []
                else
                    parse_generic_list lexbuf parse_expr
                        ~msg:"array element definition is missing"
            in let _ = expect_tok lexbuf RSQUARE in
            Some (EArray (List.map opt_get lst))

        | LPAREN ->
            let e = parse_expr lexbuf in
            if e = None then
                parse_err "empty expression found";
            let _ = expect_tok lexbuf RPAREN in
            e

        | _ -> unget_tok t; None


and parse_postfix lexbuf =
    let e = ref @@ parse_primary lexbuf in
    while !e <> None && (peek_token lexbuf) = DOT do
        let _ = get_token lexbuf in
        let e1 = parse_primary lexbuf in
        if e1 = None then
            parse_err "expected index specification in selection";
        e := Some (ESel ((opt_get !e), (opt_get e1)))
    done;
    !e

and parse_application ?(e1=None) lexbuf  =
    match e1, parse_unary lexbuf with
        | None, Some e2 -> parse_application lexbuf ~e1:(Some e2)
        | Some e1, Some e2 -> parse_application lexbuf ~e1:(Some (EApply (e1, e2)))
        | _, None -> e1

and parse_unary lexbuf =
    parse_postfix lexbuf

and parse_binary lexbuf =
    let rec resolve_stack s prec =
        let e1, op1, p1 = Stack.pop s in
        if prec <= p1 then (
            let e2, op2, p2 = Stack.pop s in
            let e = EBinary ((op_to_binop op1), (opt_get e2), (opt_get e1)) in
            Stack.push (Some e, op2, p2) s;
            resolve_stack s prec
        ) else (
            Stack.push (e1, op1, p1) s
        )
    in
    let e1 = parse_application lexbuf in
    if e1 = None then e1
    else
        let s = Stack.create () in
        (* First expression goes with no priority, priority = -1 *)
        Stack.push (e1, EOF, -1) s;
        while is_op (peek_token lexbuf) do
            let t = get_token lexbuf in

            (* resolve priority stack *)
            resolve_stack s (op_prec t);
            let e2 = parse_application lexbuf in
            if e2 = None then
                parse_err @@ sprintf "expression expected after %s" (token_to_str t);
            Stack.push (e2, t, (op_prec t)) s;
        done;
        resolve_stack s 0;
        let e, op, prec = Stack.pop s in
        e

and parse_expr lexbuf =
    parse_binary lexbuf

and parse_letin lexbuf =
    let t = get_token lexbuf in
    assert (LET = t);
    let t = expect_id lexbuf in
    let _ = expect_tok lexbuf EQ in
    let e1 = parse_expr lexbuf in
    if e1 = None then
        parse_err "expression expected after `='";
    let _ = expect_tok lexbuf IN in
    let e2 = parse_expr lexbuf in
    if e2 = None then
        parse_err "expression expected after `in'";
    Some (ELetIn ((token_to_str t), (opt_get e1), (opt_get e2)))

and parse_ifthen lexbuf =
    let t = get_token lexbuf in
    assert (IF = t);
    let e1 = parse_expr lexbuf in
    if e1 = None then
        parse_err "expression expected after `if'";
    let _ = expect_tok lexbuf THEN in
    let e2 = parse_expr lexbuf in
    if e2 = None then
        parse_err "expression expected after `then'";
    let _ = expect_tok lexbuf ELSE in
    let e3 = parse_expr lexbuf in
    if e3 = None then
        parse_err "expression expected after `else'";
    Some (EIfThen ((opt_get e1), (opt_get e2), (opt_get e3)))

let prog lexbuf =
    tok_stack := [];
    match parse_expr lexbuf with
        | Some e -> e
        | None -> parse_err "parser returned None"
 