open Ast
open Token
open Lexer
open Printf

let opt_get x = match x with
    | Some x -> x
    | None -> parse_err "expression was None"

(* stack to keep tokens that we have peeked but not yet consumed *)
let token_stack = ref []

let get_token lexbuf = match !token_stack with
    | [] -> token lexbuf
    | h :: t ->
        token_stack := t;
        h

let unget_token token =
    token_stack := token :: !token_stack

let peek_token lexbuf =
    let t = get_token lexbuf in
    unget_token t;
    t

let match_token lexbuf expected =
    if peek_token lexbuf = expected then
        let _ = get_token lexbuf in
        true
    else false

let expect_id lexbuf =
    let t = get_token lexbuf in
    match t with
        | ID s -> s
        | _ -> parse_err @@ sprintf "expected identifier, found `%s'"
                (token_to_str t)

let expect_token lexbuf expected =
    let t = get_token lexbuf in
    if t <> expected then
        parse_err @@ sprintf "expected token `%s', found `%s'"
            (token_to_str expected) (token_to_str t)

let rec expect_expr lexbuf =
    let e = parse_expr lexbuf in
    if e = None then
        parse_err "expected expression";
    opt_get e

and parse_expr lexbuf =
    parse_binary lexbuf

and parse_primary lexbuf =
    let t = get_token lexbuf in
    match t with
        (* variables *)
        | ID s -> Some (EVar s)
        | INT x -> Some (EFloat (float_of_int x))
        | FLOAT x -> Some (EFloat x)
        (* expressions *)
        | LAMBDA -> parse_lambda lexbuf
        | LET -> parse_let lexbuf
        | IF -> parse_cond lexbuf
        (* primitive functions *)
        | SHAPE -> Some (EShape (expect_expr lexbuf))
        | DIM -> Some (EDim (expect_expr lexbuf))
        | READ -> Some ERead
        (* symbols *)
        | LSQUARE ->
            let lst = if peek_token lexbuf = RSQUARE
                then [] else parse_array lexbuf parse_expr
            in
            expect_token lexbuf RSQUARE;
            Some (EArray (List.map opt_get lst))
        | LPAREN ->
            let e = parse_expr lexbuf in
            expect_token lexbuf RPAREN;
            e
        | _ -> unget_token t; None

(* Parse non-empty comma separated list of elements
    that can be parsed by `parse_fun' *)
and parse_array lexbuf parse_fun =
    let e = parse_fun lexbuf in
    if e = None then
        parse_err "expected array element definition";
    if match_token lexbuf COMMA then
        let l = parse_array lexbuf parse_fun in
        e :: l
    else [e]

and parse_postfix lexbuf =
    let e = ref @@ parse_primary lexbuf in
    while !e <> None && match_token lexbuf DOT do
        let e1 = parse_primary lexbuf in
        if e1 = None then
            parse_err "expected index specification in selection";
        e := Some (ESel (opt_get !e, opt_get e1))
    done;
    !e

and parse_application ?(e1=None) lexbuf =
    match e1, parse_unary lexbuf with
        | None, Some e2 -> parse_application lexbuf ~e1:(Some e2)
        | Some e1, Some e2 -> parse_application lexbuf ~e1:(Some (EApply (e1, e2)))
        | _ -> e1

and parse_lambda lexbuf =
    let s = expect_id lexbuf in
    expect_token lexbuf DOT;
    let e1 = expect_expr lexbuf in
    Some (ELambda (s, e1))

and parse_let lexbuf =
    let s = expect_id lexbuf in
    expect_token lexbuf EQ;
    let e1 = expect_expr lexbuf in
    expect_token lexbuf IN;
    let e2 = expect_expr lexbuf in
    Some (ELet (s, e1, e2))

and parse_cond lexbuf =
    let e1 = expect_expr lexbuf in
    expect_token lexbuf THEN;
    let e2 = expect_expr lexbuf in
    expect_token lexbuf ELSE;
    let e3 = expect_expr lexbuf in
    Some (ECond (e1, e2, e3))

and parse_binary lexbuf =
    let rec resolve_stack s prec =
        let e1, op1, p1 = Stack.pop s in
        if prec <= p1 then (
            let e2, op2, p2 = Stack.pop s in
            let e = EBinary (op_to_binop op1, opt_get e2, opt_get e1) in
            Stack.push (Some e, op2, p2) s;
            resolve_stack s prec
        ) else Stack.push (e1, op1, p1) s
    in
    let e1 = parse_application lexbuf in
    if e1 = None then e1
    else
        let s = Stack.create () in
        (* first expression has no priority; -1 *)
        Stack.push (e1, EOF, -1) s;
        while is_op @@ peek_token lexbuf do
            let t = get_token lexbuf in
                resolve_stack s (op_prec t);
                let e2 = parse_application lexbuf in
                if e2 = None then
                    parse_err @@ sprintf "expected expression after %s" (token_to_str t);
                Stack.push (e2, t, op_prec t) s;
        done;
        resolve_stack s 0;
        let e, _op, _prec = Stack.pop s in
        e

and parse_unary lexbuf =
    parse_postfix lexbuf

let parse_prog lexbuf =
    token_stack := [];
    match parse_expr lexbuf with
        | Some e ->
            printf "%s\n" (expr_to_str ~newline:true e);
            e
        | None -> parse_err "parser returned None"
 