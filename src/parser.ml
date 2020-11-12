open Ast
open Lexer
open Printf

let opt_get x = match x with
    | Some x -> x
    | None -> raise @@ Invalid_argument "opt_get"

let op_prec token = match token with
    | EQ
    | NE -> 1
    | LT
    | LE
    | GT
    | GE -> 2
    | PLUS
    | MIN -> 3
    | MULT
    | DIV -> 4
    | _ -> 5

let parse_err msg = raise @@ ParseFailure (sprintf "Error: %s" msg)

(* Stack to keep tokens that we have peeked at but not consumed yet *)
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
    let t = peek_token lexbuf in
    if t = expected then
        let _step = get_token lexbuf in
        true
    else false

let expect_id lexbuf =
    let t = get_token lexbuf in
    match t with
        | ID _x -> t
        | _ -> parse_err @@ sprintf "expected identifier, found `%s' instead"
                (token_to_str t)

let expect_token lexbuf expected =
    let t = get_token lexbuf in
    if t <> expected then
        parse_err @@ sprintf "expected token `%s', found `%s' instead"
            (token_to_str expected) (token_to_str t);
    t

let rec parse_primary lexbuf =
    let t = get_token lexbuf in
    match t with
        | ID x -> Some (EVar x)
        | INT x -> Some (EConst (float_of_int x))
        | FLOAT x -> Some (EConst x)
        | LAMBDA -> parse_lambda lexbuf
        | LET -> parse_letin lexbuf
        | IF -> parse_ifthen lexbuf
        | LSQUARE ->
            let lst = if peek_token lexbuf <> RSQUARE then
                    parse_array lexbuf parse_expr
                else []
            in
            let _square = expect_token lexbuf RSQUARE in
            Some (EArray (List.map opt_get lst))
        | LPAREN ->
            let e = parse_expr lexbuf in
            if e = None then
                parse_err "empty expression found";
            let _paren = expect_token lexbuf RPAREN in
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

and parse_expr lexbuf =
    parse_binary lexbuf

and parse_application ?(e1=None) lexbuf =
    match e1, parse_unary lexbuf with
        | None, Some e2 -> parse_application lexbuf ~e1:(Some e2)
        | Some e1, Some e2 -> parse_application lexbuf ~e1:(Some (EApply (e1, e2)))
        | _, None -> e1

and parse_lambda lexbuf =
    let t = expect_id lexbuf in
    let _dot = expect_token lexbuf DOT in
    let e = parse_expr lexbuf in
    if e = None then
        parse_err "expected expression after `.'";
    Some (ELambda (token_to_str t, opt_get e))

and parse_letin lexbuf =
    let id = expect_id lexbuf in
    let _eq = expect_token lexbuf EQ in
    let e1 = parse_expr lexbuf in
    if e1 = None then
        parse_err "expected expression after `='";

    let _in = expect_token lexbuf IN in
    let e2 = parse_expr lexbuf in
    if e2 = None then
        parse_err "expected expression after `in'";
    Some (ELetIn (token_to_str id, opt_get e1, opt_get e2))

and parse_ifthen lexbuf =
    let e1 = parse_expr lexbuf in
    if e1 = None then
        parse_err "expected expression after `if'";

    let _then = expect_token lexbuf THEN in
    let e2 = parse_expr lexbuf in
    if e2 = None then
        parse_err "expected expression after `then'";
    
    let _else = expect_token lexbuf ELSE in
    let e3 = parse_expr lexbuf in
    if e3 = None then
        parse_err "expected expression after `else'";
    Some (EIfThen (opt_get e1, opt_get e2, opt_get e3))

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
        (* first expression goes with no priority; -1 *)
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

let prog lexbuf =
    token_stack := [];
    match parse_expr lexbuf with
        | Some e -> e
        | None -> parse_err "parser returned None"
 