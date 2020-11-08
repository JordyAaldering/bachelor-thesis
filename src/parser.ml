open Ast
open Lexer
open Printf

module Tok = struct
    type t = {
        loc: Loc.t;
        tok: Lexer.token;
    }

    let mk loc tok = {loc; tok}
    let get_loc tok = tok.loc
    let get_tok tok = tok.tok
    let eq tok lextok = tok.tok = lextok
    let to_str tok = Lexer.tok_to_str tok.tok
end

let opt_get = function
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
    | DIV
    | MOD -> 4
    | _ -> 5

(* Stack to keep tokens that we have peeked at but not consumed yet.  *)
let tok_stack = ref []

let parse_err_loc loc msg =
    raise (ImapFailure (sprintf "%s: error: %s" (Loc.to_str loc) msg))

let get_loc lexbuf =
    let open Lexing in
    let loc_s = lexeme_start_p lexbuf in
    Loc.mk loc_s.pos_fname loc_s.pos_lnum (loc_s.pos_cnum - loc_s.pos_bol + 1)

(* Puts the token back on the top of the stack *)
let unget_tok tok =
    (* printf "unget-tok `%s'\n" @@ tok_to_str tok; *)
    tok_stack := tok::!tok_stack

let get_token lexbuf = match !tok_stack with
    | [] ->
        let t = Lexer.token lexbuf in
        let l = get_loc lexbuf in
        (* print_sloc l; *)
        (* printf "get-token returns `%s'\n" @@ tok_to_str t; *)
        Tok.mk l t
    | h::t ->
        tok_stack := t;
        (*printf "get-token returns `%s'\n" @@ tok_to_str h;*)
        h

let peek_token lexbuf =
    let t = get_token lexbuf in
    unget_tok t;
    (*printf "peek-tok returns `%s'\n" @@ tok_to_str t;*)
    t

let peek_loc lexbuf =
    Tok.get_loc (peek_token lexbuf)

let expect_id lexbuf =
    let t = get_token lexbuf in
    match Tok.get_tok t with
    | ID (x) -> t
    | _ -> parse_err_loc (Tok.get_loc t)
        @@ sprintf "expected identifier, found `%s' instead"
            (Tok.to_str t)

let expect_tok lexbuf t_exp =
    let t = get_token lexbuf in
    if Tok.get_tok t <> t_exp then
        parse_err_loc (Tok.get_loc t)
        @@ sprintf "expected token `%s', found `%s' instead"
            (Lexer.tok_to_str t_exp)
            (Tok.to_str t);
    t

(* Parse non-empty comma separated list of elements that
   can be parsed by `parse_fun' function *)
let rec parse_generic_list ?(msg="expression expected") lexbuf parse_fun =
    let e = parse_fun lexbuf in
    if e = None then
        parse_err_loc (peek_loc lexbuf) msg;
    let t = peek_token lexbuf in
    if Tok.get_tok t = COMMA then
        let _ = get_token lexbuf in
        let l = parse_generic_list ~msg:msg lexbuf parse_fun in
        e::l
    else
        [e]

let rec parse_primary lexbuf =
    let t = get_token lexbuf in
    let l = Tok.get_loc t in
    match Tok.get_tok t with
    | TRUE ->  Some (mk_etrue () ~loc:l)
    | FALSE -> Some (mk_efalse () ~loc:l)
    | ID x ->  Some (mk_evar x ~loc:l)
    | INT n -> Some (mk_enum n ~loc:l)

    | IF -> unget_tok t; parse_ifthen lexbuf
    | LET -> unget_tok t; parse_letin lexbuf

    | LSQUARE ->
        let lst = if Tok.get_tok (peek_token lexbuf) = RSQUARE then
                []
            else
                parse_generic_list lexbuf parse_expr
                    ~msg:"array element definition is missing"
        in let _ = expect_tok lexbuf RSQUARE in
        Some (mk_earray (List.map opt_get lst) ~loc:l)

    | LPAREN ->
        let e = parse_expr lexbuf in
        if e = None then
            parse_err_loc (peek_loc lexbuf) "empty expression found";
        let _ = expect_tok lexbuf RPAREN in
        e

    | _ -> unget_tok t; None


and parse_postfix lexbuf =
    let e = ref @@ parse_primary lexbuf in
    while !e <> None && Tok.get_tok (peek_token lexbuf) = DOT do
        let _ = get_token lexbuf in
        let e1 = parse_primary lexbuf in
        if e1 = None then
            parse_err_loc (peek_loc lexbuf) "expected index specification in selection";
        e := Some (mk_esel (opt_get !e) (opt_get e1))
    done;
    !e

and parse_application ?(e1=None) lexbuf  =
    match e1, parse_unary lexbuf with
    | None, Some e2 -> parse_application lexbuf ~e1:(Some e2)
    | Some e1, Some e2 -> parse_application lexbuf ~e1:(Some (mk_eapply e1 e2))
    | _, None -> e1

and parse_unary lexbuf =
    parse_postfix lexbuf

and parse_binary lexbuf =
    let rec resolve_stack s prec =
        let e1, op1, p1 = Stack.pop s in
        if prec <= p1 then begin
            let e2, op2, p2 = Stack.pop s in
            let e = mk_ebinary (op_to_binop op1) (opt_get e2) (opt_get e1) in
            Stack.push (Some (e), op2, p2) s;
            resolve_stack s prec
        end else begin
            Stack.push (e1, op1, p1) s
        end
    in
    let e1 = parse_application lexbuf in
    if e1 = None then
        e1
    else
        let s = Stack.create () in
        (* First expression goes with no priority, priority = -1 *)
        Stack.push (e1, EOF, -1) s;
        while is_op (Tok.get_tok @@ peek_token lexbuf) do
            let t = get_token lexbuf in

            (* resolve priority stack *)
            resolve_stack s (op_prec @@ Tok.get_tok t);
            let e2 = parse_application lexbuf in
            if e2 = None then
                parse_err_loc (peek_loc lexbuf)  @@ sprintf "expression expected after %s" (Tok.to_str t);
            Stack.push (e2, Tok.get_tok t, (op_prec @@ Tok.get_tok t)) s;
        done;
        resolve_stack s 0;
        let e, op, prec = Stack.pop s in
        e


and parse_expr lexbuf =
    parse_binary lexbuf

and parse_letin lexbuf =
    let t = get_token lexbuf in
    let l = Tok.get_loc t in
    assert (LET = Tok.get_tok t);
    let t = expect_id lexbuf in
    let _ = expect_tok lexbuf EQ in
    let e1 = parse_expr lexbuf in
    if e1 = None then
        parse_err_loc (peek_loc lexbuf) "expression expected after `='";
    let _ = expect_tok lexbuf IN in
    let e2 = parse_expr lexbuf in
    if e2 = None then
        parse_err_loc (peek_loc lexbuf) "expression expected after `in'";
    Some (mk_eletin (Tok.to_str t) (opt_get e1) (opt_get e2) ~loc:l)

and parse_ifthen lexbuf =
    let t = get_token lexbuf in
    let l = Tok.get_loc t in
    assert (IF = Tok.get_tok t);
    let e1 = parse_expr lexbuf in
    if e1 = None then
        parse_err_loc (peek_loc lexbuf) "expression expected after `if'";
    let _ = expect_tok lexbuf THEN in
    let e2 = parse_expr lexbuf in
    if e2 = None then
        parse_err_loc (peek_loc lexbuf) "expression expected after `then'";
    let _ = expect_tok lexbuf ELSE in
    let e3 = parse_expr lexbuf in
    if e3 = None then
        parse_err_loc (peek_loc lexbuf) "expression expected after `else'";
    Some (mk_eifthen (opt_get e1) (opt_get e2) (opt_get e3) ~loc:l)

let prog lexbuf =
    tok_stack := [];
    match parse_expr lexbuf with
    | Some (e) -> e
    | None -> raise (ImapFailure (sprintf "parser returned None") )
 