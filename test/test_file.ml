open OUnit2
open Src.Ast
open Src.Value
open Src.Parser
open Src.Rewrite
open Src.Eval

let lower_ast_result (e: expr) (dem: int) : expr =
    let rec lower (e: expr) (change_res: expr -> expr) : expr =
        match e with
        | EVar _
        | EFloat _
        | EArray _ -> change_res e
        | ELet (s, e1, e2) -> ELet (s, e1, lower e2 change_res)
        | _ -> e
    in
    match dem with
    | 2 -> lower e (fun e -> EShape e)
    | 1 -> lower e (fun e -> EDim e)
    | _ -> e

let lower_value (v: value) (dem: int) : value =
    match dem with
    | 2 -> value_shape v
    | 1 -> value_dim v
    | _ -> v

let test_file (e: expr) (expected: value) (dem: int) =
    let e = lower_ast_result e dem in
    let res = eval @@ rewrite e in
    assert_equal res (lower_value expected dem)

let create_tests (fname: string) (expected: value) =
    let e = parse ("../../../examples/" ^ fname ^ ".txt") in
    (fname ^ " tests") >::: [
        (fname ^ "matmul full") >:: (fun _ -> test_file e expected 3);
        (fname ^ "matmul shape") >:: (fun _ -> test_file e expected 2);
        (fname ^ "matmul dim") >:: (fun _ -> test_file e expected 1);
    ]
