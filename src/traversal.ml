open Ast

(* This traversal reconstructs the tree as it goes along
    and accumulates additional information in `m' *)
let rec topdown m e = match e with
    | { kind=EVar _ }
    | { kind=EConst _ } ->
        (m, e)
    | { kind=EArray xs; loc=l } ->
        let rec iter m lst = match lst with
            | [] -> (m, [])
            | e::es ->
                let m, es' = iter m es in
                let m, e' = topdown m e in
                (m, e'::es')
        in
        let m, lst = iter m xs in
        (m, mk_expr_array lst ~loc:l)

    | { kind=EApply (e1, e2) } ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_expr_apply e1' e2')
    | { kind=ELetIn (x, e1, e2); loc=l } ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_expr_letin x e1' e2' ~loc:l)
    | { kind=EIfThen (e1, e2, e3); loc=l } ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        let m, e3' = topdown m e3 in
        (m, mk_expr_ifthen e1' e2' e3' ~loc:l)

    | { kind=EBinary (op, e1, e2) } ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_expr_binary op e1' e2')
    | { kind=EUnary (op, e1); loc=l } ->
        let m, e1' = topdown m e1 in
        (m, mk_expr_unary op e1' ~loc:l)

    | { kind=ESel (e1, e2) } ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_expr_sel e1' e2')
    | { kind=EShape e; loc=l } ->
        let m, e' = topdown m e in
        (m, mk_expr_shape e' ~loc:l)
    | { kind=EDim e; loc=l } ->
        let m, e' = topdown m e in
        (m, mk_expr_dim e' ~loc:l)
