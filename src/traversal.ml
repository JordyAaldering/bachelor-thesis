open Ast

(* This traversal reconstructs the tree as it goes along
    and accumulates additional information in `m' *)
let rec topdown m e = match e with
    | {expr_kind=ETrue }
    | {expr_kind=EFalse }
    | {expr_kind=ENum _ }
    | {expr_kind=EVar _ } ->
        (m, e)
    | {expr_kind=EArray exprlst; loc=l} ->
        let rec iter m lst =
            match lst with
            | [] -> (m, [])
            | e::es ->
                let m, es' = iter m es in
                let m, e' = topdown m e in
                (m, e'::es')
        in
        let m, lst = iter m exprlst in
        (m, mk_earray lst ~loc:l)

    | {expr_kind=EUnary (op, e1); loc=l} ->
        let m, e1' = topdown m e1 in
        (m, mk_eunary op e1' ~loc:l)
    | {expr_kind=EBinary (op, e1, e2)} ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_ebinary op e1' e2')

    | {expr_kind=EApply (e1, e2);} ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_eapply e1' e2')
    | {expr_kind=ELetIn (x, e1, e2); loc=l } ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_eletin x e1' e2' ~loc:l)
    | {expr_kind=EIfThen (e1, e2, e3); loc=l} ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        let m, e3' = topdown m e3 in
        (m, mk_eifthen e1' e2' e3' ~loc:l)

    | {expr_kind=ESel (e1, e2)} ->
        let m, e1' = topdown m e1 in
        let m, e2' = topdown m e2 in
        (m, mk_esel e1' e2')
    | {expr_kind=EShape e; loc=l} ->
        let m, e' = topdown m e in
        (m, mk_eshape e' ~loc:l)
    | {expr_kind=EDim e; loc=l} ->
        let m, e' = topdown m e in
        (m, mk_edim e' ~loc:l)
