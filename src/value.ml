open Ast
open Env
open Printf

exception ValueError of string

let value_err msg =
    raise @@ ValueError msg

type value =
    | VArray of int list * float list
    | VClosure of string * expr * ptr_env

let shp_to_str shp =
    String.concat ", " (List.map string_of_int shp)

let data_to_str data =
    String.concat ", " (List.map (sprintf "%g") data)

let value_to_str v = match v with
    | VArray ([], [x]) -> sprintf "%g" x
    | VArray (shp, data) -> sprintf "<[%s], [%s]>"
        (shp_to_str shp) (data_to_str data)
    | VClosure (s, e, env) -> sprintf "{\\%s.%s, %s}"
        s (expr_to_str e) (ptr_env_to_str env)

let extract_value v = match v with
    | VArray (shp, data) -> (shp, data)
    | _ -> value_err "invalid extract argument"

let extract_closure v = match v with
    | VClosure (s, e, env) -> (s, e, env)
    | _ -> value_err "invalid extract argument"


(** Primitive Functions **)

let sel v iv = match v, iv with
    | VArray (_, data), VArray ([], [i]) ->
        VArray ([], [List.nth data (int_of_float i)])
    | VArray (shp, data), VArray (_, idx) ->
        let rec row_major sprod res shp iv = match shp, iv with
            | [], [] -> res
            | (sh :: shtl), (i :: ivtl) ->
                row_major (sprod * sh) (res + sprod * i) shtl ivtl
            | _ -> value_err @@ sprintf "sel got different shapes v:[%s] and iv:[%s]"
                    (shp_to_str shp) (shp_to_str (List.map int_of_float idx))
        in
        let shp_vec = List.rev shp in
        let iv_vec = List.rev @@ List.map int_of_float idx in
        let i = row_major 1 0 shp_vec iv_vec in
        VArray ([], [List.nth data i])
    | _ -> value_err @@ sprintf "invalid sel arguments %s and %s"
            (value_to_str iv) (value_to_str v)

let shape v = match v with
    | VArray (shp, _) -> VArray ([List.length shp], List.map float_of_int shp)
    | _ -> value_err @@ sprintf "invalid shape argument %s" (value_to_str v)

let dim v = match v with
    | VArray (shp, _) -> VArray ([], [float_of_int @@ List.length shp])
    | _ -> value_err @@ sprintf "invalid dim argument %s" (value_to_str v)


(** Predicates **)

let value_append v1 v2 = match v1, v2 with
    |  VArray ([], x), VArray ([], y) ->
        VArray ([2], x @ y)
    | VArray ([shp], xs), VArray ([], ys)
    | VArray ([], xs), VArray ([shp], ys) ->
        VArray ([shp + 1], xs @ ys)
    | VArray (shp1, xs), VArray (shp2, ys) ->
        VArray (shp1 @ shp2, xs @ ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_neg v = match v with
    | VArray (shp, data) -> VArray (shp, List.map (fun x -> -. x) data)
    | _ -> value_err @@ sprintf "invalid argument %s" (value_to_str v)

let value_add v1 v2 = match v1, v2 with
    | VArray ([], [c]), VArray (shp, xs)
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map ((+.) c) xs)
    | VArray (shp1, xs), VArray (_shp2, ys) ->
        VArray (shp1, List.map2 (+.) xs ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_mul v1 v2 = match v1, v2 with
    | VArray ([], [c]), VArray (shp, xs)
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map (( *.) c) xs)
    | VArray (shp1, xs), VArray (_shp2, ys) ->
        VArray (shp1, List.map2 ( *.) xs ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_div v1 v2 = match v1, v2 with
    | VArray ([], [c]), VArray (shp, xs)
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map ((/.) c) xs)
    | VArray (shp1, xs), VArray (_shp2, ys) ->
        VArray (shp1, List.map2 (/.) xs ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)


let value_is_truthy v = match v with
    | VArray (_, data) -> List.exists ((<>) 0.) data
    | _ -> false

let value_not v =
    VArray ([], [if value_is_truthy v then 0. else 1.])

let value_eq v1 v2 = match v1, v2 with
    | VArray (shp1, xs), VArray (shp2, ys) ->
        if List.length shp1 = List.length shp2
            && List.for_all2 (=) shp1 shp2
            && List.for_all2 (=) xs ys
        then VArray ([], [1.])
        else VArray ([], [0.])
    | _ -> VArray ([], [0.])

(** True (1) if ALL values in v1 are greater than the corresponding values in v2 *)
let value_gt v1 v2 = match v1, v2 with
    | VArray ([], [c]), VArray (_shp, xs) ->
        VArray ([], [if List.for_all ((>) c) xs then 1. else 0.])
    | VArray (_shp, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((<) c) xs then 1. else 0.])
    | VArray (shp1, xs), VArray (shp2, ys) ->
        if List.length shp1 <> List.length shp2
            || not @@ List.for_all2 (=) shp1 shp2
        then value_err @@ sprintf "gt got different shapes [%s] and [%s]"
                (shp_to_str shp1) (shp_to_str shp2);
        VArray ([], [if List.for_all2 (>) xs ys then 1. else 0.])
    | _ -> value_err @@ sprintf "invalid gt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

(** True (1) if ALL values in v1 are less than the corresponding values in v2 *)
let value_lt v1 v2 = match v1, v2 with
    | VArray ([], [c]), VArray (_shp, xs) ->
        VArray ([], [if List.for_all ((<) c) xs then 1. else 0.])
    | VArray (_shp, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((>) c) xs then 1. else 0.])
    | VArray (shp1, xs), VArray (shp2, ys) ->
        if List.length shp1 <> List.length shp2
            || not @@ List.for_all2 (=) shp1 shp2
        then value_err @@ sprintf "lt got different shapes [%s] and [%s]"
                (shp_to_str shp1) (shp_to_str shp2);
        VArray ([], [if List.for_all2 (<) xs ys then 1. else 0.])
    | _ -> value_err @@ sprintf "invalid lt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)
