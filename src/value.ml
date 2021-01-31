open Ast
open Env
open Printf

exception ValueError of string

let value_err msg =
    raise @@ ValueError msg

type value =
    | VArray of int list * float list
    | VClosure of string * expr * ptr_env

let shp_to_str (shp: int list) : string =
    String.concat ", " (List.map string_of_int shp)

let data_to_str (data: float list) : string =
    String.concat ", " (List.map (sprintf "%g") data)

let value_to_str (v: value) : string =
    match v with
    | VArray (shp, data) -> sprintf "<[%s], [%s]>"
        (shp_to_str shp) (data_to_str data)
    | VClosure (s, e, env) -> sprintf "{\\%s.%s, %s}"
        s (expr_to_str e) (ptr_env_to_str env)

let extract_value (v: value) : (int list * float list) =
    match v with
    | VArray (shp, data) -> (shp, data)
    | _ -> value_err @@ sprintf "invalid value extract argument '%s`"
        (value_to_str v)

let extract_closure (v: value) : (string * expr * ptr_env) =
    match v with
    | VClosure (s, e, env) -> (s, e, env)
    | _ -> value_err @@ sprintf "invalid closure extract argument '%s`"
        (value_to_str v)


(** Assertions **)

let assert_shape_eq (token: string) (v1: value) (v2: value) =
    match v1, v2 with
    | VArray (shp1, _), VArray (shp2, _) ->
        if List.length shp1 <> List.length shp2 || List.exists2 (<>) shp1 shp2 then
            value_err @@ sprintf "`%s' expected two arrays of equal shape, got shapes [%s] and [%s] (from arrays %s and %s)"
                token (shp_to_str shp1) (shp_to_str shp2) (value_to_str v1) (value_to_str v2);
    | _ -> ()

let assert_dim_eq (token: string) (v1: value) (v2: value) =
    match v1, v2 with
    | VArray (shp1, _), VArray (shp2, _) ->
        if List.length shp1 <> List.length shp2 then
            value_err @@ sprintf "`%s' expected two arrays of equal dim, got dim %d and %d (from arrays %s and %s)"
                token (List.length shp1) (List.length shp2) (value_to_str v1) (value_to_str v2);
    | _ -> ()


(** Primitive Functions **)

let sel (v: value) (iv: value) =
    match v, iv with
    | VArray (_, data), VArray ([], [i]) ->
        VArray ([], [List.nth data (int_of_float i)])
    | VArray (shp, data), VArray (_, idx) ->
        let rec row_major sprod res shp iv =
            match shp, iv with
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

let shape (v: value) : value =
    match v with
    | VArray (shp, _) -> VArray ([List.length shp], List.map float_of_int shp)
    | _ -> value_err @@ sprintf "invalid shape argument %s" (value_to_str v)

let dim (v: value) : value =
    match v with
    | VArray (shp, _) -> VArray ([], [float_of_int @@ List.length shp])
    | _ -> value_err @@ sprintf "invalid dim argument %s" (value_to_str v)


(** Predicates **)

let value_concat (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (shp1, xs), VArray (shp2, ys) ->
        assert_dim_eq "@" v1 v2;
        VArray (List.map2 (+) shp1 shp2, xs @ ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_neg (v: value) : value =
    match v with
    | VArray (shp, data) -> VArray (shp, List.map (fun x -> -. x) data)
    | _ -> value_err @@ sprintf "invalid argument %s" (value_to_str v)

let value_add (v1: value) (v2: value) : value =
    match v1, v2 with
    (* adding an empty array *)
    | v, VArray ([0], [])
    | VArray ([0], []), v -> v
    (* adding a constant *)
    | VArray ([], [c]), VArray (shp, xs)
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map ((+.) c) xs)
    (* adding two arrays *)
    | VArray (shp, xs), VArray (_, ys) ->
        assert_shape_eq "+" v1 v2;
        VArray (shp, List.map2 (+.) xs ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_sub (v1: value) (v2: value) : value =
    value_add v1 (value_neg v2)

let value_mul (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray ([], [c]), VArray (shp, xs)
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map (( *.) c) xs)
    | VArray (shp1, xs), VArray (_shp2, ys) ->
        assert_shape_eq "*" v1 v2;
        VArray (shp1, List.map2 ( *.) xs ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_div (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray ([], [c]), VArray (shp, xs)
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map ((/.) c) xs)
    | VArray (shp1, xs), VArray (_shp2, ys) ->
        assert_shape_eq "/" v1 v2;
        VArray (shp1, List.map2 (/.) xs ys)
    | _ -> value_err @@ sprintf "invalid arguments %s and %s"
            (value_to_str v1) (value_to_str v2)


(** a value is false if ALL its floats are 0, else it is true *)
let value_is_truthy (v: value) : bool =
    match v with
    | VArray (_, data) -> List.exists ((<>) 0.) data
    | _ -> false

let value_not (v: value) : value =
    VArray ([], [if value_is_truthy v then 0. else 1.])

(** True (1) if ALL values in v1 are equal to the corresponding values in v2 *)
let value_eq (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq "=" v1 v2;
        VArray ([], [if List.for_all2 (=) xs ys then 1. else 0.])
    | _ -> VArray ([], [0.])

(** True (1) if ALL values in v1 are greater than the corresponding values in v2 *)
let value_gt (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray ([], [c]), VArray (_, xs) ->
        VArray ([], [if List.for_all ((>) c) xs then 1. else 0.])
    | VArray (_, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((<) c) xs then 1. else 0.])
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq ">" v1 v2;
        VArray ([], [if List.for_all2 (>) xs ys then 1. else 0.])
    | _ -> value_err @@ sprintf "invalid gt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

(** True (1) if ALL values in v1 are less than the corresponding values in v2 *)
let value_lt (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray ([], [c]), VArray (_, xs) ->
        VArray ([], [if List.for_all ((<) c) xs then 1. else 0.])
    | VArray (_, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((>) c) xs then 1. else 0.])
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq "<" v1 v2;
        VArray ([], [if List.for_all2 (<) xs ys then 1. else 0.])
    | _ -> value_err @@ sprintf "invalid lt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)
