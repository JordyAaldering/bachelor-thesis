open Ast
open Env
open Printf

exception ValueError of string

type value =
    | VArray of int list * float list
    | VClosure of string * expr * ptr_env

let shp_to_str (shp: int list) : string =
    String.concat ", " (List.map string_of_int shp)

let data_to_str (data: float list) : string =
    String.concat ", " (List.map (sprintf "%g") data)

let value_to_str (v: value) : string =
    match v with
    | VArray (shp, data) ->
        sprintf "<[%s], [%s]>"
            (shp_to_str shp) (data_to_str data)
    | VClosure (s, e, env) ->
        sprintf "{\\%s.%s, %s}"
            s (expr_to_str e) (ptr_env_to_str env)


(** 
 * Exceptions
 *)

let value_err (msg: string) =
    raise @@ ValueError msg

let invalid_argument ?(msg: string = "") (v: value) =
    value_err @@ sprintf "invalid argument %s%s%s"
        (value_to_str v) (if msg = "" then "" else ", ") msg

let invalid_arguments ?(msg: string = "") (vs: value list) =
    value_err @@ sprintf "invalid arguments %s%s%s"
        (String.concat ", " (List.map value_to_str vs))
        (if msg = "" then "" else ", ") msg

let assert_scalar (v1: value) =
    match v1 with
    | VArray ([], _) -> ()
    | _ -> invalid_argument v1 ~msg:"expected a scalar"

let assert_shape_eq (v1: value) (v2: value) =
    match v1, v2 with
    | VArray (shp1, _), VArray (shp2, _) ->
        if List.length shp1 <> List.length shp2 || List.exists2 (<>) shp1 shp2 then
            value_err @@ sprintf "expected two arrays of equal value_shape, got shapes [%s] and [%s] (from arrays %s and %s)"
                (shp_to_str shp1) (shp_to_str shp2) (value_to_str v1) (value_to_str v2);
    | _ -> ()

let assert_dim_eq (v1: value) (v2: value) =
    match v1, v2 with
    | VArray (shp1, _), VArray (shp2, _) ->
        if List.length shp1 <> List.length shp2 then
            value_err @@ sprintf "expected two arrays of equal value_dim, got value_dim %d and %d (from arrays %s and %s)"
                (List.length shp1) (List.length shp2) (value_to_str v1) (value_to_str v2);
    | _ -> ()


(** 
 * Primitive functions
 *)

let rec row_major (iv: int list) (shp: int list) (sprod: int) (res: int) : int =
    match iv, shp with
    | [], [] -> res
    | (i :: ivtl), (sh :: shtl) ->
        row_major shtl ivtl (sprod * sh) (res + sprod * i)
    | _ ->
        value_err @@ sprintf "expected two arrays of equal value_shape, got shapes [%s] and [%s]"
            (shp_to_str iv) (shp_to_str shp)

let iv_to_index (iv: value) (shp: int list) : int =
    match iv with
    | VArray (_, iv) ->
        let iv = List.rev @@ List.map int_of_float iv in
        let shp = List.rev shp in
        row_major iv shp 1 0
    | _ ->
        invalid_argument iv

let value_set (v: value) (iv: value) (scalar: value) : value =
    match v, iv, scalar with
    | VArray (shp, data), _, VArray ([], [x]) ->
        let i = iv_to_index iv shp in
        let data = List.mapi (fun j y -> if i = j then x else y) data in
        VArray (shp, data)
    | _ ->
        invalid_arguments [v; iv; scalar]

let value_sel (v: value) (iv: value) : value =
    match v, iv with
    | VArray (_, data), VArray ([], [i]) ->
        VArray ([], [List.nth data (int_of_float i)])
    | VArray (shp, data), _ ->
        let i = iv_to_index iv shp in
        VArray ([], [List.nth data i])
    | _ ->
        invalid_arguments [iv; v]

let value_shape (v: value) : value =
    match v with
    | VArray (shp, _) ->
        let shp' = [List.length shp] in
        let data' = List.map float_of_int shp in
        VArray (shp', data')
    | _ ->
        invalid_argument v

let value_dim (v: value) : value =
    match v with
    | VArray (shp, _) ->
        VArray ([], [float_of_int @@ List.length shp])
    | _ ->
        invalid_argument v


(** 
 * Math operations
 *)

let value_concat (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (shp1, xs), VArray (shp2, ys) ->
        assert_dim_eq v1 v2;
        VArray (List.map2 (+) shp1 shp2, xs @ ys)
    | _ ->
        invalid_arguments [v1; v2]

let value_neg (v: value) : value =
    match v with
    | VArray (shp, data) -> VArray (shp, List.map (fun x -> -. x) data)
    | _ -> invalid_argument v

let value_add (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map ((+.) c) xs)
    | VArray (shp, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray (shp, List.map2 (+.) xs ys)
    | _ ->
        invalid_arguments [v1; v2]

let value_sub (v1: value) (v2: value) : value =
    value_add v1 (value_neg v2)

let value_mul (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map (( *.) c) xs)
    | VArray (shp, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray (shp, List.map2 ( *.) xs ys)
    | _ ->
        invalid_arguments [v1; v2]

let value_div (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (shp, xs), VArray ([], [c]) ->
        VArray (shp, List.map ((/.) c) xs)
    | VArray (shp, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray (shp, List.map2 (/.) xs ys)
    | _ ->
        invalid_arguments [v1; v2]


(** 
 * Equality operations
 *)

(** A value is false if ALL its floats are 0, else it is true *)
let value_is_truthy (v: value) : bool =
    match v with
    | VArray (_, xs) -> List.exists ((<>) 0.) xs
    | _ -> false

let value_not (v: value) : value =
    VArray ([], [if value_is_truthy v then 0. else 1.])

(** True (1) if ALL values in v1 are = the corresponding values in v2 *)
let value_eq (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray ([], [if List.for_all2 (=) xs ys then 1. else 0.])
    | _ ->
        VArray ([], [0.])

(** True (1) if ALL values in v1 are != the corresponding values in v2 *)
let value_ne (v1: value) (v2: value) : value =
    value_not @@ value_eq v1 v2

(** True (1) if ALL values in v1 are > the corresponding values in v2 *)
let value_gt (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (_, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((<) c) xs then 1. else 0.])
    | VArray ([], [c]), VArray (_, ys) ->
        VArray ([], [if List.for_all ((>) c) ys then 1. else 0.])
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray ([], [if List.for_all2 (>) xs ys then 1. else 0.])
    | _ ->
        invalid_arguments [v1; v2]

(** True (1) if ALL values in v1 are >= the corresponding values in v2 *)
let value_ge (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (_, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((<=) c) xs then 1. else 0.])
    | VArray ([], [c]), VArray (_, ys) ->
        VArray ([], [if List.for_all ((>=) c) ys then 1. else 0.])
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray ([], [if List.for_all2 (>=) xs ys then 1. else 0.])
    | _ ->
        invalid_arguments [v1; v2]

(** True (1) if ALL values in v1 are < the corresponding values in v2 *)
let value_lt (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (_, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((>) c) xs then 1. else 0.])
    | VArray ([], [c]), VArray (_, ys) ->
        VArray ([], [if List.for_all ((<) c) ys then 1. else 0.])
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray ([], [if List.for_all2 (<) xs ys then 1. else 0.])
    | _ ->
        invalid_arguments [v1; v2]

(** True (1) if ALL values in v1 are <= the corresponding values in v2 *)
let value_le (v1: value) (v2: value) : value =
    match v1, v2 with
    | VArray (_, xs), VArray ([], [c]) ->
        VArray ([], [if List.for_all ((>=) c) xs then 1. else 0.])
    | VArray ([], [c]), VArray (_, ys) ->
        VArray ([], [if List.for_all ((<=) c) ys then 1. else 0.])
    | VArray (_, xs), VArray (_, ys) ->
        assert_shape_eq v1 v2;
        VArray ([], [if List.for_all2 (<=) xs ys then 1. else 0.])
    | _ ->
        invalid_arguments [v1; v2]


(** 
 * Helpers
 *)

let extract_value (v: value) : (int list * float list) =
    match v with
    | VArray (shp, data) -> (shp, data)
    | _ -> invalid_argument v

let extract_closure (v: value) : (string * expr * ptr_env) =
    match v with
    | VClosure (s, e, env) -> (s, e, env)
    | _ -> invalid_argument v
