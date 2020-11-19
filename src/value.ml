open Printf

exception ValueFailure of string

let value_err msg =
    raise @@ ValueFailure msg

type value =
    | Vect of int list * float list
    | Closure of Ast.expr * Env.ptr_env

let shp_to_str shp =
    String.concat ", " (List.map string_of_int shp)

let data_to_str data =
    String.concat ", " (List.map string_of_float data)

let value_to_str v = match v with
    | Vect (shp, data) -> sprintf "<[%s], [%s]>"
        (shp_to_str shp) (data_to_str data)
    | Closure (e, env) -> sprintf "{%s, %s}"
        (Ast.expr_to_str e) (Env.ptr_env_to_str env)


(** Primitive Functions **)

let value_sel v iv = match v, iv with
    | Vect (shp, xs), Vect ([], [i]) ->
        Vect ([], [List.nth xs (int_of_float i)])
    | Vect (shp, data), Vect (_, idx) ->
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
        printf "%d\n" i;
        Vect ([], [List.nth data i])
    | _ -> value_err @@ sprintf "invalid sel arguments %s and %s"
            (value_to_str iv) (value_to_str v)

let value_shape v = match v with
    | Vect (shp, _) -> Vect ([List.length shp], List.map float_of_int shp)
    | _ -> value_err @@ sprintf "invalid shape argument %s" (value_to_str v)

let value_dim v = match v with
    | Vect (shp, _) -> Vect ([], [float_of_int @@ List.length shp])
    | _ -> value_err @@ sprintf "invalid dim argument %s" (value_to_str v)


(** Predicates **)

let value_neg v = match v with
    | Vect (shp, data) -> Vect (shp, List.map (fun x -> -. x) data)
    | _ -> value_err @@ sprintf "invalid neg argument %s" (value_to_str v)

let value_add v1 v2 = match v1, v2 with
    | Vect ([], [c]), Vect (shp, xs)
    | Vect (shp, xs), Vect ([], [c]) ->
        Vect (shp, List.map ((+.) c) xs)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        Vect (shp1, List.map2 (+.) xs ys)
    | _ -> value_err @@ sprintf "invalid add arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_mul v1 v2 = match v1, v2 with
    | Vect ([], [c]), Vect (shp, xs)
    | Vect (shp, xs), Vect ([], [c]) ->
        Vect (shp, List.map (( *.) c) xs)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        Vect (shp1, List.map2 ( *.) xs ys)
    | _ -> value_err @@ sprintf "invalid mul arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_div v1 v2 = match v1, v2 with
    | Vect ([], [c]), Vect (shp, xs)
    | Vect (shp, xs), Vect ([], [c]) ->
        Vect (shp, List.map ((/.) c) xs)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        Vect (shp1, List.map2 (/.) xs ys)
    | _ -> value_err @@ sprintf "invalid div arguments %s and %s"
            (value_to_str v1) (value_to_str v2)


let value_is_truthy v = match v with
    | Vect (_, data) -> List.for_all ((<>) 0.) data
    | _ -> false

let value_not v =
    Vect ([], [if value_is_truthy v then 0. else 1.])

let value_eq v1 v2 = match v1, v2 with
    | Vect (shp1, xs), Vect (shp2, ys) ->
        if List.length shp1 = List.length shp2
            && List.for_all2 (=) shp1 shp2
            && List.for_all2 (=) xs ys
        then Vect ([], [1.])
        else Vect ([], [0.])
    | _ -> Vect ([], [0.])

let value_gt v1 v2 = match v1, v2 with
    | Vect ([], [c]), Vect (shp, xs) ->
        Vect ([], [if List.for_all ((>) c) xs then 1. else 0.])
    | Vect (shp, xs), Vect ([], [c]) ->
        Vect ([], [if List.for_all ((<) c) xs then 1. else 0.])
    | Vect (shp1, xs), Vect (shp2, ys) ->
        if List.length shp1 <> List.length shp2
            || not @@ List.for_all2 (=) shp1 shp2
        then value_err @@ sprintf "gt got different shapes [%s] and [%s]"
                (shp_to_str shp1) (shp_to_str shp2);
        Vect ([], [if List.for_all2 (>) xs ys then 1. else 0.])
    | _ -> value_err @@ sprintf "invalid gt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

let value_lt v1 v2 = match v1, v2 with
    | Vect ([], [c]), Vect (shp, xs) ->
        Vect ([], [if List.for_all ((<) c) xs then 1. else 0.])
    | Vect (shp, xs), Vect ([], [c]) ->
        Vect ([], [if List.for_all ((>) c) xs then 1. else 0.])
    | Vect (shp1, xs), Vect (shp2, ys) ->
        if List.length shp1 <> List.length shp2
            || not @@ List.for_all2 (=) shp1 shp2
        then value_err @@ sprintf "lt got different shapes [%s] and [%s]"
                (shp_to_str shp1) (shp_to_str shp2);
        Vect ([], [if List.for_all2 (<) xs ys then 1. else 0.])
    | _ -> value_err @@ sprintf "invalid lt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)


(** Conversions **)

let value_to_pair v = match v with
    | Vect (shp, data) -> (shp, data)
    | _ -> value_err "can only get pair of a vector"

let closure_to_triple v = match v with
    | Closure (ELambda (x, body), env) -> (x, body, env)
    | _ -> value_err "can only get triple of a closure of a lambda expression"
