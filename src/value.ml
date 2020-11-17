open Printf

exception ValueFailure of string
let value_err msg = raise (ValueFailure msg)

type value =
    | Const of float
    | Vect of value list * value list
    | Closure of Ast.expr * Eval_env.env

let rec value_to_str v = match v with
    | Const x -> string_of_float x
    | Vect (shp, data) -> sprintf "<[%s], [%s]>"
        (value_lst_to_str shp) (value_lst_to_str data)
    | Closure (e, env) -> sprintf "{%s, %s}"
        (Ast.expr_to_str e) (Eval_env.env_to_str env)

and value_lst_to_str v =
    String.concat ", " (List.map value_to_str v)


(** Primitive Functions **)

let value_sel iv v = match iv, v with
    | Const i, Vect (shp, data) -> List.nth data (int_of_float i)
    | _ -> value_err @@ sprintf "invalid sel arguments %s and %s"
            (value_to_str iv) (value_to_str v)

let value_shape v = match v with
    | Const _x -> Const 0.
    | Vect (shp, _data) -> Vect ([Const (float_of_int @@ List.length shp)], shp)
    | _ -> value_err @@ sprintf "invalid shape argument %s" (value_to_str v)

let value_dim v = match v with
    | Const _x -> Const 0.
    | Vect (shp, _data) -> Const (float_of_int @@ List.length shp)
    | _ -> value_err @@ sprintf "invalid dim argument %s" (value_to_str v)


(** Predicates **)

let rec value_neg v = match v with
    | Const x -> Const (-.x)
    | Vect (shp, data) -> Vect (shp, List.map value_neg data)
    | _ -> value_err @@ sprintf "invalid neg argument %s" (value_to_str v)

and value_add v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x +. y)
    | Vect (shp1, xs), Vect (shp2, ys) -> Vect (shp1, List.map2 value_add xs ys)
    | _ -> value_err @@ sprintf "invalid add arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

and value_mul v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x *. y)
    | Vect (shp1, xs), Vect (shp2, ys) -> Vect (shp1, List.map2 value_mul xs ys)
    | _ -> value_err @@ sprintf "invalid mul arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

and value_div v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (x /. y)
    | Vect (shp1, xs), Vect (shp2, ys) -> Vect (shp1, List.map2 value_div xs ys)
    | _ -> value_err @@ sprintf "invalid div arguments %s and %s"
            (value_to_str v1) (value_to_str v2)


let rec value_is_truthy v = match v with
    | Const x -> x <> 0.
    | Vect (_shp, data) -> List.for_all value_is_truthy data
    | _ -> false

and value_not v =
    Const (if value_is_truthy v then 0. else 1.)

and value_eq v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (if x = y then 1. else 0.)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        if List.length shp1 <> List.length shp2 then
            Const 0.
        else
        List.fold_left2 (fun res x y ->
                value_mul res (value_eq x y)
            ) (Const 1.) xs ys
    | _ -> Const 0.

and value_gt v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (if x > y then 1. else 0.)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        List.fold_left2 (fun res x y ->
                value_mul res (value_gt x y)
            ) (Const 1.) xs ys
    | _ -> value_err @@ sprintf "invalid gt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)

and value_lt v1 v2 = match v1, v2 with
    | Const x, Const y -> Const (if x < y then 1. else 0.)
    | Vect (shp1, xs), Vect (shp2, ys) ->
        List.fold_left2 (fun res x y ->
                value_mul res (value_lt x y)
            ) (Const 1.) xs ys
    | _ -> value_err @@ sprintf "invalid lt arguments %s and %s"
            (value_to_str v1) (value_to_str v2)


(** Conversions **)

let value_to_pair v = match v with
    | Const x -> ([Const 0.], [Const x])
    | Vect (shp, data) -> (shp, data)
    | _ -> value_err "can only get pair of a constant or vector"

and closure_to_triple v = match v with
    | Closure (ELambda (x, body), env) -> (x, body, env)
    | _ -> value_err "can only get triple of a closure of a lambda expression"
