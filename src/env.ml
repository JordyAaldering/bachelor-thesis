open Printf

module Env = Map.Make(String)

type ptr_env = string Env.t

type pv_env = int Array.t Env.t

let ptr_env_to_str (env: ptr_env) : string =
    if Env.is_empty env then "[]"
    else
        Env.fold (fun k v tail ->
            sprintf "%s -> %s%s" k v
                (if tail = "" then "" else ", " ^ tail)
        ) env ""

let pv_env_to_str (env: pv_env) : string =
    if Env.is_empty env then "[]"
    else
        Env.fold (fun k v tail ->
            sprintf "%s -> %s%s" k
                (sprintf "[%s]" (String.concat ", " @@ Array.to_list @@ Array.map string_of_int v))
                (if tail = "" then "" else ", " ^ tail)
        ) env ""

let pv_env_union (env1: pv_env) (env2: pv_env) : pv_env =
    Env.union (fun _key x y ->
        Some (Array.map2 max x y)
    ) env1 env2
