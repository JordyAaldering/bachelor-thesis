open Printf

module Env = Map.Make(String)

type ptr_env = string Env.t

let ptr_env_to_str env =
    if Env.is_empty env then "[]"
    else
        Env.fold (fun k v tail ->
            sprintf "%s -> %s%s" k v
                (if tail = "" then "" else ", " ^ tail)
        ) env ""
