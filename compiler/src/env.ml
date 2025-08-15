open Printf

module Env = Map.Make(String)

(** an environment mapping variables to internal pointers *)
type ptr_env = string Env.t

(** a counter used for creating unique pointer names *)
let ptr_count : int ref = ref 0

(** creates a unique pointer name *)
let create_fresh_ptr () =
    ptr_count := !ptr_count + 1;
    sprintf "p%d" !ptr_count

let ptr_env_to_str (env: ptr_env) : string =
    if Env.is_empty env then "[]"
    else
        Env.fold (fun k v tail ->
            sprintf "%s -> %s%s" k v
                (if tail = "" then "" else ", " ^ tail)
        ) env ""


(** an environment mapping variables to their corresponding demand arrays *)
type pv_env = int Array.t Env.t

let pv_env_to_str (env: pv_env) : string =
    if Env.is_empty env then "[]"
    else
        Env.fold (fun k v tail ->
            sprintf "%s -> %s%s" k
                (sprintf "[%s]" (String.concat ", " @@ Array.to_list @@ Array.map string_of_int v))
                (if tail = "" then "" else ", " ^ tail)
        ) env ""

(** takes the union of two demand environments by taking the element-wise maximum 
    of demand arrays that occur in both environments *)
let pv_env_union (env1: pv_env) (env2: pv_env) : pv_env =
    Env.union (fun _k x y ->
        Some (Array.map2 max x y)
    ) env1 env2
