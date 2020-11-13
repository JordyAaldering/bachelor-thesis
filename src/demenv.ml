open Printf

exception DemEnvFailure of string

type dem_env = (string, int array) Hashtbl.t

let dem_env_empty: unit -> dem_env = fun () ->
    Hashtbl.create 100

let dem_env_new: string -> int array -> dem_env = fun x dem ->
    let st = Hashtbl.create 100 in
    Hashtbl.add st x dem;
    st

(* Helper function for debugging *)
let dem_env_find_and_raise st p expected msg =
    let exists = try Hashtbl.find st p; true with Not_found -> false in
    if expected <> exists then
        raise @@ DemEnvFailure msg

let dem_env_set: dem_env -> string -> int array -> dem_env = fun st x dem ->
    try
        let dem_old = Hashtbl.find st x in
        let dem_oplus = Array.map2 max dem dem_old in
        Hashtbl.replace st x dem_oplus;
        st
    with Not_found ->
        Hashtbl.add st x dem;
        st

let dem_env_combine: dem_env -> dem_env -> dem_env = fun xs ys ->
    Hashtbl.iter (fun y dem_y ->
        try
            let dem_x = Hashtbl.find xs y in
            let dem_max = Array.map2 max dem_x dem_y in
            Hashtbl.replace xs y dem_max;
        with Not_found ->
            Hashtbl.add xs y dem_y;
    ) ys;
    xs

let dem_env_remove st p =
    Hashtbl.remove st p;
    st

let dem_env_lookup st p =
    dem_env_find_and_raise st p true
        @@ sprintf "Attempt to lookup non-existing pointer `%s'" p;
    Hashtbl.find st p

let dem_env_to_str st =
    Hashtbl.fold (fun k v tail ->
        sprintf "%s -> %s\n%s" k (String.concat ", " (List.map string_of_int v)) tail
    ) st ""
