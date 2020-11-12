exception EnvFailure of string

type env = (string * string) list

let env_err msg = raise @@ EnvFailure msg

let env_new: unit -> env = fun () -> []

let env_add e v p = (v, p) :: e

let rec env_lookup e v = match e with
    | [] -> env_err @@ Printf.sprintf "lookup of variable `%s' failed" v
    | (v', p') :: tl -> if v' = v then p' else env_lookup tl v

let env_to_str e =
    if List.length e = 0 then "[]"
    else
        String.concat ", " (List.map (fun vp ->
                let v, p = vp in
                Printf.sprintf "%s -> %s" v p
            ) e)
