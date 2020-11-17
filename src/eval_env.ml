open Printf

type env = (string * string) list

exception EnvFailure of string
let env_err msg = raise @@ EnvFailure msg

let env_add env v p =
    (v, p) :: env

let rec env_lookup env v = match env with
    | [] -> env_err @@ sprintf "lookup of variable `%s' failed" v
    | (v', p') :: tl -> if v' = v then p' else env_lookup tl v

let env_to_str env =
    if List.length env = 0 then "[]"
    else
        String.concat ", " (List.map (fun vp ->
            let v, p = vp in
            sprintf "%s -> %s" v p
        ) env)
