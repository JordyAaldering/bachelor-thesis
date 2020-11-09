type loc =
    | Internal
    | Source of {
        name: string;
        line: int;
        col: int
    }

let mk_loc name l c =
    Source { name=name; line=l; col=c }

let loc_to_str loc = match loc with
    | Internal ->
        Printf.sprintf "<loc internal>"
    | Source { name=name; line=l; col=c } ->
        Printf.sprintf "%s:%d:%d" name l c
