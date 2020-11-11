open Ast
open Value
open Printf

exception StorageFailure of string

type storage = (string, value) Hashtbl.t

let st_new: unit -> storage = fun () ->
    Hashtbl.create 100

(* Helper function for debugging *)
let find_and_raise st p expected msg =
    let exists = try Hashtbl.find st p; true with Not_found -> false in
    if expected <> exists then
        raise (StorageFailure msg)

let st_add st p v =
    find_and_raise st p false
        @@ sprintf "Attempt to add duplicate pointer `%s'" p;
    Hashtbl.add st p v;
    st

let st_remove st p =
    find_and_raise st p true
        @@ sprintf "Attempt to remove non-existing pointer `%s'" p;
    Hashtbl.remove st p;
    st

let st_update st p v =
    find_and_raise st p true
        @@ sprintf "Attempt to update non-existing pointer `%s'" p;
    Hashtbl.replace st p v;
    st

let st_lookup st p =
    find_and_raise st p true
        @@ sprintf "Attempt to lookup non-existing pointer `%s'" p;
    Hashtbl.find st p

let st_to_str st =
    Hashtbl.fold (fun k v tail ->
        sprintf "%s -> %s\n%s" k (value_to_str v) tail
    ) st ""
