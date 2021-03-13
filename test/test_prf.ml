open OUnit2
open Src.Value

let dim_is_scalar () =
    let v = VArray ([5], [1.; 2.; 3.; 4.; 5.]) in
    let dim = value_dim v in
    let shp, _ = extract_value dim in
    assert_equal shp []

let tests = "primitive function tests" >::: [
    "dim is scalar" >:: (fun _ -> dim_is_scalar ());
]
