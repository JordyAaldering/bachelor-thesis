open OUnit2
open Src.Value

let tests = "test suite" >::: [
    "matmul" >:: (fun _ -> run_test_tt_main (Test_file.create_tests 
        "matmul" (VArray ([5], [5.; 10.; 20.; 30.; 40.]))));
    "shift" >:: (fun _ -> run_test_tt_main (Test_file.create_tests 
        "shift" (VArray ([5], [0.; 0.; 1.; 2.; 3.]))));
]

let _ =
    Src.Debug.debug_print := false;
    run_test_tt_main tests
