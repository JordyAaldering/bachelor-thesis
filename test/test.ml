open OUnit2

let tests = "test suite" >::: [
    "primitive functions" >:: (fun _ -> run_test_tt_main Test_prf.tests);
]

let _ = run_test_tt_main tests
