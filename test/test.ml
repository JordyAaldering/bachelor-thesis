open OUnit2

let path = "matmul"
let expected = Src.Value.VArray ([5], [5.; 10.; 20.; 30.; 40.])

let tests = "test suite" >::: [
    "matmul" >:: (fun _ -> run_test_tt_main (Test_file.create_tests path expected));
]

let _ =
    Src.Debug.debug_print := false;
    run_test_tt_main tests
