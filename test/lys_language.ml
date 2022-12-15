open OUnit2

let () = run_test_tt_main Test_parsing.suite

let () = run_test_tt_main Test_typing.suite
