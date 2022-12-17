open OUnit2

let suites = "suite" >::: [ Test_parsing.suite; Test_typing.suite ]
let () = run_test_tt_main suites
