open OUnit2

let choice_generator = QCheck2.Gen.(int_bound 3)
let tests = "test suite " >::: []
let _ = run_test_tt_main tests
