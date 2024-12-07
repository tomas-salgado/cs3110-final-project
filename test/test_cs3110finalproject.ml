open OUnit2
open Cs3110finalproject.Adventure

let choice_generator = QCheck2.Gen.(int_bound 5)
let choice_list_generator = QCheck2.Gen.(list_size (int_bound 10) (int_bound 5))

let size_test =
  "size test" >:: fun _ -> assert_equal 3 (List.length initial_scenario.choices)

let state_player_test chr expected =
  "property test for character" >:: fun _ ->
  assert_equal expected (create_game_state (create_character chr)).player.name

let invalid_character_test =
  "invalid character test" >:: fun _ ->
  assert_raises (Failure "Invalid character choice") (fun () ->
      create_character 6)

let initial_state_test expected property =
  "initial state" ^ property ^ "test" >:: fun _ ->
  let state = create_game_state (create_character 1) in
  let prop = function
    | "days survived" -> state.days_survived
    | "food" -> state.food
    | "gold" -> state.gold
    | _ -> failwith "not an option"
  in
  assert_equal expected (prop property)

let name_check n =
  let name =
    match n with
    | 1 -> "Kate the Knight"
    | 2 -> "Walter the Wizard"
    | 3 -> "Max the Monk"
    | 4 -> "Abigail the Archer"
    | 5 -> "Alan the Alchemist"
    | _ -> ""
  in
  String.equal name (create_game_state (create_character n)).player.name

(**checks that character name is as expected when chosen*)
let rand_state_player_test =
  QCheck_runner.to_ounit2_test
    (QCheck2.Test.make ~count:10 choice_generator name_check)

let tests =
  "test suite "
  >::: [
         size_test;
         state_player_test 1 "Kate the Knight";
         initial_state_test 0 "days survived";
         initial_state_test 100 "food";
         initial_state_test 50 "gold";
         rand_state_player_test;
         invalid_character_test;
       ]

let _ = run_test_tt_main tests
