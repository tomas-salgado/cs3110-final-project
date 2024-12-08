open OUnit2
open Cs3110finalproject.Adventure
open Cs3110finalproject.Maze

let int_printer (i : int) : string = string_of_int i

let make_int_test expected_int output_int =
  "test" >:: fun _ -> assert_equal expected_int output_int ~printer:int_printer

let pair_printer (p : int * int) : string =
  "(" ^ string_of_int (fst p) ^ ", " ^ string_of_int (snd p) ^ ")"

let pair_comparator (p1 : int * int) (p2 : int * int) : bool =
  fst p1 = fst p2 && snd p1 = snd p2

let make_pair_test (expected : int * int) (output : int * int) =
  "test" >:: fun _ ->
  assert_equal expected output ~printer:pair_printer ~cmp:pair_comparator

let maze_result_printer (r : maze_result) : string =
  match r with
  | Success steps -> "Success " ^ string_of_int steps
  | Failure -> "Failure"

let maze_result_comparator (r1 : maze_result) (r2 : maze_result) : bool =
  match (r1, r2) with
  | Success steps1, Success steps2 -> steps1 = steps2
  | Failure, Failure -> true
  | _ -> false

let make_maze_result_test (expected : maze_result) (output : maze_result) =
  "test" >:: fun _ ->
  assert_equal expected output ~printer:maze_result_printer
    ~cmp:maze_result_comparator

let bool_printer b_val = if b_val then "true" else "false"

let make_boolean_test expected_bool actual_bool =
  "test" >:: fun _ ->
  assert_equal expected_bool actual_bool ~cmp:Bool.equal ~printer:bool_printer

let array_comparator arr1 arr2 =
  let rows_equal row1 row2 =
    Array.length row1 = Array.length row2 && Array.for_all2 ( = ) row1 row2
  in
  Array.length arr1 = Array.length arr2 && Array.for_all2 rows_equal arr1 arr2

let array_printer arr =
  let row_to_string row =
    Array.fold_left (fun acc elem -> acc ^ elem ^ " ") "" row |> String.trim
  in
  Array.fold_left (fun acc row -> acc ^ row_to_string row ^ "\n") "" arr

let print_2d_array arr = array_printer arr |> print_string

let make_2d_array_test arr1 arr2 =
  "test" >:: fun _ ->
  assert_equal arr1 arr2 ~cmp:array_comparator ~printer:array_printer

let choice_generator = QCheck2.Gen.(int_bound 5)
let choice_list_generator = QCheck2.Gen.(list_size (int_bound 10) (int_bound 5))

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

let adventure_tests =
  [
    make_int_test 3 (List.length initial_scenario_one.choices);
    state_player_test 1 "Kate the Knight";
    initial_state_test 0 "days survived";
    initial_state_test 100 "food";
    initial_state_test 50 "gold";
    rand_state_player_test;
    invalid_character_test;
  ]

let maze_tests =
  [
    (let maze = initialize_maze () in
     let expected_maze =
       [|
         [| "S"; " "; " "; " "; "X" |];
         [| " "; "X"; " "; "X"; " " |];
         [| " "; " "; " "; " "; " " |];
         [| "X"; "X"; " "; "X"; " " |];
         [| " "; " "; " "; " "; "E" |];
       |]
     in
     make_2d_array_test maze expected_maze);
    (let maze = initialize_maze () in
     make_pair_test (1, 0) (move_player maze (0, 0) 's'));
    (let maze = initialize_maze () in
     make_pair_test (0, 1) (move_player maze (0, 0) 'd'));
    (let maze = initialize_maze () in
     make_pair_test (0, 1) (move_player maze (0, 1) 's'));
    (let maze = initialize_maze () in
     make_pair_test (3, 2) (move_player maze (3, 2) 'a'));
    (let maze = initialize_maze () in
     make_pair_test (3, 2) (move_player maze (3, 2) 'z'));
    (let maze = initialize_maze () in
     make_pair_test (1, 2) (move_player maze (2, 2) 'w'));
    (let maze = initialize_maze () in
     make_boolean_test true (is_exit_reached maze (4, 4)));
    (let maze = initialize_maze () in
     let result = game_loop maze (4, 4) 0 in
     make_maze_result_test (Success 0) result);
  ]

let tests = "test suite" >::: adventure_tests @ maze_tests
let _ = run_test_tt_main tests
