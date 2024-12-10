open OUnit2
open Cs3110finalproject.Adventure
open Cs3110finalproject.Maze
open Cs3110finalproject.Wordscramble

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

let int_option_cmp opt1 opt2 =
  match (opt1, opt2) with
  | Some x, Some y -> x = y
  | None, None -> true
  | _, _ -> false

let int_option_printer = function
  | Some x -> "Some " ^ string_of_int x
  | None -> "None"

let make_option_test option1 option2 =
  "test" >:: fun _ ->
  assert_equal option1 option2 ~cmp:int_option_cmp ~printer:int_option_printer

let string_comparator (s1 : string) (s2 : string) : bool = String.equal s1 s2
let string_printer (s : string) : string = "\"" ^ s ^ "\""

let make_string_test str1 str2 =
  "test" >:: fun _ ->
  assert_equal str1 str2 ~cmp:string_comparator ~printer:string_printer

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
    make_int_test 2 (List.length initial_scenario_one.choices);
    state_player_test 1 "Kate the Knight";
    initial_state_test 100 "food";
    initial_state_test 20 "gold";
    rand_state_player_test;
    invalid_character_test;
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth initial_scenario_one.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a skilled Knight, you are able to fend off the guards and enter \
        through the castle's front gates. Well done Kate! ♞"
       message);
    (let abigail = create_character 4 in
     let state = create_game_state abigail in
     let choice = List.nth initial_scenario_one.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a skilled Archer, you are able to use your bow and arrow to fend \
        off the guards and enter through the castle's front gates. Well done \
        Abigail! 🏹"
       message);
    (let walter = create_character 2 in
     let state = create_game_state walter in
     let choice = List.nth initial_scenario_one.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "Your character is not skilled in battle, you make it through the front \
        gates, but the guards do some serious damage to you! You lose 50 \
        health points. 🤕"
       message);
    (let walter = create_character 2 in
     let state = create_game_state walter in
     let choice = List.nth initial_scenario_one.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "You begin walking around to the garden to see if you can enter from \
        there."
       message);
    (let max = create_character 3 in
     let state = create_game_state max in
     let choice = List.nth garden_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a monk, your calming demeanor helps you to blend in with and sneak \
        past the guards, entering the castle through the back entrance. 😌"
       message);
    (let alan = create_character 5 in
     let state = create_game_state alan in
     let choice = List.nth garden_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "You successfully sneak into the castle but get spotted and take some \
        damage. You lose 50 health points. 🤕"
       message);
    (let alan = create_character 5 in
     let state = create_game_state alan in
     let choice = List.nth garden_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test "Camelot Castle" new_state.current_location);
    (let walter = create_character 2 in
     let state = create_game_state walter in
     let choice = List.nth initial_scenario_two.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test "Camelot Castle" new_state.current_location);
    (let alan = create_character 5 in
     let state = create_game_state alan in
     let choice = List.nth initial_scenario_two.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test "Camelot Castle" new_state.current_location);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth initial_scenario_two.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "The guard overpowers you, you win but nearly lose your life in the \
        process. You lose 60 health points. 🤕"
       message);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth initial_scenario_two.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test "Castle Lakes" new_state.current_location);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth after_fight_front_entrance_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test "Queen's Quarters" new_state.current_location);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth after_fight_front_entrance_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test "Queen's Quarters" new_state.current_location);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth side_entrance_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "The queen is not impressed by your jokes and tells the guards to take \
        you to the castle's jail. You lose 60 health points. 🤕"
       message);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth side_entrance_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a knight, you begin jousting with one of the queen's guards and win \
        the duel. The queen is impressed by such a well fought battle and lets \
        you proceed."
       message);
    (let max = create_character 3 in
     let state = create_game_state max in
     let choice = List.nth side_entrance_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a monk you show the queen the meaning of peace and hapiness. She is \
        very please with this and lets you proceed."
       message);
    (let walter = create_character 2 in
     let state = create_game_state walter in
     let choice = List.nth side_entrance_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a wizard, you cast a spell that causes flowers petals to begin \
        falling from the sky. The queen is impressed and lets you proceed."
       message);
    (let alan = create_character 5 in
     let state = create_game_state alan in
     let choice = List.nth side_entrance_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As an alchemist, you mix a potion using the Queen's teas to create a \
        potion of youth. The queen is impressed and lets you proceed."
       message);
    (let abigail = create_character 4 in
     let state = create_game_state abigail in
     let choice = List.nth side_entrance_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As an archer you show your crossbow skills to the queen and she is \
        impressed. She lets you proceed."
       message);
    (let abigail = create_character 4 in
     let state = create_game_state abigail in
     let choice = List.nth side_entrance_scenario.choices 2 in
     let new_state, message = choice.consequence state in
     make_string_test
       "The queen's guards beat you up really badly. You wake up in the \
        castle's jail. You lose 30 health points!!! 🤕"
       message);
    (let abigail = create_character 4 in
     let state = create_game_state abigail in
     let choice = List.nth market_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "You bought some bread and vegetables for 10 pieces of gold. You eat \
        the food and gain 20 health points! 🍞"
       message);
    (let abigail = create_character 4 in
     let state = create_game_state abigail in
     let choice = List.nth market_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test "You leave the market to go somewhere else." message);
    (let walter = create_character 2 in
     let state = create_game_state walter in
     let choice = List.nth after_fight_guard_in_garden_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "You do a dance for the princess and she is impressed. She gives you 10 \
        pieces of gold and tells you to go to the market to buy yourself \
        something as a treat."
       message);
    (let walter = create_character 2 in
     let state = create_game_state walter in
     let choice = List.nth after_fight_guard_in_garden_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test "Princess's Courtyard" new_state.current_location);
    (let max = create_character 3 in
     let state = create_game_state max in
     let choice = List.nth opposing_guards_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test "Camelot Market" new_state.current_location);
    (let max = create_character 3 in
     let state = create_game_state max in
     let choice = List.nth opposing_guards_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "You walk with the group of opposing knights towards the town market."
       message);
    (let abigail = create_character 3 in
     let state = create_game_state abigail in
     let choice = List.nth mystic_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test "" message);
    (let abigail = create_character 3 in
     let state = create_game_state abigail in
     let choice = List.nth king_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test "" message);
    (let abigail = create_character 3 in
     let state = create_game_state abigail in
     let choice = List.nth jail_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test "" message);
  ]

let maze_tests =
  [
    (let maze = initialize_maze () in
     let expected_maze =
       [|
         [| "S"; " "; "X"; " "; " "; "X"; " "; " "; " "; " " |];
         [| " "; "X"; "X"; " "; "X"; " "; "X"; "X"; " "; " " |];
         [| " "; " "; " "; " "; " "; " "; " "; "X"; " "; " " |];
         [| " "; "X"; "X"; "X"; "X"; " "; "X"; " "; " "; " " |];
         [| " "; " "; " "; " "; "X"; " "; " "; " "; "X"; " " |];
         [| "X"; "X"; "X"; " "; " "; " "; "X"; " "; "X"; " " |];
         [| " "; " "; " "; " "; " "; "X"; "X"; " "; " "; " " |];
         [| " "; "X"; "X"; "X"; " "; " "; " "; " "; "X"; " " |];
         [| " "; " "; " "; "X"; " "; "X"; "X"; " "; " "; " " |];
         [| " "; " "; " "; " "; " "; " "; " "; " "; " "; "E" |];
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
     make_pair_test (0, 3) (move_player maze (0, 3) 'a'));
    (let maze = initialize_maze () in
     make_pair_test (3, 2) (move_player maze (3, 2) 'z'));
    (let maze = initialize_maze () in
     make_pair_test (2, 2) (move_player maze (2, 2) 'w'));
    (let maze = initialize_maze () in
     make_boolean_test true (is_exit_reached maze (9, 9)));
    (let maze = initialize_maze () in
     let result = game_loop maze (9, 9) 0 in
     make_maze_result_test (Success 0) result);
    make_option_test (get_final_score (Success 0)) (Some 0);
    make_option_test (get_final_score Failure) None;
  ]

let word_scramble_tests =
  [
    make_boolean_test true (List.length words_bank > 0);
    (let first_list = [ 1; 2; 3; 4; 5; 6; 7 ] in
     let shuffled_list = shuffle first_list in
     make_boolean_test true (first_list <> shuffled_list));
    (let first_list = [ "a"; "b"; "c"; "d"; "e" ] in
     let shuffled_list = shuffle first_list in
     make_int_test (List.length shuffled_list) (List.length first_list));
    (let word = pick_random_word () in
     make_boolean_test true (List.mem word words_bank));
    (let word = "medevial" in
     let scrambled_word = scramble_word word in
     make_int_test (String.length word) (String.length scrambled_word));
  ]

let tests = "test suite" >::: adventure_tests @ maze_tests @ word_scramble_tests
let _ = run_test_tt_main tests
