open OUnit2
open Cs3110finalproject.Adventure
open Cs3110finalproject.Maze
open Cs3110finalproject.Wordscramble

(** [int_printer i] is a printer for integer values. Given integer value [i], it
    returns this value as a string.*)
let int_printer (i : int) : string = string_of_int i

(** [make_int_test expected_int output_int] is an OUnit2.test that takes an
    [expected_int] as the integer value it will check equal against the integer
    [output_int]. *)
let make_int_test expected_int output_int =
  "test" >:: fun _ -> assert_equal expected_int output_int ~printer:int_printer

(** [pair_printer p] is a printer for a pair of integer values. Given an integer
    pair [p], it returns this value as a string. *)
let pair_printer (p : int * int) : string =
  "(" ^ string_of_int (fst p) ^ ", " ^ string_of_int (snd p) ^ ")"

(** [pair_comparator p1 p2] is a comparator for a pair of integer values. Given
    integer pairs [p1] and [p2] returns true if both the first and second values
    of the pairs are equal and false otherwise. *)
let pair_comparator (p1 : int * int) (p2 : int * int) : bool =
  fst p1 = fst p2 && snd p1 = snd p2

(** [make_pair_test expected output] is an OUnit2.test that takes [expected] as
    the integer pair value it will check equal against the integer pair
    [output]. *)
let make_pair_test (expected : int * int) (output : int * int) =
  "test" >:: fun _ ->
  assert_equal expected output ~printer:pair_printer ~cmp:pair_comparator

(** [maze_result_printer r] is a printer for a pair of maze_result value. Given
    maze_result [r], it returns this value as a string. *)
let maze_result_printer (r : maze_result) : string =
  match r with
  | Success steps -> "Success " ^ string_of_int steps
  | Failure -> "Failure"

(** [maze_result_comparator r1 r2] is a comparator for maze_result values. Given
    maze_result values [r1] and [r2] returns true if both are Success with the
    same integer number of steps or true if both are failure, and false
    otherwise. *)
let maze_result_comparator (r1 : maze_result) (r2 : maze_result) : bool =
  match (r1, r2) with
  | Success steps1, Success steps2 -> steps1 = steps2
  | Failure, Failure -> true
  | _ -> false

(** [make_maze_result_test expected output] is an OUnit2.test that takes
    [expected] as the maze_result value it will check equal against the
    maze_result value [output]. *)
let make_maze_result_test (expected : maze_result) (output : maze_result) =
  "test" >:: fun _ ->
  assert_equal expected output ~printer:maze_result_printer
    ~cmp:maze_result_comparator

(** [bool_printer b_val] is a printer for boolean values. Given boolean value
    [b_val] it prints "True" if [b_val] is true or "False" if [b_val] is false. *)
let bool_printer b_val = if b_val then "true" else "false"

(** [make_correct_spelling_test expected_bool actual_bool] is an OUnit2.test
    that takes an [expected_bool] as the boolean value it will check equal
    against the boolean [actual_bool]. *)
let make_boolean_test expected_bool actual_bool =
  "test" >:: fun _ ->
  assert_equal expected_bool actual_bool ~cmp:Bool.equal ~printer:bool_printer

(** [array_comparator arr1 arr2] is a comparator for array values. Given array
    values [arr1] and [arr2] returns true if these arrays and equal and false
    otherwise. *)
let array_comparator arr1 arr2 =
  let rows_equal row1 row2 =
    Array.length row1 = Array.length row2 && Array.for_all2 ( = ) row1 row2
  in
  Array.length arr1 = Array.length arr2 && Array.for_all2 rows_equal arr1 arr2

(** [array_printer arr] is a printer for array values. Given array value [arr]
    it prints the string representation of [arr]. *)
let array_printer arr =
  let row_to_string row =
    Array.fold_left (fun acc elem -> acc ^ elem ^ " ") "" row |> String.trim
  in
  Array.fold_left (fun acc row -> acc ^ row_to_string row ^ "\n") "" arr

(** [print_2d_array arr] is the string representation of the 2D array [arr]. *)
let print_2d_array arr = array_printer arr |> print_string

(** [make_2d_array_test arr1 arr2] is an OUnit2.test that takes a 2D array value
    [arr1] and checks equal against the 2D array value [arr2]. *)
let make_2d_array_test arr1 arr2 =
  "test" >:: fun _ ->
  assert_equal arr1 arr2 ~cmp:array_comparator ~printer:array_printer

(** [int_option_cmp opt1 opt2] is a comparator for integer option values. Given
    int option values [opt2] and [opt2] returns true if these options are the
    same and false otherwise. *)
let int_option_cmp opt1 opt2 =
  match (opt1, opt2) with
  | Some x, Some y -> x = y
  | None, None -> true
  | _, _ -> false

(** [int_option_printer x] is a printer for integer option values. Given integer
    option value [x] it prints the string representation of [x]. *)
let int_option_printer = function
  | Some x -> "Some " ^ string_of_int x
  | None -> "None"

(** [make_option_test option1 option2] is an OUnit2.test that takes an int
    option value [option1] and checks equal against the int option value
    [option2]. *)
let make_option_test option1 option2 =
  "test" >:: fun _ ->
  assert_equal option1 option2 ~cmp:int_option_cmp ~printer:int_option_printer

(** [string_comparator s1 s2] is a comparator for string values. Given the
    strings [s1] and [s2] returns true if these strings are the same and false
    otherwise. *)
let string_comparator (s1 : string) (s2 : string) : bool = String.equal s1 s2

(** [string_printer s] is a printer for string values. Given string [s] it
    prints the string. *)
let string_printer (s : string) : string = "\"" ^ s ^ "\""

(** [make_string_test str1 str2] is an OUnit2.test that takes a string value
    [str1] and checks equal against the string value [str2]. *)
let make_string_test str1 str2 =
  "test" >:: fun _ ->
  assert_equal str1 str2 ~cmp:string_comparator ~printer:string_printer

(** [choice_generator] is a random integer 1-5. *)
let choice_generator = QCheck2.Gen.(int_bound 5)

(** [choice_list_generator] is a list of random integers. *)
let choice_list_generator = QCheck2.Gen.(list_size (int_bound 10) (int_bound 5))

(** [state_player_test chr expected] creates tests based on the input character
    [chr] and expected result [expected] to test the player's state in the
    choose your adventure game. *)
let state_player_test chr expected =
  "property test for character" >:: fun _ ->
  assert_equal expected (create_game_state (create_character chr)).player.name

(** [invalid_character_test] creates a test for when the option to user selects
    for character is invalid. *)
let invalid_character_test =
  "invalid character test" >:: fun _ ->
  assert_raises (Failure "Invalid character choice") (fun () ->
      create_character 6)

(** [initial_state_test expected property] creates OUnit2 tests to check that
    the initial state's gold and food is initialized to the correct amounts. *)
let initial_state_test expected property =
  "initial state" ^ property ^ "test" >:: fun _ ->
  let state = create_game_state (create_character 1) in
  let prop = function
    | "food" -> state.food
    | "gold" -> state.gold
    | _ -> failwith "not an option"
  in
  assert_equal expected (prop property)

(** [name_check n] creates OUnit2 tests to check that the correct character is
    chosen when the user inputs the correct corresponding number. *)
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

(** [rand_state_player_test] uses QCheck to create random tests for testing
    different initial properties of game state and character choice with OUnit2
    tests. *)
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
    (let abigail = create_character 4 in
     let state = create_game_state abigail in
     let choice = List.nth garden_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As an archer, your stealth and agility allow you to sneak past the \
        gardeners quickly and quietly, entering the castle through the backe \
        entrance. 🏹"
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
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth market_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "You do not have anything to offer the merchant. You take half the \
        bread for 15 gold pieces. Well done, you eat and gain 10 health \
        points! 🍞"
       message);
    (let walter = create_character 2 in
     let state = create_game_state walter in
     let choice = List.nth market_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a wizard, you cast a spell on the merchant, and he gives you the \
        bread for free. Well done, you eat and gain 20 health points! 🍞"
       message);
    (let alan = create_character 5 in
     let state = create_game_state alan in
     let choice = List.nth market_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As an alchemist, you give the merchant an elixir of life plus your 30 \
        gold pieces for the bread. Well done, you eat and gain 20 health \
        points! 🍞"
       message);
    (let abigail = create_character 4 in
     let state = create_game_state abigail in
     let choice = List.nth market_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As an archer, you offer the merchant bow and arrow lessons so that he \
        can protect himself from thieves along with your 30 gold pieces. Well \
        done, you eat and gain 20 health points! 🍞"
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
       "As a wizard, you use magic to do an enchanting dance for the princess. \
        She is please and gives your 5 gold pieces to buy yourself a treat \
        with at the market."
       message);
    (let max = create_character 3 in
     let state = create_game_state max in
     let choice = List.nth after_fight_guard_in_garden_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a monk, you are able to do a calming dance for the princess that \
        makes her laugh. She is please and gives your 5 gold pieces to buy \
        yourself a treat with at the market."
       message);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth after_fight_guard_in_garden_scenario.choices 0 in
     let new_state, message = choice.consequence state in
     make_string_test
       "The princess is not impressed by your dance, she has her guards kick \
        you out of the castle. You begin to walk to the town market to find \
        some food to eat."
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
       "You are not able to blend in very well with this crowd of Knights \
        since you look out of place. They notice you are an intruder and you \
        have to run away. You lose 10 health points."
       message);
    (let kate = create_character 1 in
     let state = create_game_state kate in
     let choice = List.nth opposing_guards_scenario.choices 1 in
     let new_state, message = choice.consequence state in
     make_string_test
       "As a knight you are easily able to blend in with this crowd. You walk \
        with them as they go to the town farmer's market."
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
    make_boolean_test false (guess_loop 0 "");
  ]

let tests = "test suite" >::: adventure_tests @ maze_tests @ word_scramble_tests
let _ = run_test_tt_main tests
