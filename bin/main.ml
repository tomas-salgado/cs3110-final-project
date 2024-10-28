open Cs3110finalproject.Adventure

let display_character_message choice =
  let character = create_character choice in
  print_endline ("\nYou have chosen " ^ character.name ^ "!");
  print_endline ("Special ability: " ^ character.special_ability);
  print_endline "\nYour journey begins..."

let rec choose_character () =
  print_endline "";
  print_endline "⚔️🐪 WELCOME TO CAMELOT ADVENTURES 🐪⚔️";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline
    "A group of knights from the neighboring Kingdom of Cornwall have just \
     invaded Camelot! A battle has broken out, and as a citizen of Camelot it \
     is up to you to survive this battle, and help save Camelot from \
     downfall! ";
  print_endline "";
  print_endline
    "Please input the number corresponding to the character you would like to \
     be throughout this journey, each character has different strengths, so \
     choose wisely!:\n\
    \    - (1) Kate the Knight (Strength: Swordsmanship)\n\
    \    - (2) Walter the Wizard (Strength: Spell Casting)\n\
    \    - (3) Max the Monk (Strength: Healing)\n\
    \    - (4) Abigail the Archer (Strength: Speed)\n\
    \    - (5) Alan the Alchemist (Strength: Potion Crafting)\n\
    \    ";
  print_string "Enter your choice (1-5): ";

  match read_int_opt () with
  | Some choice when choice >= 1 && choice <= 5 ->
      display_character_message choice;
      choice
  | _ ->
      print_endline "Invalid input, please enter a number between 1 and 5.";
      choose_character ()

let rec get_valid_choice max_choice =
  print_string "> ";
  match read_int_opt () with
  | Some n when n >= 1 && n <= max_choice -> n
  | _ ->
      print_endline "Invalid choice. Please try again.";
      get_valid_choice max_choice

let display_game_status state =
  print_endline ("\nDay " ^ string_of_int state.days_survived);
  print_endline ("Health: " ^ string_of_int state.player.health);
  print_endline ("Location: " ^ state.current_location.name);
  print_endline ("Food: " ^ string_of_int state.food);
  print_endline ("Gold: " ^ string_of_int state.gold)

let rec game_loop state current_scenario =
  display_game_status state;
  print_endline current_scenario.description;
  List.iter
    (fun (choice : choice) -> print_endline choice.description)
    current_scenario.choices;

  let choice_num = get_valid_choice (List.length current_scenario.choices) in
  let choice = List.nth current_scenario.choices (choice_num - 1) in
  let new_state, message = choice.consequence state in
  print_endline message;

  if new_state.player.health <= 0 then
    print_endline "\nGame Over! You have fallen in battle."
  else
    let next_scenario =
      match current_scenario.description with
      | "The battle has just broken out and you arrive at the gates of Camelot \
         Castle. There are a few options for how could proceed. Would you like \
         to: "
        when choice_num = 1 ->
          wizard_fight_scenario (* Move to wizard fight if option 1 is chosen *)
      | "The battle has just broken out and you arrive at the gates of Camelot \
         Castle. There are a few options for how could proceed. Would you like \
         to: "
        when choice_num = 2 ->
          courtyard_scenario
          (* Move to courtyard scenario if option 2 is chosen *)
      | _ -> current_scenario
    in
    game_loop
      { new_state with days_survived = new_state.days_survived + 1 }
      next_scenario

let () =
  let choice = choose_character () in
  let character = create_character choice in
  let initial_state = create_game_state character in
  game_loop initial_state initial_scenario
