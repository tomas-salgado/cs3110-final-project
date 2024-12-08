open Cs3110finalproject.Adventure
open Cs3110finalproject.Maze

(** [play_intro_maze state] runs the maze game as an introduction to the
    adventure. Adjusts the [state] based on the maze result. *)
let play_intro_maze state =
  print_endline
    "\nBefore your adventure begins, you find yourself lost in a maze!";
  let maze_result = play_maze () in
  match get_final_score maze_result with
  | Some steps ->
      print_endline
        ("Congratulations! You completed the maze in " ^ string_of_int steps
       ^ " steps.");
      {
        state with
        gold = state.gold + (50 - steps);
        food = state.food + 1;
        maze_result = Some (Success steps);
      }
  | None ->
      print_endline
        "You failed to escape the maze. You start your journey injured.";
      {
        state with
        player = { state.player with health = state.player.health - 10 };
        maze_result = Some Failure;
      }

(** [display_character_message choice] displayed a message regarding the
    character [choice] that the user selects and their special ability within
    the game. *)
let display_character_message choice =
  let character = create_character choice in
  print_endline ("\nYou have chosen " ^ character.name ^ "!");
  print_endline ("Special ability: " ^ character.special_ability);
  print_endline "\nYour journey begins..."

(** [choose_character] prints the initial message to the user introducing them
    to the game and allowing them to select their character. *)
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

(** [get_valid_choice max_choice] handles if the user does not select a choice
    within bounds of [max_choice]. *)
let rec get_valid_choice max_choice =
  print_string "> ";
  match read_int_opt () with
  | Some n when n >= 1 && n <= max_choice -> n
  | _ ->
      print_endline "Invalid choice. Please try again.";
      get_valid_choice max_choice

(** [display_game_status state] displays the [state] of the user in the game
    inclduing days survived, health, location, food, and gold. *)
let display_game_status state =
  print_endline ("\nDay " ^ string_of_int state.days_survived);
  print_endline ("Health: " ^ string_of_int state.player.health);
  print_endline ("Location: " ^ state.current_location.name);
  print_endline ("Food: " ^ string_of_int state.food);
  print_endline ("Gold: " ^ string_of_int state.gold)

(** [game_loop state current_scenario] loop that allows the user to interact
    with and pass through multiple scenarios, updating the [state] to account
    for the choice the user makes in the [current_scenario]. *)
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
  else if new_state.player.health = 120 then print_endline "\n YOU WIN!"
  else
    let next_scenario =
      match current_scenario.description with
      | "The battle has just broken out and you arrive at the gates of Camelot \
         Castle. There are a few options for how could proceed. Would you like \
         to: "
        when choice_num = 1 -> wizard_fight_scenario
      | "The battle has just broken out and you arrive at the gates of Camelot \
         Castle. There are a few options for how could proceed. Would you like \
         to: "
        when choice_num = 2 -> courtyard_scenario
      | "The battle has just broken out and you arrive at the gates of Camelot \
         Castle. There are a few options for how could proceed. Would you like \
         to: "
        when choice_num = 3 -> market_scenario
      | "The castle's Wizard appears and challenges you to a magical duel in \
         order to pass through the castle's gates. How would you like to \
         proceed?"
        when choice_num = 2 -> mystic_scenario
      | "You entered the town farmer's market. What would you like to do?"
        when choice_num = 2 || choice_num = 1 -> mystic_scenario
      | "You walk over to to the castle courtyard, but guards are patrolling. \
         What would you like to do?"
        when choice_num = 2 -> sneak_in_scenario
      | _ -> current_scenario
    in
    game_loop
      { new_state with days_survived = new_state.days_survived + 1 }
      next_scenario

let choose_initial_scenario maze_result =
  match maze_result with
  | Some (Success steps) ->
      if steps >= 1 && steps <= 3 then courtyard_scenario
      else if steps >= 4 && steps <= 5 then wizard_fight_scenario
      else if steps > 5 then market_scenario
      else courtyard_scenario
  | _ -> courtyard_scenario

let () =
  let choice = choose_character () in
  let character = create_character choice in
  let initial_state = create_game_state character in
  let state_after_maze = play_intro_maze initial_state in
  game_loop state_after_maze
    (choose_initial_scenario state_after_maze.maze_result)
