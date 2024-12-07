open Cs3110finalproject.Adventure
open Cs3110finalproject.Maze

(** [play_intro_maze state] runs the maze game as an introduction to the
    adventure. Adjusts the [state] based on the maze result. *)
let play_intro_maze state =
  print_endline
    "\n\
     You awake in the Camelot Castle gardens at the very corner of a maze. You\n\
    \    must navigate and escape the maze in order to escape and save Camelot \
     from attack.";
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
      (* First Option *)
      | "You escape the maze quickly and are able to make it to the gates of \
         Camelot Castle. There are guards everywhere. There are a few options \
         for how can proceed. Would you like to:"
        when choice_num = 1 -> after_fight_front_entrance_scenario
      | "You escape the maze quickly and are able to make it to the gates of \
         Camelot Castle. There are guards everywhere. There are a few options \
         for how can proceed. Would you like to:"
        when choice_num = 2 -> side_entrance_scenario
      | "You escape the maze quickly and are able to make it to the gates of \
         Camelot Castle. There are guards everywhere. There are a few options \
         for how can proceed. Would you like to:"
        when choice_num = 3 -> market_scenario
      (* Second Option *)
      | "It took you some time to escape the maze. After escaping the maze, \
         you arrive at the back entrance to Camelot Castle, but there are \
         multiple gardeners armed with shears patrolling. There are a few \
         options for how could proceed. Would you like to:"
        when choice_num = 1 -> mystic_scenario (* change this one *)
      | "It took you some time to escape the maze. After escaping the maze, \
         you arrive at the back entrance to Camelot Castle, but there are \
         multiple gardeners armed with shears patrolling. There are a few \
         options for how could proceed. Would you like to:"
        when choice_num = 2 -> side_entrance_scenario
      | "It took you some time to escape the maze. After escaping the maze, \
         you arrive at the back entrance to Camelot Castle, but there are \
         multiple gardeners armed with shears patrolling. There are a few \
         options for how could proceed. Would you like to:"
        when choice_num = 3 -> market_scenario
      (* Third Option *)
      | "It took you a significant amount of time to escape the maze, it is \
         already nightfall! As you make your way out of the maze, the castle's \
         main guard sneak attacks you, beginning a battle. The guard begins to \
         over power you. How would you like to proceed?:"
        when choice_num = 1 -> after_fight_guard_in_garden_scenario
      | "It took you a significant amount of time to escape the maze, it is \
         already nightfall! As you make your way out of the maze, the castle's \
         main guard sneak attacks you, beginning a battle. The guard begins to \
         over power you. How would you like to proceed?:"
        when choice_num = 2 -> opposing_guards_scenario
      | "It took you some time to escape the maze. After escaping the maze, \
         you arrive at the back entrance to Camelot Castle, but there are \
         multiple gardeners armed with shears patrolling. There are a few \
         options for how could proceed. Would you like to:"
        when choice_num = 3 -> market_scenario
      | _ -> current_scenario
    in
    game_loop
      { new_state with days_survived = new_state.days_survived + 1 }
      next_scenario

let choose_initial_scenario maze_result =
  match maze_result with
  | Some (Success steps) ->
      if steps >= 1 && steps <= 3 then initial_scenario_one
      else if steps >= 4 && steps <= 5 then initial_scenario_two
      else if steps > 5 then initial_scenario_three
      else courtyard_scenario
  | _ -> courtyard_scenario

let () =
  let choice = choose_character () in
  let character = create_character choice in
  let initial_state = create_game_state character in
  let state_after_maze = play_intro_maze initial_state in
  game_loop state_after_maze
    (choose_initial_scenario state_after_maze.maze_result)
