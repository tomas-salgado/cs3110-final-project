open Cs3110finalproject.Adventure
open Cs3110finalproject.Maze
open Cs3110finalproject.Wordscramble

(** [guess_loop attempts_left original_word f] is a loop that continuosly prints
    results to screen as the user attempts to guess the scrambled word. The user
    has [attempts_left] attempts to guess the scrambled word. The word they must
    guess is [original_word]. *)
let rec guess_loop attempts_left original_word f =
  if attempts_left = 0 then (
    Printf.printf "You've run out of guesses! The word was: %s.\n" original_word;
    false)
  else (
    Printf.printf "Enter your guess (%d attempt(s) left): " attempts_left;
    let guess = f () in
    if String.lowercase_ascii guess = original_word then (
      Printf.printf "Congratulations! You unscrambled the word!\n";
      true)
    else (
      Printf.printf "Incorrect guess.\n";
      guess_loop (attempts_left - 1) original_word f))

(** [play_word_scramble ()] initiates the word scramble game for the user at the
    very end of the choose your own adventure game. *)
let play_word_scramble () =
  let original_word = pick_random_word () in
  let scrambled_word = scramble_word original_word in
  Printf.printf "Unscramble this word: %s\n" scrambled_word;
  guess_loop 3 original_word read_line

(** [game_loop maze player_position steps] creates a loop that continuously
    prints and updates the maze [maze] by changing the player's position
    [player_position], recording the amount of steps [steps] that it takes the
    player to successfully complete the maze. *)
let rec game_loop maze player_position steps =
  print_newline ();
  print_maze maze player_position;
  if is_exit_reached maze player_position then Success steps
  else (
    print_endline "Enter a direction (W: Up, A: Left, S: Down, D: Right): ";
    let direction = read_line () in
    match direction with
    | "w" | "a" | "s" | "d" ->
        let new_position =
          move_player maze player_position (String.get direction 0)
        in
        game_loop maze new_position (steps + 1)
    | _ ->
        print_endline "Invalid input! Please enter w, a, s, or d.";
        game_loop maze player_position steps)

(** [play_maze ()] intializes a maze and game loop for the user at the start of
    the choose your own adventure. Casues the player to spawn at the very upper
    left corner and begin from there. *)
let play_maze () =
  let maze = initialize_maze () in
  game_loop maze (0, 0) 0

(** [play_intro_maze state] prints and allows the user to play the initial maze
    game at the start of the choose your own adventure, updating the [state]
    variable with the resutling number of steps it takes the user to complete
    the maze. *)
let play_intro_maze state =
  print_endline
    "You awake in the Camelot Castle gardens at the very corner of a maze. You \
     must navigate and escape the maze in order to escape and save Camelot \
     from attack.";
  let maze_result = play_maze () in
  match get_final_score maze_result with
  | Some steps ->
      print_endline
        ("Congratulations! You completed the maze in " ^ string_of_int steps
       ^ " steps.");
      { state with maze_result = Some (Success steps) }
  | None ->
      print_endline
        "You failed to escape the maze. You start your journey injured.";
      {
        state with
        player = { state.player with health = state.player.health - 10 };
        maze_result = Some Failure;
      }

(** [play_word_scramble_game state] prints and allows the user to play the
    ending word scramble game at the start of the choose your own adventure,
    updating the [state] variable to cause the user to win or lose the game
    overall. *)
let play_word_scramble_game state =
  print_endline
    "You have one final challenge: a magical word scramble that determines the \
     fate of Camelot!";
  let result = play_word_scramble () in
  match result with
  | true -> { state with player = { state.player with health = 120 } }
  | false -> { state with player = { state.player with health = 0 } }

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
  print_endline "";
  print_endline ("Health: " ^ string_of_int state.player.health);
  print_endline ("Location: " ^ state.current_location);
  print_endline ("Food: " ^ string_of_int state.food);
  print_endline ("Gold: " ^ string_of_int state.gold);
  print_endline ""

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
  else if
    current_scenario.description
    = "You walk to the nearby fortune teller's shop. In the shop you encounter \
       a mystic who says she has been expecting you. The mystic says she will \
       show you how to save Camelot."
    || current_scenario.description
       = "The queen is very pleased with your display of talent and calls in \
          the King of Camelot to meet you. You explain the situation to the \
          king and he tells you that he knows how to help you save the Kingdom \
          of Camelot"
    || current_scenario.description
       = "You dress as a butler and another servant hands you a platter and \
          points you to a room you need to take it. As you enter the room you \
          realize you are serving the king. You break cover and explain your \
          situation to the King. He tells you that he knows how to help you \
          save the Kingdom of Camelot "
    || current_scenario.description
       = "While sitting in the Castle jail cell recovering from the day, you \
          notice two other people. One is an old man who says he has lived in \
          Camelot for over 100 years. You explain your situation and how you \
          are tring to save Camelot to him and he says he wants to provide you \
          with adviceo on how to win."
    || current_scenario.description
       = "The wizard then tells you he knows how to help you end this war."
    || current_scenario.description
       = "The king then tells you he knows how to help you end this war."
    || current_scenario.description
       = "You begin to battle the opposing side but in order to win you must \
          solve this puzzle."
    || current_scenario.description
       = "A mystic walks out the lake as you approach for a swim and tells you \
          she can help you save camelot."
  then
    let final_state = play_word_scramble_game new_state in
    if final_state.player.health = 120 then
      print_endline
        "You successfully unscrambled the word! Camelot is saved. You are a \
         hero!"
    else
      print_endline
        "You failed to unscramble the word. Camelot has fallen. Better luck \
         next time."
  else
    let next_scenario =
      match current_scenario.description with
      | "You escape the maze quickly and are able to make it to the gates of \
         Camelot Castle. There are guards everywhere. There are a few options \
         for how can proceed. Would you like to:"
        when choice_num = 1 -> after_fight_front_entrance_scenario
      | "You escape the maze quickly and are able to make it to the gates of \
         Camelot Castle. There are guards everywhere. There are a few options \
         for how can proceed. Would you like to:"
        when choice_num = 2 -> garden_scenario
      | "You walk into the castle's front entrance but realize you look out of \
         place. You do not want people to know you are an intruder! How would \
         you like to proceed?:"
        when choice_num = 1 || choice_num = 2 -> side_entrance_scenario
      | "You arrive at the back entrance to Camelot Castle, but there are \
         multiple gardeners armed with shears patrolling. There are a few \
         options for how could proceed. Would you like to:"
        when choice_num = 1 || choice_num = 2 -> side_entrance_scenario
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
      | "You walk to the nearby lake where you see smoke emerging. As you \
         approach, you see that it is a group of knights from the opposing \
         Kingdom of Cornwall, they have started a fire and camped out near the \
         lake. They do not notice you. What would you like to do?:"
        when choice_num = 1 || choice_num = 2 -> market_scenario
      | "After battling the guard and barely escaping, you decide to scale and \
         climb a shorter part of the castle wall in order to get in. You climb \
         into the castle's courtyard. As you are passing through the courtyard \
         you encounter the castle's princess who is starteled and starts to \
         yell for help. What would you like to do next?"
        when choice_num = 1 || choice_num = 2 -> market_scenario
      | "You enter the town farmer's market and a merchant offers you some \
         bread for 30 gold pieces. You do not have enough gold but need \
         something to eat to get your strength up. What would you like to do?"
        when choice_num = 1 -> mystic_scenario
      | "You enter the town farmer's market and a merchant offers you some \
         bread for 30 gold pieces. You do not have enough gold but need \
         something to eat to get your strength up. What would you like to do?"
        when choice_num = 2 -> mystic_scenario
      | "You begin wandering around the castle and opening random doors. You \
         accidentally enter the room where the King's wife, the queen, is \
         drinking tea surrounded by her guards. She is startled by you, but \
         says that if you entertain her she will let you proceed. What would \
         you like to do? :"
        when choice_num = 2 -> king_scenario
      | "You begin wandering around the castle and opening random doors. You \
         accidentally enter the room where the King's wife, the queen, is \
         drinking tea surrounded by her guards. She is startled by you, but \
         says that if you entertain her she will let you proceed. What would \
         you like to do? :"
        when choice_num = 1 || choice_num = 3 -> jail_scenario
      | _ -> current_scenario
    in
    game_loop new_state next_scenario

(** [choose_initial_scenario maze_result] chooses which initial scenario will be
    displayed to the player of the choose your own adventure game based on their
    score in their score on the initial maze game which is stored in
    [maze_result]. *)
let choose_initial_scenario maze_result =
  match maze_result with
  | Some (Success steps) ->
      if steps < 20 then initial_scenario_one
      else if steps >= 20 then initial_scenario_two
      else mystic_scenario
  | _ -> mystic_scenario

let () =
  let choice = choose_character () in
  let character = create_character choice in
  let initial_state = create_game_state character in
  let state_after_maze = play_intro_maze initial_state in
  game_loop state_after_maze
    (choose_initial_scenario state_after_maze.maze_result)
