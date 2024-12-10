type character = {
  name : string;
  role : string;
  health : int;
  special_ability : string;
}
(** [character] is a type representing a character that the user can choose
    within the game. This type stores information about the character's name,
    role, health, strength, and special ability. *)

type game_state = {
  player : character;
  current_location : string;
  food : int;
  gold : int;
  maze_result : Maze.maze_result option;
}
(** [game_state] is a type representing the state of the game which allows the
    the user to continuously update the state, storing information on the
    character, location, days survived, food, and gold amounts. *)

type choice = {
  description : string;
  consequence : game_state -> game_state * string;
}
(** [choice] is a type representing the choice that the user makes in the
    terminal for how to proceed with the game including description of the
    choice and consequence or outcome for what will happen if they make that
    choice. *)

type scenario = {
  description : string;
  choices : choice list;
}
(** [scenario] is a type representing a scenario that would be displayed to the
    user through the terminal for the user to then go and choose a choice from
    regarding that scenario. It stores a description of the scenario and the
    choices the user can make regarding it. *)

val create_character : int -> character
(** [create_character choice] creates a character for the user based on the
    [choice] they input through the terminal from a set of given options. *)

val create_game_state : character -> game_state
(** [create_game_state character] initializes a new game state with preset
    conditions at the beginning of the game for the user. *)

val initial_scenario_one : scenario
(** [initial_scenario_one] is one of two options for a first scenario displayed
    to the user to choose options from. *)

val initial_scenario_two : scenario
(** [initial_scenario_two] is one of two options for a first scenario displayed
    to the user to choose options from. *)

val after_fight_front_entrance_scenario : scenario
(** [after_fight_front_entrance_scenario] is a scenario that could possibly be
    displayed to the user involving walking throughout the Castle's front
    entrance. *)

val side_entrance_scenario : scenario
(** [side_entrance_scenario] is a scenario that could possibly be displayed to
    the user involving sneaking into the Castle through a side entrance. *)

val opposing_guards_scenario : scenario
(** [opposing_guards_scenario] is a scenario that could possibly be displayed to
    the user involving interacting with knights from the battling kingdom. *)

val after_fight_guard_in_garden_scenario : scenario
(** [after_fight_guard_in_garden_scenario] is a scenario that could possibly be
    displayed to the user involving interacting with castle's princess. *)

val king_scenario : scenario
(** [after_fight_guard_in_garden_scenario] is a scenario that could possibly be
    displayed to the user that involves interacting with Camelot's King and
    invokes the word scramble game, causing the player to either win or lose.*)

val jail_scenario : scenario
(** [jail_scenario] is a scenario that could possibly be displayed to the user
    that involves going to the Camelot Castle jail and invokes the word scramble
    game, causing the player to either win or lose.*)

val garden_scenario : scenario
(** [garden_scenario] is a scenario that could possibly be displayed to the user
    that involves going to the Camelot Castle gardens. *)

val market_scenario : scenario
(** [market_scenario] is a scenario involving going to the farmers market
    displayed to the user to choose options from. *)

val mystic_scenario : scenario
(** [mystic_scenario] is a scenario involving encountering a mystic displayed to
    the user to choose options form. *)
