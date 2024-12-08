type character = {
  name : string;
  role : string;
  health : int;
  strength : int;
  special_ability : string;
}
(** [character] is a type representing a character that the user can choose
    within the game. This type stores information about the character's name,
    role, health, strength, and special ability. *)

type location = {
  name : string;
  description : string;
  danger_level : int;
}
(** [location] is a type representing the location of a character within the
    game including the name of the location, location description, and danger
    level of this location. *)

type game_state = {
  player : character;
  current_location : location;
  days_survived : int;
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

val initial_scenario : scenario
(** [initial_scenario] is the first scenario displayed to the user to choose
    options from. *)

val courtyard_scenario : scenario
(** [courtyard_scenario] is a scenario involving the castle courtyard displayed
    to the user to choose options from. *)

val wizard_fight_scenario : scenario
(** [wizard_fight_scenario] is a scenario involving fighting a wizard that is
    displayed to the user to choose options from. *)

val market_scenario : scenario
(** [market_scenario] is a scenario involving going to the farmers market
    displayed to the user to choose options from. *)

val mystic_scenario : scenario
(** [mystic_scenario] is a scenario involving encountering a mystic displayed to
    the user to choose options form. *)

val sneak_in_scenario : scenario
(** [sneak_in_scenario] is a scenario involving sneaking into the castle
    displayed to the user to choose options form. *)
