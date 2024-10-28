type character = {
  name : string;
  role : string;
  health : int;
  strength : int;
  special_ability : string;
}

type location = { name : string; description : string; danger_level : int }

type game_state = {
  player : character;
  current_location : location;
  days_survived : int;
  food : int;
  gold : int;
}

type choice = {
  description : string;
  consequence : game_state -> game_state * string;
}

type scenario = { description : string; choices : choice list }

val create_character : int -> character
val create_game_state : character -> game_state
val castle_gate_scenario : scenario
val courtyard_scenario : scenario
