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

let create_character choice =
  match choice with
  | 1 ->
      {
        name = "Kate the Knight";
        role = "Knight";
        health = 100;
        strength = 85;
        special_ability = "Swordsmanship";
      }
  | 2 ->
      {
        name = "Walter the Wizard";
        role = "Wizard";
        health = 70;
        strength = 60;
        special_ability = "Spell Casting";
      }
  | 3 ->
      {
        name = "Max the Monk";
        role = "Monk";
        health = 80;
        strength = 40;
        special_ability = "Healing";
      }
  | 4 ->
      {
        name = "Abigail the Archer";
        role = "Archer";
        health = 70;
        strength = 50;
        special_ability = "Speed";
      }
  | 5 ->
      {
        name = "Alan the Alchemist";
        role = "Alchemist";
        health = 65;
        strength = 35;
        special_ability = "Potion Crafting";
      }
  | _ -> failwith "Invalid character choice"

let initial_location =
  {
    name = "Camelot Castle Gates";
    description = "The grand entrance to Camelot, now under siege.";
    danger_level = 3;
  }

let create_game_state character =
  {
    player = character;
    current_location = initial_location;
    days_survived = 0;
    food = 100;
    gold = 50;
  }

let display_game_status state =
  Printf.printf "\nStatus:\nHealth: %d\nFood: %d\nGold: %d\nDays Survived: %d\n"
    state.player.health state.food state.gold state.days_survived

let castle_gate_scenario =
  {
    description = "You stand before the castle gates. What will you do?";
    choices =
      [
        {
          description = "1. Try to sneak past the guards";
          consequence =
            (fun state ->
              ( {
                  state with
                  player =
                    { state.player with health = state.player.health - 10 };
                },
                "You attempt to sneak past but get spotted and take some \
                 damage." ));
        };
        {
          description = "2. Look for another entrance";
          consequence =
            (fun state ->
              ( state,
                "You search around and find a potential alternative route." ));
        };
      ];
  }

let courtyard_scenario =
  {
    description =
      "You've made it to the castle courtyard. Guards are patrolling.";
    choices =
      [
        {
          description = "1. Hide behind the market stalls";
          consequence =
            (fun state -> (state, "You successfully avoid detection."));
        };
        {
          description = "2. Try to blend in with the crowd";
          consequence =
            (fun state ->
              ( { state with food = state.food - 10 },
                "You manage to blend in but lose some supplies in the process."
              ));
        };
      ];
  }
