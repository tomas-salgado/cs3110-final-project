type character = {
  name : string;
  role : string;
  health : int;
  strength : int;
  special_ability : string;
}

type location = {
  name : string;
  description : string;
  danger_level : int;
}

type game_state = {
  player : character;
  current_location : location;
  days_survived : int;
  food : int;
  gold : int;
  maze_result : Maze.maze_result option;
}

type choice = {
  description : string;
  consequence : game_state -> game_state * string;
}

type scenario = {
  description : string;
  choices : choice list;
}

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
    maze_result = None;
  }

let display_game_status state =
  Printf.printf "\nStatus:\nHealth: %d\nFood: %d\nGold: %d\nDays Survived: %d\n"
    state.player.health state.food state.gold state.days_survived

let create_scenario description choices = { description; choices }
let create_choice description consequence = { description; consequence }

let initial_scenario =
  create_scenario
    "The battle has just broken out and you arrive at the gates of Camelot \
     Castle. There are a few options for how could proceed. Would you like \
     to: "
    [
      create_choice "1. Enter King Arthur's Castle through the gates"
        (fun state ->
          ( {
              state with
              player = { state.player with health = state.player.health - 30 };
            },
            "You attempt to sneak past but get spotted and take some damage." ));
      create_choice "2. Look for another entrance to the castle" (fun state ->
          (state, "You search around and find a potential alternative route."));
      create_choice "3. Go to the town center where the market is located"
        (fun state ->
          (state, "You search around and find the town farmer's market."));
    ]

let courtyard_scenario =
  create_scenario
    "You walk over to the castle courtyard, but guards are patrolling. What \
     would you like to do?"
    [
      create_choice
        "1. Fight the guards and attempt to enter the castle through the \
         courtyard" (fun state ->
          ( { state with player = { state.player with health = 0 } },
            "The guards overpower you and win the fight, you lose your life in \
             the process." ));
      create_choice "2. Sneak your way into the castle" (fun state ->
          ( { state with food = state.food - 10 },
            "You manage to blend in but lose some supplies in the process." ));
    ]

let wizard_fight_scenario =
  create_scenario
    "The castle's Wizard appears and challenges you to a magical duel in order \
     to pass through the castle's gates. How would you like to proceed?"
    [
      create_choice "1. Accept the Wizard's challenge and duel. " (fun state ->
          if state.player.name = "Walter the Wizard" then
            ( state,
              "Walter's strength in Wizardy prevails, and he wins the duel \
               unscathed!" )
          else
            ( { state with player = { state.player with health = 0 } },
              "You don't know magic, the wizard does some serious damage and \
               you end up losing the duel!" ));
      create_choice "2. Run away from the wizard and go somewhere else."
        (fun state ->
          ( { state with food = state.food - 10 },
            "You leave but trip and fall over a tree trunk on your way out." ));
    ]

let market_scenario =
  create_scenario
    "You entered the town farmer's market. What would you like to do?"
    [
      create_choice "1. Buy some food" (fun state ->
          ( { state with food = state.food + 20; gold = state.gold - 10 },
            "You bought some vegetables for 10 pieces of gold" ));
      create_choice "2. Go somewhere else" (fun state ->
          (state, "You left the market to explore the rest of town"));
      create_choice "3. Try to steal food" (fun state ->
          ( { state with player = { state.player with health = 0 } },
            "You managed to sneak away with a piece of fruit, but the fruit \
             vendor caught up with you and ended your life." ));
    ]

let mystic_scenario =
  create_scenario
    "You walk to the nearby lake instead and encounter a mystic with magical \
     abilities upon arriving. The mystic tells you the secrets to ending this \
     war and saving Camelot. Will you use these secrets to end the battle for \
     good?"
    [
      create_choice
        "1. Take the mystic's advice back to Camelot, ending the war"
        (fun state ->
          ( { state with player = { state.player with health = 120 } },
            "You have saved Camelot and the lives of millions of people." ));
      create_choice
        "2. Don't take the Mystic's advice, run away and do what you think is \
         best for Camelot." (fun state ->
          ( { state with player = { state.player with health = 0 } },
            "Your decisions are not well informed and you cause the downfall \
             of yourself and Camelot as a whole." ));
    ]

let sneak_in_scenario =
  create_scenario
    "You are now in the castle and encounter King Arthur himself as you pass \
     through the halls. He stops you and asks who you are, you explain you are \
     a towns-person who wants to help end the battle. King Arthur decides to \
     give you his magic sword to help defeat the enemies. What do you do \
     next? "
    [
      create_choice "1. The the magic sword and use it to save Camelot "
        (fun state ->
          ( { state with player = { state.player with health = 120 } },
            "You have saved Camelot and the lives of millions of people." ));
      create_choice "2. Run away out of the fright of the power of the sword. "
        (fun state ->
          ( { state with player = { state.player with health = 0 } },
            "King Arthur is not impressed by your cowardliness and strikes you \
             down himself with the magic sword, ending your life. " ));
    ]
