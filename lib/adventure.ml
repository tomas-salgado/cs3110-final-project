type character = {
  name : string;
  role : string;
  health : int;
  strength : int;
  special_ability : string;
}

type game_state = {
  player : character;
  current_location : string;
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

let initial_location = "Camelot Castle Garden Maze"

let create_game_state character =
  {
    player = character;
    current_location = initial_location;
    food = 100;
    gold = 20;
    maze_result = None;
  }

let display_game_status state =
  Printf.printf "\nStatus:\nHealth: %d\nFood: %d\nGold: %d\n"
    state.player.health state.food state.gold

let create_scenario description choices = { description; choices }
let create_choice description consequence = { description; consequence }

let initial_scenario_one =
  create_scenario
    "You escape the maze quickly and are able to make it to the gates of \
     Camelot Castle. There are guards everywhere. There are a few options for \
     how can proceed. Would you like to:"
    [
      create_choice
        "1. Face the guards and enter Camelot Castle through the gates ⚔️"
        (fun state ->
          if state.player.name = "Kate the Knight" then
            ( { state with current_location = "Castle Gardens" },
              "As a skilled Knight, you are able to fend off the guards and \
               enter through the castle's front gates. Well done Kate! ♞" )
          else if state.player.name = "Abigail the Archer" then
            ( { state with current_location = "Castle Gardens" },
              "As a skilled Archer, you are able to use your bow and arrow to \
               fend off the guards and enter through the castle's front gates. \
               Well done Abigail! 🏹" )
          else
            ( {
                state with
                player = { state.player with health = state.player.health - 30 };
                current_location = "Castle Gardens";
              },
              "You make it through the front gates, but the guards do some \
               serious damage to you! You lose 30 health points. 🤕" ));
      create_choice "2. Look for another entrance to the castle 👀" (fun state ->
          ( state,
            "You search around and find a potential alternative route through \
             a side door to the castle." ));
      create_choice
        "3. Leave and go to the town center where the market is located 🛒"
        (fun state ->
          (state, "You search around and find the town farmer's market."));
    ]

let initial_scenario_two =
  create_scenario
    "It took you some time to escape the maze. After escaping the maze, you \
     arrive at the back entrance to Camelot Castle, but there are multiple \
     gardeners armed with shears patrolling. There are a few options for how \
     could proceed. Would you like to:"
    [
      create_choice
        "1. Attempt to sneak past the gardeners and enter the castle through \
         the back entrance 👨‍🌾" (fun state ->
          if state.player.name = "Max the Monk" then
            ( { state with current_location = "Castle Gardens" },
              "As a monk, your calming demeanor helps you to blend in with and \
               sneak past the guards through the back entrance. 😌" )
          else
            ( {
                state with
                player = { state.player with health = state.player.health - 30 };
                current_location = "Castle Gardens";
              },
              "You attempt to sneak past but get spotted and take some damage. \
               You lose 30 health points. 🤕" ));
      create_choice "2. Look for another entrance to the castle 👀" (fun state ->
          ( { state with current_location = "Castle Gardens" },
            "You search around and find a potential alternative route through \
             a side door to the castle." ));
      create_choice "3. Go to the town center where the market is located 🛒"
        (fun state ->
          (state, "You search around and find the town farmer's market."));
    ]

let initial_scenario_three =
  create_scenario
    "It took you a significant amount of time to escape the maze, it is \
     already nightfall! As you make your way out of the maze, the castle's \
     main guard sneak attacks you, beginning a battle. The guard begins to \
     over power you. How would you like to proceed?:"
    [
      create_choice
        "1. Continue fighting the guard in hopes of winning the battle. ⚔️"
        (fun state ->
          if state.player.name = "Walter the Wizard" then
            ( { state with current_location = "Castle Gardens" },
              "As a wizard, you cast a spell on the guard, putting him to \
               sleep and winning the attack 😴" )
          else if state.player.name = "Alan the Alchemist" then
            ( { state with current_location = "Castle Gardens" },
              "The guard nearly takes you out, but as an alchemist you use an \
               elixir of life to resuscitate yourself. 🧪" )
          else
            ( {
                state with
                player = { state.player with health = state.player.health - 60 };
                current_location = "Castle Gardens";
              },
              "The guard overpowers you, you win but nearly lose your life in \
               the process. You lose 60 health points. 🤕" ));
      create_choice "2. Look for another entrance to the castle 👀" (fun state ->
          ( { state with current_location = "Castle Gardens" },
            "Because it's so dark outside you are not able to find an \
             alternative entrance to the castle. You decide to walk towards a \
             nearby lake where you see smoke from a fire." ));
      create_choice "3. Go to the town center where the market is located 🛒"
        (fun state ->
          ( { state with current_location = "Castle Gardens" },
            "You search around and find the town farmer's market." ));
    ]

let after_fight_front_entrance_scenario =
  create_scenario
    "You walk into the castle but realize you look out of place. You do not \
     want people to know you are an intruder! How would you like to proceed?:"
    [
      create_choice
        "1. Try to find some servant's clothes to blend in with the castle's \
         staff." (fun state ->
          ( { state with current_location = "Camelot Castle, Grand Entrance" },
            "You find some butler's clothes and put them on. You begin walking \
             around the castle in disguise." ));
      create_choice
        "2. Go to the catacombs of the castle to navigate the castle through \
         secret tunnels. " (fun state ->
          ( { state with current_location = "Camelot Castle, Grand Entrance" },
            "You go to the catacombs and begin walking around. Its dark and \
             cold and you get lost." ));
    ]

let side_entrance_scenario =
  create_scenario
    "You enter the castle through an unlocked door within the attached \
     greenhouse. You enter immediately into the living room where the King's \
     wife, the queen, is drinking tea surrounded by her guards. She is \
     startled by you, but says that if you entertain her she will let you \
     proceed. What would you like to do? :"
    [
      create_choice
        "1. Tell the queen some jokes in an attempt to make her laugh."
        (fun state ->
          ( {
              state with
              player = { state.player with health = state.player.health - 20 };
              current_location = "Queen's Quarters";
            },
            "The queen is not impressed by your jokes and tells the guards to \
             throw you out. You lose 60 health points. 🤕" ));
      create_choice
        "2. Show the queen a special talent and hope this satifies her."
        (fun state ->
          if state.player.name = "Walter the Wizard" then
            ( { state with current_location = "Queen's Quarters" },
              "As a wizard, you cast a spell that causes flowers petals to \
               begin falling from the sky. The queen is impressed and lets you \
               proceed." )
          else if state.player.name = "Alan the Alchemist" then
            ( { state with current_location = "Queen's Quarters" },
              "As an alchemist, you mix a potion using the Queen's teas to \
               create a potion of youth. The queen is impressed and lets you \
               proceed." )
          else if state.player.name = "Kate the Knight" then
            ( { state with current_location = "Queen's Quarters" },
              "As a knight, you begin jousting with one of the queen's guards \
               and win the duel. The queen is impressed by such a well fought \
               battle and lets you proceed." )
          else if state.player.name = "Max the Monk" then
            ( { state with current_location = "Queen's Quarters" },
              "As a monk you show the queen the meaning of peace and hapiness. \
               She is very please with this and lets you proceed." )
          else
            ( { state with current_location = "Queen's Quarters" },
              "As an archer you show your crossbow skills to the queen and she \
               is impressed. She lets you proceed." ));
      create_choice
        "3. Ignore the queen's wishes and attempt to run past her to the doors \
         on the other side of the room." (fun state ->
          ( {
              state with
              player = { state.player with health = state.player.health - 90 };
              current_location = "Queen's Quarters";
            },
            "The queen's guards beat you up really badly. You wake up in the \
             castle's jail. You lose 90 health points!!! 🤕" ));
    ]

let market_scenario =
  create_scenario
    "You entered the town farmer's market. What would you like to do?"
    [
      create_choice "1. Buy some food" (fun state ->
          ( {
              state with
              food = state.food + 20;
              gold = state.gold - 10;
              player = { state.player with health = state.player.health + 20 };
              current_location = "Camelot Farmer's Market";
            },
            "You bought some bread and vegetables for 10 pieces of gold. You \
             eat the food and gain 20 health points! 🍞" ));
      create_choice "2. Go somewhere else" (fun state ->
          (state, "You leave the market to go somewhere else."));
    ]

let after_fight_guard_in_garden_scenario =
  create_scenario
    "After battling the guard and barely escaping, you decide to scale and \
     climb a shorter part of the castle wall in order to get in. You climb \
     into the castle's courtyard. As you are passing through the courtyard you \
     encounter the castle's princess who is starteled and starts to yell for \
     help. What would you like to do next?"
    [
      create_choice "1. Do a dance for the princess to make her less afraid."
        (fun state ->
          ( {
              state with
              player = { state.player with health = state.player.health - 25 };
              current_location = "Princess's Courtyard";
            },
            "You do a dance for the princess but she is not impressed. She \
             knows how to fight and kicks you while you run away. You lose 25 \
             health points. 🤕" ));
      create_choice "2. Just run away and hope no one catches you."
        (fun state ->
          ( { state with current_location = "Princess's Courtyard" },
            "You  run immedaitley and no one catches you luckily. Relieved, \
             you are now within the castle." ));
    ]

let wizard_fight_scenario =
  create_scenario
    "Once you get past the gardeners and enter the castle, there is only one \
     entrance to go through, it is a long spiraling staircase leading to the \
     castle's tallest tower. You climb the stairs and arrive at the top of the \
     tower. The castle's wizard is there and he challenge's you to a duel. \
     What would you like to do?:"
    [
      create_choice "1. Accept the Wizard's challenge and duel." (fun state ->
          if state.player.name = "Walter the Wizard" then
            ( { state with current_location = "Wizard's Tower" },
              "Walter's strength in Wizardy prevails, and he wins the duel \
               unscathed!" )
          else
            ( {
                state with
                player = { state.player with health = 0 };
                current_location = "Wizard's Tower";
              },
              "You don't know magic, the wizard does some serious damage and \
               you end up losing the duel!" ));
      create_choice "2. Run back down the stairs away from the wizard."
        (fun state ->
          ( {
              state with
              food = state.food - 20;
              current_location = "Wizard's Tower";
            },
            "You leave but trip and fall down the long staircase, injuring \
             yourself. You lose 20 health points. 🤕" ));
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

let opposing_guards_scenario =
  create_scenario
    "You walk to the nearby lake where you see smoke emerging. As you \
     approach, you see that it is a group of knights from the opposing Kingdom \
     of Cornwall, they have started a fire and camped out near the lake. They \
     do not notice you. What would you like to do?:"
    [
      create_choice
        "1. See if you can blend in with the knights from Cornwall to find out \
         their battle secrets. " (fun state ->
          ( {
              state with
              player = { state.player with health = 120 };
              current_location = "Camelot Lake";
            },
            "You have saved Camelot and the lives of millions of people." ));
      create_choice
        "2. Leave and go for a swim in the lake to wash yourself off after a \
         long day." (fun state ->
          ( {
              state with
              player = { state.player with health = 0 };
              current_location = "Camelot Lake";
            },
            "Your decisions are not well informed and you cause the downfall \
             of yourself and Camelot as a whole." ));
      create_choice "3. Battle these knights and see if you can defeat them. "
        (fun state ->
          ( {
              state with
              player = { state.player with health = 0 };
              current_location = "Camelot Lake";
            },
            "Your decisions are not well informed and you cause the downfall \
             of yourself and Camelot as a whole." ));
    ]

let mystic_scenario =
  create_scenario
    "You walk to the nearby fortune teller's shop. In the shop you encounter a \
     mystic who says she has been expecting you. The mystic says she will show \
     you how to save Camelot. What do you want to do? "
    [
      create_choice "1. See what the mystic has to say and try to save Camelot."
        (fun state ->
          ( {
              state with
              player = { state.player with health = 120 };
              current_location = "Fortune Teller's Shop";
            },
            "You have saved Camelot and the lives of millions of people." ));
      create_choice "2. Don't take the Mystic's advice, run away." (fun state ->
          ( {
              state with
              player = { state.player with health = 0 };
              current_location = "Fortune Teller's Shop";
            },
            "Your decisions are not well informed and you cause the downfall \
             of yourself and Camelot as a whole." ));
    ]
