type character = {
  name : string;
  role : string;
  health : int;
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
        special_ability = "Swordsmanship";
      }
  | 2 ->
      {
        name = "Walter the Wizard";
        role = "Wizard";
        health = 100;
        special_ability = "Spell Casting";
      }
  | 3 ->
      {
        name = "Max the Monk";
        role = "Monk";
        health = 100;
        special_ability = "Healing";
      }
  | 4 ->
      {
        name = "Abigail the Archer";
        role = "Archer";
        health = 100;
        special_ability = "Speed";
      }
  | 5 ->
      {
        name = "Alan the Alchemist";
        role = "Alchemist";
        health = 100;
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

let create_scenario description choices = { description; choices }
let create_choice description consequence = { description; consequence }

let initial_scenario_one =
  create_scenario
    "You escape the maze quickly and are able to make it to the gates of \
     Camelot Castle. There are guards everywhere. There are a few options for \
     how can proceed. Would you like to:"
    [
      create_choice
        "1. Face the guards in battle and enter Camelot Castle through the \
         front entrance. ⚔️" (fun state ->
          if state.player.name = "Kate the Knight" then
            ( { state with current_location = "Camelot Castle" },
              "As a skilled Knight, you are able to fend off the guards and \
               enter through the castle's front gates. Well done Kate! ♞" )
          else if state.player.name = "Abigail the Archer" then
            ( { state with current_location = "Camelot Castle" },
              "As a skilled Archer, you are able to use your bow and arrow to \
               fend off the guards and enter through the castle's front gates. \
               Well done Abigail! 🏹" )
          else
            ( {
                state with
                player = { state.player with health = state.player.health - 50 };
                current_location = "Camelot Castle";
              },
              "Your character is not skilled in battle, you make it through \
               the front gates, but the guards do some serious damage to you! \
               You lose 50 health points. 🤕" ));
      create_choice
        "2. Go to the back entrance of Camelot Castle and try to enter the \
         castle there. 🪴" (fun state ->
          ( { state with current_location = "Camelot Castle Gardens" },
            "You begin walking around to the garden to see if you can enter \
             from there." ));
    ]

let garden_scenario =
  create_scenario
    "You arrive at the back entrance to Camelot Castle, but there are multiple \
     gardeners armed with shears patrolling. There are a few options for how \
     could proceed. Would you like to:"
    [
      create_choice
        "1. Attempt to sneak past the gardeners and enter the castle through \
         the back entrance 👨‍🌾" (fun state ->
          if state.player.name = "Max the Monk" then
            ( { state with current_location = "Camelot Castle" },
              "As a monk, your calming demeanor helps you to blend in with and \
               sneak past the guards, entering the castle through the back \
               entrance. 😌" )
          else if state.player.name = "Abigail the Archer" then
            ( { state with current_location = "Camelot Castle" },
              "As an archer, your stealth and agility allow you to sneak past \
               the gardeners quickly and quietly, entering the castle through \
               the backe entrance. 🏹" )
          else
            ( {
                state with
                player = { state.player with health = state.player.health - 50 };
                current_location = "Castle Gardens";
              },
              "You successfully sneak into the castle but get spotted and take \
               some damage. You lose 50 health points. 🤕" ));
      create_choice "2. Look for another entrance to the castle 👀" (fun state ->
          ( { state with current_location = "Camelot Castle" },
            "You search around and find a potential alternative route through \
             a side door to the castle and enter there unbothered." ));
    ]

let initial_scenario_two =
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
            ( { state with current_location = "Camelot Castle" },
              "As a wizard, you cast a spell on the guard, putting him to \
               sleep and winning the attack 😴" )
          else if state.player.name = "Alan the Alchemist" then
            ( { state with current_location = "Camelot Castle" },
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
      create_choice
        "2. Run towards the lake behind the Castle Gardens where you see smoke \
         from amongst the 👀" (fun state ->
          ( { state with current_location = "Castle Lakes" },
            "Because it's so dark outside you are not able to find an \
             alternative entrance to the castle. You decide to walk towards a \
             nearby lake where you see smoke from a fire." ));
    ]

let after_fight_front_entrance_scenario =
  create_scenario
    "You walk into the castle's front entrance but realize you look out of \
     place. You do not want people to know you are an intruder! How would you \
     like to proceed?:"
    [
      create_choice
        "1. Try to find some servant's clothes to blend in with the castle's \
         staff." (fun state ->
          ( { state with current_location = "Queen's Quarters" },
            "You find some butler's clothes and put them on. You begin walking \
             around the castle in disguise." ));
      create_choice
        "2. Go to the catacombs of the castle to navigate the castle through \
         secret tunnels. " (fun state ->
          ( { state with current_location = "Queen's Quarters" },
            "You go to the catacombs and begin walking around. Its dark and \
             you get lost." ));
    ]

let side_entrance_scenario =
  create_scenario
    "You begin wandering around the castle and opening random doors. You \
     accidentally enter the room where the King's wife, the queen, is drinking \
     tea surrounded by her guards. She is startled by you, but says that if \
     you entertain her she will let you proceed. What would you like to do? :"
    [
      create_choice
        "1. Tell the queen some jokes in an attempt to make her laugh."
        (fun state ->
          ( {
              state with
              player = { state.player with health = state.player.health - 20 };
              current_location = "Camelot Jail";
            },
            "The queen is not impressed by your jokes and tells the guards to \
             take you to the castle's jail. You lose 60 health points. 🤕" ));
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
              player = { state.player with health = state.player.health - 30 };
              current_location = "Castle Jail";
            },
            "The queen's guards beat you up really badly. You wake up in the \
             castle's jail. You lose 30 health points!!! 🤕" ));
    ]

let market_scenario =
  create_scenario
    "You enter the town farmer's market and a merchant offers you some bread \
     for 30 gold pieces. You do not have enough gold but need something to eat \
     to get your strength up. What would you like to do?"
    [
      create_choice
        "1. Bargain with the merchant and see if you can give him something in \
         addition to gold pieces for some food." (fun state ->
          if state.player.name = "Walter the Wizard" then
            ( {
                state with
                food = state.food + 20;
                player = { state.player with health = state.player.health + 20 };
                current_location = "Camelot Fortune Teller's Shop";
              },
              "As a wizard, you cast a spell on the merchant, and he gives you \
               the bread for free. Well done, you eat and gain 20 health \
               points! 🍞" )
          else if state.player.name = "Alan the Alchemist" then
            ( {
                state with
                food = state.food + 20;
                gold = state.gold - 20;
                player = { state.player with health = state.player.health + 20 };
                current_location = "Camelot Fortune Teller's Shop";
              },
              "As an alchemist, you give the merchant an elixir of life plus \
               your 20 gold pieces for the bread. Well done, you eat and gain \
               20 health points! 🍞" )
          else if state.player.name = "Abigail the Archer" then
            ( {
                state with
                food = state.food + 20;
                gold = state.gold - 20;
                player = { state.player with health = state.player.health + 20 };
                current_location = "Camelot Fortune Teller's Shop";
              },
              "As an archer, you offer the merchant bow and arrow lessons so \
               that he can protect himself from thieves along with your 20 \
               gold pieces. Well done, you eat and gain 20 health points! 🍞" )
          else
            ( {
                state with
                food = state.food + 10;
                gold = state.gold - 15;
                player = { state.player with health = state.player.health + 10 };
                current_location = "Camelot Fortune Teller's Shop";
              },
              "You do not have anything to offer the merchant. You take half \
               the bread for 15 gold pieces. Well done, you eat and gain 10 \
               health points! 🍞" ));
      create_choice "2. Go somewhere else" (fun state ->
          ( { state with current_location = "Camelot Fortune Teller's Shop" },
            "You leave the market to go somewhere else." ));
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
          if state.player.name = "Walter the Wizard" then
            ( {
                state with
                current_location = "Camelot Market";
                gold = state.gold + 5;
              },
              "As a wizard, you use magic to do an enchanting dance for the \
               princess. She is please and gives your 5 gold pieces to buy \
               yourself a treat with at the market." )
          else if state.player.name = "Max the Monk" then
            ( {
                state with
                current_location = "Camelot Market";
                gold = state.gold + 5;
              },
              "As a monk, you are able to do a calming dance for the princess \
               that makes her laugh. She is please and gives your 5 gold \
               pieces to buy yourself a treat with at the market." )
          else
            ( { state with current_location = "Camelot Market" },
              "The princess is not impressed by your dance, she has her guards \
               kick you out of the castle. You begin to walk to the town \
               market to find some food to eat." ));
      create_choice
        "2. Go back up the wall and leave to go to the town's market."
        (fun state ->
          ( { state with current_location = "Princess's Courtyard" },
            "You run back up the wall towards the town's center where the \
             market is located." ));
    ]

let opposing_guards_scenario =
  create_scenario
    "You walk to the nearby lake where you see smoke emerging. As you \
     approach, you see that it is a group of knights from the opposing Kingdom \
     of Cornwall, they have started a fire and camped out near the lake. They \
     do not notice you. What would you like to do?:"
    [
      create_choice
        "1. Leave and go to the market to buy some food and get your strength \
         up." (fun state ->
          ( { state with current_location = "Camelot Market" },
            "You walk to the market which is near the town's center." ));
      create_choice
        "2. Blend in with the crowd and walk with them as they go towards the \
         town center. " (fun state ->
          if state.player.name = "Kate the Knight" then
            ( { state with current_location = "Camelot Market" },
              "As a knight you are easily able to blend in with this crowd. \
               You walk with them as they go to the town farmer's market." )
          else
            ( {
                state with
                current_location = "Camelot Market";
                player = { state.player with health = state.player.health + 20 };
              },
              "You are not able to blend in very well with this crowd of \
               Knights since you look out of place. They notice you are an \
               intruder and you have to run away. You lose 10 health points." ));
    ]

let mystic_scenario =
  create_scenario
    "You walk to the nearby fortune teller's shop. In the shop you encounter a \
     mystic who says she has been expecting you. The mystic says she will show \
     you how to save Camelot."
    [
      create_choice
        "1. Please enter 1 to solve a word scrambling riddle that will unlock \
         the power to save Camelot" (fun state -> (state, ""));
    ]

let king_scenario =
  create_scenario
    "The queen is very pleased with your display of talent and calls in the \
     King of Camelot to meet you. You explain the situation to the king and he \
     tells you that he knows how to help you save the Kingdom of Camelot"
    [
      create_choice
        "1. Please enter 1 to solve a word scrambling riddle that will unlock \
         the power to save Camelot" (fun state -> (state, ""));
    ]

let jail_scenario =
  create_scenario
    "While sitting in the Castle jail cell recovering from the day, you notice \
     two other people. One is an old man who says he has lived in Camelot for \
     over 100 years. You explain your situation and how you are tring to save \
     Camelot to him and he says he wants to provide you with adviceo on how to \
     win."
    [
      create_choice
        "1. Please enter 1 to solve a word scrambling riddle that will unlock \
         the power to save Camelot" (fun state -> (state, ""));
    ]
