let display_character_message choice =
  match choice with
  | 1 -> print_endline "You have chosen Kate the Knight!"
  | 2 -> print_endline "You have chosen Walter the Wizard!"
  | 3 -> print_endline "You have chosen Max the Monk!"
  | 4 -> print_endline "You have chosen Abigail the Archer!"
  | 5 -> print_endline "You have chosen Alan the Alchemist!"
  | _ -> print_endline "Invalid choice, please enter a number between 1 and 5."

let choose_character () =
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
  | Some choice -> display_character_message choice
  | None ->
      print_endline "Invalid input, please enter a number between 1 and 5."

let () = choose_character ()
