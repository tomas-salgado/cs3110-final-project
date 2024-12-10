let words_bank =
  [
    "adventure";
    "castle";
    "wizard";
    "knight";
    "maze";
    "dragon";
    "garden";
    "potion";
    "quest";
    "battle";
    "sword";
    "shield";
    "kingdom";
    "courage";
    "victory";
    "treasure";
    "princess";
    "spell";
    "alchemy";
    "hero";
    "villain";
    "challenge";
    "camelot";
    "magic";
    "armor";
  ]

let () = Random.self_init ()

let shuffle lst =
  let indexed = List.mapi (fun i x -> (Random.int 1000, x)) lst in
  List.map snd (List.sort compare indexed)

let pick_random_word () =
  let len = List.length words_bank in
  let index = Random.int len in
  List.nth words_bank index

let scramble_word word =
  let char_list = List.init (String.length word) (String.get word) in
  let shuffled = shuffle char_list in
  String.concat "" (List.map (String.make 1) shuffled)

let rec guess_loop attempts_left original_word =
  if attempts_left = 0 then (
    Printf.printf "You've run out of guesses! The word was: %s.\n" original_word;
    false)
  else (
    Printf.printf "Enter your guess (%d attempt(s) left): " attempts_left;
    let guess = read_line () in
    if String.lowercase_ascii guess = original_word then (
      Printf.printf "Congratulations! You unscrambled the word!\n";
      true)
    else (
      Printf.printf "Incorrect guess.\n";
      guess_loop (attempts_left - 1) original_word))

let play_word_scramble () =
  let original_word = pick_random_word () in
  let scrambled_word = scramble_word original_word in
  Printf.printf "Unscramble this word: %s\n" scrambled_word;
  guess_loop 3 original_word
