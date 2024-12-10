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
