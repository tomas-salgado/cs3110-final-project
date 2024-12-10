val words_bank : string list
(** [words_bank] is a string list of possible words to be scrambled and
    presented to the player to unscramble. *)

val pick_random_word : unit -> string
(** [pick_random_word ()] is a randomly chosen string from the word bank. *)

val shuffle : 'a list -> 'a list
(** [shuffle lst] shuffles the list [lst], puts the elements of [lst] in a
    random order. *)

val scramble_word : string -> string
(** [scramble_word word] is the string [word] with the letters rearranged in a
    random order. *)

val guess_loop : int -> string -> (unit -> string) -> bool
(** [guess_loop attempts_left original_word f] is a loop that continuosly prints
    results to screen as the user attempts to guess the scrambled word. The user
    has [attempts_left] attempts to guess the scrambled word. The word they must
    guess is [original_word]. *)

val play_word_scramble : unit -> bool
(** [play_word_scramble ()] initiates the word scramble game for the user at the
    very end of the choose your own adventure game. *)
