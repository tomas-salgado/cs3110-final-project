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
    completely random order. *)

val guess_loop : int -> string -> bool
(** [guess_loop attempts_left original_word] creates a loop to allow the user to
    play and interact with the word scramble game through the terminal. The user
    has [attempts_left] to solve the word unscramble with the unscrambled
    version of the word the user must guess being [original_word] *)

val play_word_scramble : unit -> bool
(** [play_word_scramble ()] initiates the word scramble game for the user at the
    very end of the choose your own adventure game. *)
