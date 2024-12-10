val words_bank : string list
(** [words_bank] is a string list of possible words to be scrambled and
    presented to the player to unscramble. *)

val pick_random_word : unit -> string
(** [pick_random_word ()] is a randomly chosen string from the word bank. *)

val shuffle : 'a list -> 'a list
(** [shuffle lst] shuffles the list [lst], puts the elements of [lst] in a
    random order. *)

val scramble_word : string -> string
val guess_loop : int -> string -> (unit -> string) -> bool

val play_word_scramble : unit -> bool
(** [play_word_scramble ()] initiates the word scramble game for the user at the
    very end of the choose your own adventure game. *)
