(** Represents the result of the maze game. *)
type maze_result =
  | Success of int
  | Failure

val initialize_maze : unit -> string array array
val is_exit_reached : string array array -> int * int -> bool
val print_maze : string array array -> int * int -> unit
val move_player : string array array -> int * int -> char -> int * int
val game_loop : string array array -> int * int -> int -> maze_result
val play_maze : unit -> maze_result
val get_final_score : maze_result -> int option
