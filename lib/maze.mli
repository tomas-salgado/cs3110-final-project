(** Represents the result of the maze game. *)
type maze_result =
  | Success of int
  | Failure

val play_maze : unit -> maze_result
(** Starts the maze game in the terminal. *)
