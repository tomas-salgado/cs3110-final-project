(** [maze_result] represents the result of the maze game. The result is either
    [Success of int] storing the integer number of steps it took the user to
    complete the maze game, or [Failure]. *)
type maze_result =
  | Success of int
  | Failure

val initialize_maze : unit -> string array array
(** [initialize_maze ()] initializes a 9x9 maze as an array of array of strings
    for the user to navigate initially. The user starts in the very top left
    corner of the maze and must navigate through obstacles to the bottom right
    corner of the maze where the exit is located. *)

val is_exit_reached : string array array -> int * int -> bool
(** [is_exit_reached maze (x, y)] returns true if the position [(x,y)] of the
    user within the maze, [maze] is the exit position in the very bottom right
    corner, and false otherwise. *)

val print_maze : string array array -> int * int -> unit
(** [print_maze maze player_position] prints the maze [maze] to the player's
    screen each time a move is made so they can see their updated position,
    [player_position], with respect to the maze they are navigating. *)

val move_player : string array array -> int * int -> char -> int * int
(** [move_player maze player_position direction] moves the player on the screen
    based on the input keys of WASD that the user would provide. Based on the
    [direction] (WASD) they choose, the player is moved from their current
    position [player_position] to a new position within the overall maze,
    [maze]. *)

val get_final_score : maze_result -> int option
(** [get_final_score result] returns an option representing the final score of
    the [result] or final score of the user within the maze game. *)
