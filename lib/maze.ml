type maze_result =
  | Success of int
  | Failure

(* A terminal-based maze game (can be changed in the future). *)
let play_maze () =
  let maze =
    [|
      [| 'S'; ' '; ' '; ' '; 'X' |];
      [| ' '; 'X'; ' '; 'X'; ' ' |];
      [| ' '; ' '; ' '; ' '; ' ' |];
      [| 'X'; 'X'; ' '; 'X'; ' ' |];
      [| ' '; ' '; ' '; ' '; 'E' |];
    |]
  in
  let player_position = ref (0, 0) in
  let is_exit_reached () =
    let x, y = !player_position in
    maze.(x).(y) = 'E'
  in
  let print_maze () =
    for i = 0 to Array.length maze - 1 do
      for j = 0 to Array.length maze.(i) - 1 do
        if (i, j) = !player_position then print_char 'P'
        else print_char maze.(i).(j);
        print_char ' '
      done;
      print_newline ()
    done
  in
  let move_player direction =
    let x, y = !player_position in
    let new_position =
      match direction with
      | 'w' -> (max 0 (x - 1), y)
      | 's' -> (min (Array.length maze - 1) (x + 1), y)
      | 'a' -> (x, max 0 (y - 1))
      | 'd' -> (x, min (Array.length maze.(0) - 1) (y + 1))
      | _ -> (x, y)
    in
    let nx, ny = new_position in
    if maze.(nx).(ny) <> 'X' then player_position := new_position
  in
  let rec game_loop steps =
    print_maze ();
    if is_exit_reached () then Success steps
    else (
      print_endline "\nMove (w: up, s: down, a: left, d: right): ";
      let input = read_line () in
      if String.length input = 1 then move_player input.[0];
      game_loop (steps + 1))
  in
  print_endline "Welcome to the maze! Reach the exit (E).";
  game_loop 0
