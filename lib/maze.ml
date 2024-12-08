type maze_result =
  | Success of int
  | Failure

let initialize_maze () =
  [|
    [| "S"; " "; " "; " "; "X" |];
    [| " "; "X"; " "; "X"; " " |];
    [| " "; " "; " "; " "; " " |];
    [| "X"; "X"; " "; "X"; " " |];
    [| " "; " "; " "; " "; "E" |];
  |]

let is_exit_reached maze (x, y) = maze.(x).(y) = "E"

let print_maze maze player_position =
  let height = Array.length maze in
  let width = Array.length maze.(0) in
  print_endline (String.make ((width * 2) + 1) '+');
  for i = 0 to height - 1 do
    print_string "|";
    for j = 0 to width - 1 do
      if (i, j) = player_position then print_string "P "
      else print_string (maze.(i).(j) ^ " ")
    done;
    print_endline "|"
  done;
  print_endline (String.make ((width * 2) + 1) '+')

let move_player maze player_position direction =
  let x, y = player_position in
  let new_position =
    match direction with
    | 'w' -> (max 0 (x - 1), y)
    | 's' -> (min (Array.length maze - 1) (x + 1), y)
    | 'a' -> (x, max 0 (y - 1))
    | 'd' -> (x, min (Array.length maze.(0) - 1) (y + 1))
    | _ -> (x, y)
  in
  let nx, ny = new_position in
  if maze.(nx).(ny) <> "X" then new_position else (x, y)

let rec game_loop maze player_position steps =
  print_newline ();
  print_maze maze player_position;
  if is_exit_reached maze player_position then Success steps
  else (
    print_endline "Enter a direction (W: Up, A: Left, S: Down, D: Right): ";
    let direction = read_line () in
    match direction with
    | "w" | "a" | "s" | "d" ->
        let new_position =
          move_player maze player_position (String.get direction 0)
        in
        game_loop maze new_position (steps + 1)
    | _ ->
        print_endline "Invalid input! Please enter w, a, s, or d.";
        game_loop maze player_position steps)

let play_maze () =
  let maze = initialize_maze () in
  game_loop maze (0, 0) 0

let get_final_score result =
  match result with
  | Success steps -> Some steps
  | Failure -> None
