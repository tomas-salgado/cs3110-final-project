type maze_result =
  | Success of int
  | Failure

let initialize_maze () =
  [|
    [| "S"; " "; "X"; " "; " "; "X"; " "; " "; " "; " " |];
    [| " "; "X"; "X"; " "; "X"; " "; "X"; "X"; " "; " " |];
    [| " "; " "; " "; " "; " "; " "; " "; "X"; " "; " " |];
    [| " "; "X"; "X"; "X"; "X"; " "; "X"; " "; " "; " " |];
    [| " "; " "; " "; " "; "X"; " "; " "; " "; "X"; " " |];
    [| "X"; "X"; "X"; " "; " "; " "; "X"; " "; "X"; " " |];
    [| " "; " "; " "; " "; " "; "X"; "X"; " "; " "; " " |];
    [| " "; "X"; "X"; "X"; " "; " "; " "; " "; "X"; " " |];
    [| " "; " "; " "; "X"; " "; "X"; "X"; " "; " "; " " |];
    [| " "; " "; " "; " "; " "; " "; " "; " "; " "; "E" |];
  |]

let is_exit_reached maze (x, y) = maze.(x).(y) = "E"

let print_maze maze player_position =
  let height = Array.length maze in
  let width = Array.length maze.(0) in
  print_endline (String.make ((width * 2) + 2) '+');
  for i = 0 to height - 1 do
    print_string "|";
    for j = 0 to width - 1 do
      if (i, j) = player_position then print_string "P "
      else print_string (maze.(i).(j) ^ " ")
    done;
    print_endline "|"
  done;
  print_endline (String.make ((width * 2) + 2) '+')

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

let get_final_score result =
  match result with
  | Success steps -> Some steps
  | Failure -> None
