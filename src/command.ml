type command =
  | Check
  | RaiseBy of int
  | Fold
  | Call

exception Empty

exception Illegal

let parse str : command =
  let lower_str = String.lowercase_ascii str in
  let splitstring = String.split_on_char ' ' lower_str in
  let list = List.filter (fun a -> a <> "") splitstring in
  match list with
  | [] -> raise Empty
  | h :: t ->
      if h = "check" then Check
      else if h = "fold" then Fold
      else if h = "call" then Call
      else if h = "raise" then RaiseBy (int_of_string (List.nth t 1))
      else raise Illegal

let in_to_cmd input : command option =
  try Some (parse input) with
  | e -> None

let rec get_cmd () : command =
  print_string
    "Enter a command. Enter either check, fold, call, or raise by n\n";
  print_string "> ";
  match in_to_cmd (read_line ()) with
  | None ->
      print_endline " ";
      print_endline "Invalid input. Please try again.\n";
      get_cmd ()
  | Some cmd -> cmd

