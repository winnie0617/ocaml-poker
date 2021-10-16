type command =
  | Check
  | RaiseBy of float
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
      else if h = "raise" then RaiseBy (float_of_string (List.nth t 1))
      else raise Illegal
