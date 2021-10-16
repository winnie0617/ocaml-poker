type command =
| Check
| Raise
| Fold
| Call

exception Empty
exception Illegal

let parse str =
  let splitstring = String.split_on_char ' ' str in
  let list = List.filter (fun a -> a <> "") splitstring in
  match list with
  | [] -> raise Empty
  | Check ->
  | Raise
  | Fold
  | Call