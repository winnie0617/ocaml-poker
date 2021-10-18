type command =
  | Check
  | RaiseBy of int
  | Fold
  | Call

exception Empty

exception Illegal

(** [parse str] takes in the user's input [str] and returns the proper command*)
val parse : string -> command

val in_to_cmd : string -> command option

(** prompts the player to enter sth, returns a command*)
val get_cmd :unit -> command