(* values 1..13 *)
(* 1 maps to A, 11 maps to J, etc *)
type rank = int 

(* values 0, 1, 2, 3, corresponding to diamonds, clubs, hearts and spades *)
type suit = int

type t = rank * suit

type deck = t list

val new_deck : deck

(* draw is a tuple of (drawn card * updated deck) *)
val draw : deck -> t * deck

val shuffle : deck -> deck 