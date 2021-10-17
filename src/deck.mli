type card

type deck = card list

val empty: deck

(** [push card deck] takes in a new card and places in the front of the existed deck*)
val push: card -> deck -> deck

(** [peek deck] returns the first card of the deck but does not change the deck*)
(* val peek: deck -> card *)

(** [pop deck] removes the first card from the deck and returns the rest of the deck*)
(* val pop: deck -> card *)

(** [len deck] returns the length of the deck*)
val len: deck -> int

val new_deck : deck

(** draw is a tuple of (drawn card * updated deck) *)
val draw : deck -> card * deck

(** shuffle is the same [deck] but shuffled *)
val shuffle : deck -> deck

(** Prints card *)
val card_to_string : card -> string 