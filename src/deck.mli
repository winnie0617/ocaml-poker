type card = int * int

type deck = card list

val empty : deck

val push : card -> deck -> deck
(** [push card deck] takes in a new card and places in the front of the
    existed deck*)

(** [peek deck] returns the first card of the deck but does not change
    the deck*)
(* val peek: deck -> card *)

(** [pop deck] removes the first card from the deck and returns the rest
    of the deck*)
(* val pop: deck -> card *)

val len : deck -> int
(** [len deck] returns the length of the deck*)

val new_deck : deck

val draw : deck -> card * deck
(** draw is a tuple of (drawn card * updated deck) *)

val shuffle : deck -> deck
(** shuffle is the same [deck] but shuffled *)

val card_to_string : card -> string
(** Prints card *)

val cards_to_string : card list -> string