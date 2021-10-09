type card

type deck = card list

val new_deck : deck

(** draw is a tuple of (drawn card * updated deck) *)
val draw : deck -> card * deck

(** shuffle is the same [deck] but shuffled *)
val shuffle : deck -> deck
