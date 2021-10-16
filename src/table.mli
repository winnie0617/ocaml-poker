type t

(** Initialize table with provided players*)
val init : Player.t list -> t

(** Deal[plst] is the new player list and deck after two cards are dealt to each player*)
val deal_cards : Player.t list -> Player.t list * Deck.deck

