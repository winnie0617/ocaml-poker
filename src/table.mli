type t

val get_players: t -> Player.t list

val get_deck : t -> Deck.deck

val get_com_cards : t -> Deck.card list

(** Initialize table with provided players*)
val init : Player.t list -> t

(** Deal[plst] is the new player list and deck after two cards are dealt to each player*)
val deal_cards : Player.t list -> Deck.deck -> Player.t list * Deck.deck

val transition : t -> t