type t

val get_players : t -> Player.t list

val get_deck : t -> Deck.deck

val get_pot : t -> int

val get_com_cards : t -> Deck.card list

val init : Player.t list -> t
(** Initialize table with provided players*)

val deal_cards : Player.t list -> Deck.deck -> Player.t list * Deck.deck
(** Deal[plst] is the new player list and deck after two cards are dealt
    to each player*)

val transition : t -> t

val get_big_blind : t -> Player.t

val get_small_blind : t -> Player.t