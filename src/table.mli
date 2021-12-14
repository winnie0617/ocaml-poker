type t

val get_players : t -> Player.t list
(** [get_players t] is the player list of table t*)

val get_deck : t -> Deck.deck
(** [get_deck t] is the deck of table t*)

val get_pot : t -> int
(** [get_pot t] is the amount in the pot of table t*)

val get_com_cards : t -> Deck.card list
(** [get_com_cards t] is the current community cards being shown*)

val init : Player.t list -> t
(** [init plist] is the initialized table with provided players*)

val deal_cards : Player.t list -> Deck.deck -> Player.t list * Deck.deck
(** Deal[plst] is the new player list and deck after two cards are dealt
    to each player*)

val transition : t -> t
(** [transition t] is the table after one stage has been played*)

val get_big_blind : t -> Player.t
(** [get_big_blind t] is the big blind of table t*)

val get_small_blind : t -> Player.t
(** [get_small_blind t] is the small blind of table t*)
