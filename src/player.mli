type t

val new_player: string -> int -> t

val add_cards: t -> Deck.card list -> t

val update_chips : t -> int -> t

val update_prev_bet : t -> int



