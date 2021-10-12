type t

val new_player: string -> int -> t

val add_cards: t -> Deck.card list -> t