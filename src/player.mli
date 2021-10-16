type t

(** [new_player str id] initializes a new player by assigning an [id] and [name] 
and returns a new player of type t*)
val new_player: string -> int -> t

(** [add_cards player lst] updates the player's cards*)
val add_cards: t -> Deck.card list -> t

(** [update_chips player chips] updates the player's chips*)
val update_chips : t -> int -> t

(** [update_prev_bet player amount] updates the player's previous bet*)
val update_prev_bet : t -> int



