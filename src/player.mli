type t

(** [new_player str id] initializes a new player by assigning an [id] and [name] 
and returns a new player of type t*)
val new_player : int -> string -> t

(** [add_cards player lst] is the player with the list of cards added to their hand*)
val add_cards : t -> Deck.card list -> t

(** [update_chips player chips] is the player with updated amount of chips*)
val update_chips : t -> int -> t

(** [update_prev_bet player amount] is the player with updated prev bet*)
val update_prev_bet : t -> int -> t

val get_player : int -> t list -> t

val get_cards : t -> Deck.card list

val get_chips : t -> int

val get_prev_bet : t -> int

