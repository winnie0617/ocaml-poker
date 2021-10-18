type t

(** [new_player str id] initializes a new player by assigning an [id] and [name] 
and returns a new player of type t*)
val new_player : int -> string -> t

(** [add_cards player lst] is the player with the list of cards added to their hand*)
val add_cards : t -> Deck.card list -> t

(** [increase_chips chips player ] is the player with updated amount of chips*)
val increase_chips : int -> t -> t

(** [increase_bet amount player ] is the player with updated prev bet*)
val increase_bet : int -> t -> t

val get_player : int -> t list -> t

val get_cards : t -> Deck.card list

val get_chips : t -> int

val get_prev_bet : t -> int

val player_string : t -> string
