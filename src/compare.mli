type ratings =
  | RoyalFlush
  | StraightFlush
  | FourOfAKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

type counts

val check_straight : int list -> bool

val check_flush : Deck.card list -> bool

val check_royal_flush : Deck.card list -> bool

val check_straight_flush : Deck.card list -> bool

val check_four : Deck.card list -> bool

val check_full_house : Deck.card list -> bool

val check_three : Deck.card list -> bool

val check_two_pair : Deck.card list -> bool

val check_pair : Deck.card list -> bool

val compare_one : Player.t -> Deck.card list -> ratings

val compare : Player.t list -> Deck.card list -> int -> int -> int