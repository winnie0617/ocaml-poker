type ratings =
  | RoyalStraightFlush
  | StraightFlush
  | FourOfAKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

exception Empty

(* let compare_helper (player : Player.t) (com_cards : Deck.card list) =
  let cards = Player.get_cards player @ com_cards in
  match cards with
  | [] -> raise Empty
  | h :: t ->  *)

(* let compare (players : Player.t list) (com_cards : Deck.card list) =
  match players with
  | [] -> []
  | h :: t -> compare_helper h com_cards *)
