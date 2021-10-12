type t = {
  id : int;
  name : string;
  cards : Deck.card list;
}

let new_player (name : string) (id : int) = { id; name; cards = [] }

let add_cards (player : t) (lst : Deck.card list) =
  { player with cards = player.cards @ lst }

