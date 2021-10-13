type play =
  | Raise
  | Calls
  | Folds

type t = {
  id : int;
  name : string;
  cards : Deck.card list;
  chips : int;
  position : play option;
}

let new_player (id : int) (name : string) =
  { id; name; cards = []; chips = 0; position = None }

let add_cards (player : t) (lst : Deck.card list) =
  { player with cards = player.cards @ lst }
