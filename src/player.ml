type t = {
  id : int;
  name : string;
  cards : Deck.card list;
  chips : int;
  prev_bet : int;
}

let new_player (id : int) (name : string) =
  { id; name; cards = []; chips = 100; prev_bet = 0 }

let add_cards (player : t) (lst : Deck.card list) =
  { player with cards = player.cards @ lst }

let update_chips (player : t) (amount : int) =
  { player with chips = player.chips - amount }

let update_prev_bet (player : t) (amount : int) =
  player.prev_bet = amount
