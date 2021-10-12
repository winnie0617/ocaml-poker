type stage =
  | Preflop
  | Flop
  | Turn
  | River

type t = {
  players : Player.t list;
  pot : int;
  deck : Deck.deck;
  stage : stage;
}

let init (plst : Player.t list) : t =
  { players = plst; pot = 0; deck = Deck.new_deck; stage = Preflop }
