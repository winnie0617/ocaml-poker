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
  small_blind : int;
  big_blind : int;
}

let init (plst : Player.t list) : t =
  {
    players = plst;
    pot = 0;
    deck = Deck.new_deck;
    stage = Preflop;
    small_blind = 1;
    big_blind = 2;
  }
