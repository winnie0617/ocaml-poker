type stage =
  | Preflop
  | Flop
  | Turn
  | River
  | Showdown

type t = {
  players : Player.t list;
  pot : int;
  deck : Deck.deck;
  stage : stage;
  small_blind : int;
  big_blind : int;
  com_cards : Deck.card list;
}

let init (plst : Player.t list) : t =
  {
    players = plst;
    pot = 0;
    deck = Deck.new_deck;
    stage = Preflop;
    small_blind = 1;
    big_blind = 2;
    com_cards = [];
  }

let rec deal_cards (plst : Player.t list) (d : Deck.deck) :
    Player.t list * Deck.deck =

  let deal_h p d =
    let c1, d1 = Deck.draw d in
    let c2, d2 = Deck.draw d1 in
    let p' = Player.add_cards p [ c1; c2 ] in
    (p', d2)
  in
  
  match plst with
  | [] -> (plst, d)
  | h :: t ->
      let plst', d' = deal_cards t d in
      let h', d_final = deal_h h d' in
      (h' :: plst', d_final)
