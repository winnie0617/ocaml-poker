type stage =
  | Preflop
  | Flop
  | Turn
  | River
  | Showdown
  | End

type t = {
  players : Player.t list;
  pot : int;
  deck : Deck.deck;
  stage : stage;
  small_blind : int;
  big_blind : int;
  com_cards : Deck.card list;
}

let get_players t : Player.t list = t.players

let get_stage t : stage = t.stage

let get_deck t : Deck.deck = t.deck

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

(* let place_com_card (n:int) (t:t) : t = *)

let transition t : t =
  match t.stage with
  | Preflop ->
      let plst, d = deal_cards t.players t.deck in
      { t with players = plst; deck = d; stage = Flop }
  | Flop ->
      let c1, d1 = Deck.draw t.deck in
      let c2, d2 = Deck.draw d1 in
      let c3, d3 = Deck.draw d2 in
      { t with com_cards = t.com_cards @ [ c1; c2; c3 ]; stage = Turn }
  | Turn -> { t with stage = River }
  | River -> { t with stage = Showdown }
  | Showdown -> { t with stage = End }
  | End -> failwith "Game had ended"
