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
  min_bet : int;
  curr_max : int;
  num_p_checked : int;
}

let get_players t : Player.t list = t.players

let get_stage t : stage = t.stage

let get_deck t : Deck.deck = t.deck

let get_com_cards t : Deck.card list = t.com_cards

let init (plst : Player.t list) : t =
  {
    players = plst;
    pot = 0;
    deck = Deck.new_deck;
    stage = Preflop;
    small_blind = 1;
    big_blind = 2;
    com_cards = [];
    min_bet = 2;
    curr_max = 0;
    num_p_checked = 0;
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

(** [raise a t] is the updated table after the first player on the list
    raises by a The player just acted is appended at the end of the list*)
let raise (a : int) (t : t) : t =
  let p' =
    Player.(List.hd t.players |> increase_bet a |> increase_chips a)
  in
  {
    t with
    players = List.tl t.players @ [ p' ];
    pot = t.pot + a;
    curr_max = Player.get_prev_bet p';
    num_p_checked = 1;
  }

let call (t : t) : t =
  let p = List.hd t.players in
  let a = t.curr_max - Player.get_prev_bet p in
  let p' = Player.(p |> increase_bet a |> increase_chips a) in
  {
    t with
    players = List.tl t.players @ [ p' ];
    pot = t.pot + a;
    (* num_p_checked = t.num_p_checked + 1; *)
  }
(* TODO: does # of p checked change?*)

let rec betting_loop (t : t) : t =
  (* Check if stage is preflop. If so handle differently*)
  if t.num_p_checked = List.length t.players then t
    (* everyone checked -> done!*)
  else
    match t.players with
    (* TODO: placeholders *)
    | [] -> failwith "No players"
    | [] -> failwith "You are the only player left. You won"
    | curr :: rest -> begin
        match Command.get_cmd () with
        | Fold ->
            (* remove that player, who is head of the list*)
            betting_loop { t with players = rest }
        | Call -> betting_loop (call t)
        | Check ->
            betting_loop
              {
                t with
                players = rest @ [ curr ];
                num_p_checked = t.num_p_checked + 1;
              }
        | RaiseBy a -> betting_loop (raise a t)
      end

let preflop t : t =
  let sb = Player.get_player t.small_blind t.players in
  let bb = Player.get_player t.big_blind t.players in
  t

(* let action (p:Player.t) (c:Command.command) : p = match command
   with *)

let transition t : t =
  match t.stage with
  | Preflop ->
      let d' = Deck.shuffle t.deck in
      let plst, d = deal_cards t.players d' in
      let t' = { t with players = plst; deck = d } |> betting_loop in
      { t' with stage = Flop }
  | Flop ->
      let c1, d1 = Deck.draw t.deck in
      let c2, d2 = Deck.draw d1 in
      let c3, d3 = Deck.draw d2 in
      let t' =
        { t with com_cards = t.com_cards @ [ c1; c2; c3 ]; deck = d3 }
        |> betting_loop
      in
      { t' with stage = Turn ; num_p_checked = 0 } (*reset check count*)
  | Turn ->
      let c1, d1 = Deck.draw t.deck in
      let t' =
        { t with com_cards = t.com_cards @ [ c1 ]; deck = d1 }
        |> betting_loop
      in
      { t' with stage = River ; num_p_checked = 0 }
  | River ->
      let c1, d1 = Deck.draw t.deck in
      let t' =
        { t with com_cards = t.com_cards @ [ c1 ]; deck = d1 }
        |> betting_loop
      in
      { t' with stage = Showdown ; num_p_checked = 0 }
  | Showdown -> { t with stage = End } (*TODO*)
  | End -> failwith "Game had ended"
