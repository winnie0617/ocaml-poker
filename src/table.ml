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

let get_big_blind t : Player.t = Player.get_player t.big_blind t.players

let get_small_blind t  : Player.t = Player.get_player t.small_blind t.players

let is_big_blind t p = Player.get_id p = t.big_blind

let get_pot t = t.pot

let init (plst : Player.t list) : t =
  {
    players = plst;
    pot = 0;
    deck = Deck.new_deck;
    stage = Preflop;
    small_blind = 2;
    big_blind = 1; (*Player index is inverse order*)
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
    Player.(List.hd t.players |> increase_bet a |> increase_chips ~-a)
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
  let p' = Player.(p |> increase_bet a |> increase_chips ~-a) in
  {
    t with
    players = List.tl t.players @ [ p' ];
    pot = t.pot + a (* num_p_checked = t.num_p_checked + 1; *);
  }
(* TODO: does # of p checked change?*)

(** [preflop_updates players t] is the list of players in the same order
    but with forced bets from big blind and small blind*)
let rec preflop_updates (players : Player.t list) (t : t) =
  let bb = get_big_blind t in
  let sb = get_small_blind t in
  match players with
  | [] -> []
  | h :: tail ->
      if h = sb then
        let h1 = Player.increase_bet t.min_bet h in
        let h2 = Player.increase_chips (-1 * t.min_bet) h1 in
        h2 :: preflop_updates tail t
      else if h = bb then
        let h1 = Player.increase_bet (t.min_bet * 2) h in
        let h2 = Player.increase_chips (-2 * t.min_bet) h1 in
        h2 :: preflop_updates tail t
      else h :: preflop_updates tail t

(** Rearrange list such that list starts with the player right after s*)
let rec set_order (t : t) (players : Player.t list) =
  match players with
  | [] -> []
  | curr :: rest ->
      if is_big_blind t curr then rest @ [ curr ]
      else begin print_endline ("Not yet");
      set_order t (rest @ [ curr ]) end
(* let set_order2 t (players : Player.t list) = if t.big_blind = 0 then
   List.tl players @ [ List.hd players ] else (* move fist n element in
   lst to acc*) let rec split n acc lst = if n = List.length acc then
   (acc, lst) else match lst with | [] -> failwith "Empty List" | h :: t
   -> split n (acc @ [ h ]) t in let fst, snd = split (t.big_blind + 1)
   [] players in snd @ fst *)

(** [legal_lst p t] is the list of string of allowed commands*)
let legal_lst p t : string list =
  (* Only big blind can check in preflop*)
  if t.stage = Preflop && not (is_big_blind t p) then
    [ "call"; "raise by n"; "fold" ] (* When need to match max bet*)
  else if Player.get_prev_bet p < t.curr_max then
    [ "call"; "raise by n"; "fold" ]
  else [ "check"; "raise by n"; "fold" ]

let rec get_legal_cmd (p : Player.t) (t : t) : Command.command =
  let cmd_string cmd =
    match cmd with
    | Command.Fold -> "fold"
    | Check -> "check"
    | Call -> "call"
    | RaiseBy a -> "raise by n"
  in
  let cmd = Command.get_cmd () in
  let lst = legal_lst p t in
  if List.mem (cmd_string cmd) lst then cmd
  else begin
    print_endline (cmd_string cmd ^ " is not allowed");
    get_legal_cmd p t
  end

(** Prompts player to type in command until it is a legal action*)
let rec get_legal_cmd2 (p : Player.t) (t : t) : Command.command =
  match Command.get_cmd () with
  | Fold -> Fold
  | Call -> Call
  | Check ->
      if t.stage = Preflop then
        print_endline
          "You cannot check preflop. Please either raise, call or fold.";
      get_legal_cmd2 p t
  | RaiseBy a ->
      if a >= t.curr_max then Command.RaiseBy a
      else begin
        print_endline
          ("You have to place a bet that is higher than the current \
            minimum bet, ."
          ^ string_of_int t.curr_max
          ^ ", by at least double.");
        get_legal_cmd2 p t
      end

let rec betting_loop (t : t) : t =
  if t.num_p_checked = List.length t.players then t
    (* everyone checked -> done!*)
  else
    match t.players with
    (* TODO: placeholders *)
    | [] -> failwith "No players"
    | curr :: rest -> begin
        print_endline ("It is " ^ Player.get_name curr ^ "'s turn");
        match get_legal_cmd curr t with
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

(*let set_blinds t : t = let sb = Player.get_player t.small_blind
  t.players in let bb = Player.get_player t.big_blind t.players in t*)

(* let action (p:Player.t) (c:Command.command) : p = match command
   with *)

let transition t : t =
  match t.stage with
  | Preflop ->
      let d' = Deck.shuffle t.deck in
      let plst, d = deal_cards t.players d' in
      let t' = { t with players = plst; deck = d } in
      let t'' =
        (* Forced bets*)
        {
          t' with
          players = preflop_updates t.players t |> set_order t;
          pot = t.pot + (t.min_bet * 3);
        }
        |> betting_loop
      in
      { t'' with stage = Flop }
  | Flop ->
      let c1, d1 = Deck.draw t.deck in
      let c2, d2 = Deck.draw d1 in
      let c3, d3 = Deck.draw d2 in
      let t' =
        { t with com_cards = t.com_cards @ [ c1; c2; c3 ]; deck = d3 }
        |> betting_loop
      in
      { t' with stage = Turn; num_p_checked = 0 }
      (*reset check count*)
  | Turn ->
      let c1, d1 = Deck.draw t.deck in
      let t' =
        { t with com_cards = t.com_cards @ [ c1 ]; deck = d1 }
        |> betting_loop
      in
      { t' with stage = River; num_p_checked = 0 }
  | River ->
      let c1, d1 = Deck.draw t.deck in
      let t' =
        { t with com_cards = t.com_cards @ [ c1 ]; deck = d1 }
        |> betting_loop
      in
      { t' with stage = Showdown; num_p_checked = 0 }
  | Showdown -> { t with stage = End } (*TODO*)
  | End -> failwith "Game had ended"
