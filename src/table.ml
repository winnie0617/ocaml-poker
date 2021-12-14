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
  num_acted : int;
}

let get_players t : Player.t list = t.players

let get_stage t : stage = t.stage

let get_deck t : Deck.deck = t.deck

let get_com_cards t : Deck.card list = t.com_cards

let get_big_blind t : Player.t = Player.get_player t.big_blind t.players

let get_small_blind t : Player.t =
  Player.get_player t.small_blind t.players

let is_big_blind t p = Player.get_id p = t.big_blind

let get_pot t = t.pot

let init (plst : Player.t list) : t =
  {
    players = plst;
    pot = 0;
    deck = Deck.new_deck;
    stage = Preflop;
    small_blind = 2;
    big_blind = 1;
    com_cards = [];
    min_bet = 2;
    curr_max = 0;
    num_acted = 0 (* number of players that have acted*);
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

(** [raise a t] is the updated table after the first player on the list
    raises by a The player just acted is appended at the end of the list*)
let raise (a : int) (t : t) : t =
  let p' =
    Player.(List.hd t.players |> increase_bet a |> increase_chips ~-a)
  in
  {
    t with
    players = List.tl t.players @ [ p' ];
    curr_max = Player.get_prev_bet p';
    num_acted = 1 (* Reset*);
  }

let call (t : t) : t =
  let p = List.hd t.players in
  let a = t.curr_max - Player.get_prev_bet p in
  let p' = Player.(p |> increase_bet a |> increase_chips ~-a) in
  {
    t with
    players = List.tl t.players @ [ p' ];
    num_acted = t.num_acted + 1;
  }

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
      else set_order t (rest @ [ curr ])

(** [legal_lst p t] is the list of string of allowed commands*)
let legal_lst p t : string list =
  (* Only big blind can check in preflop*)
  if t.stage = Preflop && not (is_big_blind t p) then
    [ "call"; "raise by n"; "fold" ] (* When need to match max bet*)
  else if Player.get_prev_bet p < t.curr_max then
    [ "call"; "raise by n"; "fold" ]
  else [ "check"; "raise by n" ]

let rec get_legal_cmd (p : Player.t) (t : t) : Command.command =
  let cmd_string cmd =
    match cmd with
    | Command.Fold -> "fold"
    | Check -> "check"
    | Call -> "call"
    | RaiseBy a -> "raise by n"
  in
  let lst = legal_lst p t in
  print_string
    ("Enter a command. Currently you can: "
    ^ Util.pp_list (fun x -> x) lst
    ^ "\n");
  let cmd = Command.get_cmd () in
  if List.mem (cmd_string cmd) lst then
    match cmd with
    | RaiseBy a when a <= t.curr_max - Player.get_prev_bet p ->
        print_endline
          ("Current minimum bet is "
          ^ string_of_int t.curr_max
          ^ ". You need to raise by more than "
          ^ string_of_int (t.curr_max - Player.get_prev_bet p));
        get_legal_cmd p t
    | _ -> cmd
  else begin
    print_endline (cmd_string cmd ^ " is not allowed");
    get_legal_cmd p t
  end

(** [end_betting] is the updated table with players' bets collected and
    put into pot and counters reset*)
let end_betting (t : t) : t =
  let total =
    List.fold_left ( + ) 0 (List.map Player.get_prev_bet t.players)
  in
  let p_lst =
    List.map
      (fun p -> Player.(increase_bet (-get_prev_bet p) p))
      t.players
  in
  {
    t with
    pot = t.pot + total;
    players = p_lst;
    curr_max = 0;
    num_acted = 0;
  }

(** [split_pot t] s the updated table with pot emptied and given to
    winners*)
let split_pot winner_ids t : t =
  let t' = end_betting t in
  let p_lst = t'.players in
  let amt = t'.pot / List.length winner_ids in
  let p_lst' =
    List.map
      (fun p ->
        if List.mem (Player.get_id p) winner_ids then
          Player.increase_chips amt p
        else p)
      p_lst
  in
  {
    t' with
    pot = 0;
    players = p_lst';
    curr_max = 0;
    num_acted = 0;
    stage = Showdown;
  }

let rec betting_loop (t : t) : t =
  print_endline ("Pot: " ^ string_of_int t.pot);
  t.players
  |> List.map (fun x -> print_endline (Player.player_string x));
  (* All other players folded *)
  if List.length t.players = 1 then
    split_pot (List.map (fun p -> Player.get_id p) t.players) t
  else if List.length t.players = t.num_acted then end_betting t
  else
    match t.players with
    (* TODO: placeholders *)
    | [] -> failwith "No players"
    | curr :: rest -> begin
        print_endline ("It is " ^ Player.get_name curr ^ "'s turn");
        match get_legal_cmd curr t with
        | Fold ->
            (* remove that player, who is head of the list*)
            betting_loop
              {
                t with
                players = rest;
                pot = t.pot + Player.get_prev_bet curr;
              }
        | Call -> betting_loop (call t)
        | Check ->
            betting_loop
              {
                t with
                players = rest @ [ curr ];
                num_acted = t.num_acted + 1;
              }
        | RaiseBy a -> betting_loop (raise a t)
      end

let transition t : t =
  match t.stage with
  | Preflop -> (
      print_endline
        "=========================== Preflop \
         ===========================";
      let d' = Deck.shuffle t.deck in
      let plst, d = deal_cards t.players d' in
      let t = { t with players = plst; deck = d } in
      let t' =
        (* Forced bets*)
        {
          t with
          players = preflop_updates t.players t |> set_order t;
          curr_max = t.min_bet * 2;
        }
        |> betting_loop
      in
      match t'.stage with
      | Showdown -> t'
      | _ -> { t' with stage = Flop })
  | Flop -> (
      print_endline
        "============================= Flop \
         =============================";
      let c1, d1 = Deck.draw t.deck in
      let c2, d2 = Deck.draw d1 in
      let c3, d3 = Deck.draw d2 in
      let t' =
        { t with com_cards = t.com_cards @ [ c1; c2; c3 ]; deck = d3 }
        |> betting_loop
      in
      match t'.stage with
      | Showdown -> t'
      | _ -> { t' with stage = Turn }
      (*reset check count*))
  | Turn -> (
      print_endline
        "============================= Turn \
         =============================";
      let c1, d1 = Deck.draw t.deck in
      let t' =
        { t with com_cards = t.com_cards @ [ c1 ]; deck = d1 }
        |> betting_loop
      in
      match t'.stage with
      | Showdown -> t'
      | _ -> { t' with stage = River })
  | River -> (
      print_endline
        "============================= River \
         =============================";
      let c1, d1 = Deck.draw t.deck in
      let t' =
        { t with com_cards = t.com_cards @ [ c1 ]; deck = d1 }
        |> betting_loop
      in
      match t'.stage with
      | Showdown -> t'
      | _ -> { t' with stage = Showdown })
  | Showdown ->
      let winner = Compare.compare t.players t.com_cards 0 (-1) in
      (* TODO: wait for determinign multiple winners*)
      let winners = [ winner ] in
      let t' = split_pot winners t in
      print_endline
        "=========================== Showdown \
         ===========================";

      print_string
        ("Winner: "
        ^ Player.get_name (Player.get_player winner t.players)
        ^ "\n");
      { t' with stage = End }
      (*Should never reach here*)
  | End -> failwith "Game has ended"
