type t = {
  id : int;
  name : string;
  cards : Deck.card list;
  chips : int;
  prev_bet : int;
}

exception Illegal

let new_player (id : int) (name : string) =
  { id; name; cards = []; chips = 100; prev_bet = 0 }

let add_cards (player : t) (lst : Deck.card list) =
  { player with cards = player.cards @ lst }

let increase_chips (amount : int) (player : t) =
  { player with chips = player.chips + amount }

let increase_bet (amount : int) (player : t) =
  { player with prev_bet = player.prev_bet + amount }

let rec get_player id lst =
  match lst with
  | [] -> raise Illegal
  | h :: t -> if h.id = id then h else get_player id t

let get_cards t = t.cards

let get_chips t = t.chips

let get_name t = t.name

let get_prev_bet t = t.prev_bet

let set_name (player : t) (name : string) = { player with name }

(*TODO: write a better representation. Kinda ugly now*)
let player_string t : string =
  "Name: " ^ t.name ^ ", Chips:" ^ string_of_int t.chips ^ ", Bet:"
  ^ string_of_int t.prev_bet
  ^ ", Cards: "
  ^ Deck.cards_to_string t.cards
