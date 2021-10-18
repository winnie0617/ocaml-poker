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

let update_chips (player : t) (amount : int) =
  { player with chips = player.chips - amount }

let update_prev_bet (player : t) (amount : int) =
  { player with prev_bet = amount }

let rec get_player id lst =
  match lst with
  | [] -> raise Illegal
  | h :: t -> if h.id = id then h else get_player id t

let get_cards t = t.cards

let get_chips t = t.chips

let get_prev_bet t = t.prev_bet

    
(*TODO: write a better representation. Kinda ugly now*)
let player_string t : string =
  "Name: " ^ t.name ^ ", Hand: " ^ Deck.cards_to_string t.cards