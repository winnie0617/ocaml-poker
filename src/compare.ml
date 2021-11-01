type ratings =
  | RoyalStraightFlush
  | StraightFlush
  | FourOfAKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

exception Empty

type counts = {
  diamonds : int;
  clubs : int;
  hearts : int;
  spades : int;
}

(*From 99 Problems in OCaml*)
let slice list i k =
  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n - 1) t
  in
  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n - 1) t
  in
  take (k - i + 1) (drop i list)

let rec subtract_consecutive combolist =
  match combolist with
  | [] -> []
  | h :: t ->
      List.map (fun x -> x - List.hd h) h :: subtract_consecutive t

let rec check_consecutive combolist =
  match combolist with
  | [] -> false
  | h :: t ->
      if h = [ 0; 1; 2; 3; 4 ] || h = [ 0; 9; 10; 11; 12 ] then true
      else check_consecutive t

let rec get_cards_of_suit cards (suit : int) =
  match cards with
  | [] -> []
  | h :: t ->
      if snd h = suit then h :: get_cards_of_suit t suit
      else get_cards_of_suit t suit

let check_straight (ranks : int list) =
  let sorted_ranks = List.sort compare ranks in
  let list1 = slice sorted_ranks 0 4 in
  let list2 = slice sorted_ranks 1 5 in
  let list3 = slice sorted_ranks 2 6 in
  let combolist = [ list1; list2; list3 ] in
  let sub = subtract_consecutive combolist in
  check_consecutive sub

let rec count_suits (suits : int list) (counts : counts) : counts =
  let c = counts in
  match suits with
  | [] -> c
  | x :: t ->
      if x = 0 then count_suits t { c with diamonds = c.diamonds + 1 }
      else if x = 1 then count_suits t { c with clubs = c.clubs + 1 }
      else if x = 2 then count_suits t { c with hearts = c.hearts + 1 }
      else count_suits t { c with spades = c.spades + 1 }

(* let rec count_ranks (rank: int list) (counts:counts):counts = *)

let check_flush cards =
  let suits = List.map (fun x -> snd x) cards in
  let (counts : counts) =
    { diamonds = 0; clubs = 0; hearts = 0; spades = 0 }
  in
  let result_counts = count_suits suits counts in
  result_counts.clubs >= 5
  || result_counts.diamonds >= 5
  || result_counts.hearts >= 5
  || result_counts.spades >= 5

let check_royal cards (a : int) =
  let suit_cards = List.filter (fun x -> snd x = a) cards in
  let suit_ranks = List.map (fun x -> fst x) suit_cards in
  let sort_ranks = List.sort compare suit_ranks in
  List.mem 1 sort_ranks && List.mem 10 sort_ranks
  && List.mem 11 sort_ranks && List.mem 12 sort_ranks
  && List.mem 13 sort_ranks

let check_royal_flush cards =
  if check_flush cards then
    let suits = List.map (fun x -> snd x) cards in
    let (counts : counts) =
      { diamonds = 0; clubs = 0; hearts = 0; spades = 0 }
    in
    let result_count = count_suits suits counts in
    if
      result_count.diamonds > result_count.clubs
      && result_count.diamonds > result_count.hearts
      && result_count.diamonds > result_count.spades
    then check_royal cards 0
    else if
      result_count.clubs > result_count.diamonds
      && result_count.clubs > result_count.hearts
      && result_count.clubs > result_count.spades
    then check_royal cards 1
    else if
      result_count.hearts > result_count.diamonds
      && result_count.hearts > result_count.clubs
      && result_count.hearts > result_count.spades
    then check_royal cards 2
    else check_royal cards 3
  else false

let check_straight_flush cards =
  if check_flush cards then
    let suit_list = List.map (fun x -> snd x) cards in
    let (counts : counts) =
      { diamonds = 0; clubs = 0; hearts = 0; spades = 0 }
    in
    let result_count = count_suits suit_list counts in
    if
      result_count.diamonds > result_count.clubs
      && result_count.diamonds > result_count.hearts
      && result_count.diamonds > result_count.spades
    then
      check_straight
        (List.map (fun x -> fst x) (get_cards_of_suit cards 0))
    else if
      result_count.clubs > result_count.diamonds
      && result_count.clubs > result_count.hearts
      && result_count.clubs > result_count.spades
    then
      check_straight
        (List.map (fun x -> fst x) (get_cards_of_suit cards 1))
    else if
      result_count.hearts > result_count.diamonds
      && result_count.hearts > result_count.clubs
      && result_count.hearts > result_count.spades
    then
      check_straight
        (List.map (fun x -> fst x) (get_cards_of_suit cards 2))
    else
      check_straight
        (List.map (fun x -> fst x) (get_cards_of_suit cards 3))
  else false

let insert k lst =
  if List.mem_assoc k lst then
    let prev_val = List.assoc k lst in
    let new_lst = List.remove_assoc k lst in
    (k, prev_val + 1) :: new_lst
  else (k, 1) :: lst

let rec make_rank_assoc ranks acc =
  match ranks with
  | [] -> acc
  | h :: t -> make_rank_assoc t (insert h acc)

let occurance_list cards =
  let ranks = List.map (fun x -> fst x) cards in
  let assoc_ranks = make_rank_assoc ranks [] in
  let ocur_list =
    List.map (fun x -> List.assoc (fst x) assoc_ranks) assoc_ranks
  in
  ocur_list

let check_num cards (a : int) =
  let ocur_list = occurance_list cards in
  List.mem a ocur_list

let check_times_occur cards (a : int) =
  let ocur_list = occurance_list cards in
  let times_occur_list = make_rank_assoc ocur_list [] in
  try List.assoc a times_occur_list with
  | Not_found -> 0

let check_four cards = check_num cards 4

let check_full_house cards = check_num cards 3 && check_num cards 2

let check_three cards = check_num cards 3

let check_two_pair cards =
  check_times_occur cards 2 = 2 || check_times_occur cards 2 = 3

let check_pair cards = check_num cards 2

(* let compare_one (player : Player.t) (com_cards : Deck.card list) =
   let cards = Player.get_cards player @ com_cards in if

   let compare (players : Player.t list) (com_cards : Deck.card list) =
   match players with | [] -> [] | h :: t -> compare_one h com_cards *)
