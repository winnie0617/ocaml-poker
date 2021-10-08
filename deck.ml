(* rank * suit. rank: values 1..13. 1 maps to A, 11 maps to J, etc suit:
   0, 1, 2, 3, corresponding to diamonds, clubs, hearts and spades *)
type card = int * int

type deck = card list

let new_deck : deck =
  (* helper fun to generate a list of all ranks of same suit *)
  let all_ranks s =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13 ]
    |> List.map (fun r -> (r, s))
  in
  (* generate all ranks for each suit *)
  [ 0; 1; 2; 3 ] |> List.map (fun s -> all_ranks s) |> List.concat

(* TODO: mutable list? *)
let draw d =
  match d with
  | [] -> failwith "No more cards"
  | h :: t -> (h, t)

let shuffle (d : deck) =
  (* Prepend random int to indicate order*)
  d
  |> List.map (fun c -> (Random.bits (), c))
  |> List.sort compare
  (* Remove order after sorting *)
  |> List.map (fun (_, c) -> c)

let rank_of (c:card) : string =
  match fst c with
  | 1 -> "A"
  | 11 -> "J"
  | 12 -> "Q"
  | 13 -> "K"
  | r -> string_of_int r

let suit_of (c:card) : string =
  match snd c with
  | 0 -> "D"
  | 1 -> "C"
  | 2 -> "H"
  | 3 -> "S"
  | _ -> failwith "Invalid suit"

let card_to_string (c:card) : string =
  (rank_of c) ^ (suit_of c)
