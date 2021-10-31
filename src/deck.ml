(** AF: [card] of type int*int represents a valid playing card with the
    first component representing its rank between 1 and 13 inclusive and
    suit between 0 and 3 inclusive RI: 1 <= [rank] <= 13 and 0 <= [suit]
    <= 4 *)

(* rank * suit. rank: values 1..13. 1 maps to A, 11 maps to J, etc suit:
   0, 1, 2, 3, corresponding to diamonds, clubs, hearts and spades *)
type card = int * int

type deck = card list

exception Empty

let empty : deck = []

let push (c : card) (d : deck) = c :: d

(* let peek = function | [] -> raise Empty | x :: _ -> x

   let pop = function | [] -> raise Empty | _ :: s -> s *)

let len (d : deck) = List.length d

let new_deck : deck =
  (* helper fun to generate a list of all ranks of same suit *)
  let all_ranks s =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13 ]
    |> List.map (fun r -> (r, s))
  in
  (* generate all ranks for each suit *)
  [ 0; 1; 2; 3 ] |> List.map (fun s -> all_ranks s) |> List.concat

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

(* let random_card : card = let card = (Random.int 13 + 1, Random.int 4)
   in card *)

(* let start_round : deck = let card1 = random_card in let card2 =
   random_card in [ card1; card2 ] *)

let rank_of (c : card) : string =
  match fst c with
  | 1 -> "A"
  | 11 -> "J"
  | 12 -> "Q"
  | 13 -> "K"
  | r -> string_of_int r

let suit_of (c : card) : string =
  match snd c with
  | 0 -> "D"
  | 1 -> "C"
  | 2 -> "H"
  | 3 -> "S"
  | _ -> failwith "Invalid suit"

(** TODO: put this in util so every module can use*)
let card_to_string (c : card) : string = rank_of c ^ suit_of c

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let cards_to_string c_lst : string = pp_list card_to_string c_lst
