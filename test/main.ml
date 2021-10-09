open OUnit2
open Game
open Deck

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

let d = Deck.new_deck

let ad, draw1 = Deck.draw d

let full_deck =
  "[AD; 2D; 3D; 4D; 5D; 6D; 7D; 8D; 9D; 10D; JD; QD; KD; AC; 2C; 3C; \
   4C; 5C; 6C; 7C; 8C; 9C; 10C; JC; QC; KC; AH; 2H; 3H; 4H; 5H; 6H; \
   7H; 8H; 9H; 10H; JH; QH; KH; AS; 2S; 3S; 4S; 5S; 6S; 7S; 8S; 9S; \
   10S; JS; QS; KS]"

let without_ad =
  "[2D; 3D; 4D; 5D; 6D; 7D; 8D; 9D; 10D; JD; QD; KD; AC; 2C; 3C; 4C; \
   5C; 6C; 7C; 8C; 9C; 10C; JC; QC; KC; AH; 2H; 3H; 4H; 5H; 6H; 7H; \
   8H; 9H; 10H; JH; QH; KH; AS; 2S; 3S; 4S; 5S; 6S; 7S; 8S; 9S; 10S; \
   JS; QS; KS]"

let test_same_deck name res expected =
  name >:: fun _ ->
  assert_equal
    (pp_list Deck.card_to_string res)
    expected ~printer:pp_string

let deck_tests =
  [
    test_same_deck "New deck should have all 52 cards" d full_deck;
    test_same_deck "First card drawn should be AD" [ ad ] "[AD]";
    test_same_deck "New deck should have everything but AD" draw1 without_ad;
  ]

let suite = "test suite for poker" >::: List.flatten [ deck_tests ]

let _ = run_test_tt_main suite