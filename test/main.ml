open OUnit2
open Game
open Deck


let deck_tests = []
let suite = "test suite for poker" >::: List.flatten [ deck_tests ]

let _ = run_test_tt_main suite