open Game

type game = {
  table : Table.t;
  (* TODO: state to keep check of rounds, players standing etc *)
  active : bool;
}

(* Broken function rn: *)
(* let rec valid_input (f : 'a -> 'b) input : 'b = try f input with
   "Invalid" -> print_endline " "; print_endline "Invalid input. Please
   try again.\n"; valid_input read_line() *)

let rec add_player acc name_lst : Player.t list =
  match name_lst with
  | [] -> acc
  | name :: rest ->
      let p = Player.new_player (List.length acc) name in
      add_player (p :: acc) rest

(** TODO: need to rewrite this. Currently just a quick implementation to
    help visualization what's going on*)
let transition g : game =
  Table.get_players g.table
  |> List.map (fun x -> print_endline (Player.player_string x));
  print_endline
    ("Deck: " ^ Deck.cards_to_string (Table.get_deck g.table));
  print_endline
    ("Community cards: "
    ^ Deck.cards_to_string (Table.get_com_cards g.table));
  { g with table = Table.transition g.table }

let rec game_loop g : unit =
  let g' = transition g in
  match g'.active with
  | false ->
      print_endline "";
      print_string "End of the game. Bye!\n";
      exit 0
  | true -> game_loop g'

let play_game g : unit = game_loop g

let rec make_n_players n acc : Player.t list =
  let intn = int_of_string n in
  if intn = 1 then Player.new_player 1 "" :: acc
  else
    make_n_players
      (string_of_int (intn - 1))
      (Player.new_player intn "" :: acc)

let rec make_n_names n acc =
  match n with
  | [] -> acc
  | h :: t ->
      print_endline "Please enter the name of the Player.\n";
      let user_name = read_line () in
      make_n_names t (Player.set_name h user_name :: acc)

let print_blinds (table : Table.t) =
  let bb = Table.get_big_blind table in
  let sb = Table.get_small_blind table in
  print_endline (Player.get_name sb ^ " is the Small Blind\n");
  print_endline (Player.get_name bb ^ " is the Big Blind\n")

let main () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\n\nWelcome to OCaml Poker.\n";
  print_endline "Please enter the number of players.\n";
  let n = make_n_players (read_line ()) [] in
  let players = make_n_names n [] in
  let t = Table.init players in
  let g = { table = t; active = true } in
  print_blinds t;
  game_loop g

(* Execute the game engine. *)
let () = main ()