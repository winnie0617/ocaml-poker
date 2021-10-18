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

let transition g : game =
  Table.get_players g.table
  |> List.map (fun x -> print_endline (Player.player_string x));
  print_endline (Deck.cards_to_string (Table.get_deck g.table));
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

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to OCaml Poker.\n";
  print_endline "Please enter the number of players.\n";
  let n = 3 in
  print_endline "Please enter the names of the players.\n";
  let t =
    [ "Yilun"; "Fionna"; "Winnie" ] |> add_player [] |> Table.init
  in
  let g = { table = t; active = true } in

  game_loop g

(* Execute the game engine. *)
let () = main ()