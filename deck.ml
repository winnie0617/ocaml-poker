type rank = int 

type suit = int

type t = rank * suit

type deck = t list

let new_deck : deck =
  (* helper fun to generate a list of all ranks of same suit *)
  let all_ranks s = [1;2;3;4;5;6;7;8;9;10;11;12;13] |> List.map (fun r -> (r, s)) in
  (* generate all ranks for each suit *)
  [0;1;2;3] |> List.map (fun s -> all_ranks s) |> List.concat

(* TODO: mutable list? *)
let draw deck = 
  match deck with
| [] -> failwith "No more cards"
| h :: t -> (h, t)

let shuffle deck = 
  (* Prepend random int to indicate order, then remove after sorting*)
  deck |> List.map (fun c -> (Random.bits(), c)) |> List.sort compare |> List.map (fun (_, c) -> c)


