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
