open Aoc2023.Util
open Aoc2023.Parsing

type node = { left: string; right: string }

let parse_input = p_new (fun ds ns -> ds, ns)
|= p_end_lf |. p_lf
|= p_any (p_new (fun l' l r -> l', { left = l; right = r })
  |= p_ifs c_alnum +> string_of_char_list
  |. p_sps |. p_str "=" |. p_sps |. p_str "("
  |= p_ifs c_alnum +> string_of_char_list
  |. p_str "," |. p_sps
  |= p_ifs c_alnum +> string_of_char_list
  |. p_str ")" |. p_lf_opt
) +> List.to_seq +> Hashtbl.of_seq

let take_step map from =
  let get label = if String.equal label "ZZZ" then None 
    else Some (Hashtbl.find map label) 
  in
  function
  | 'L' -> get from.left
  | 'R' -> get from.right
  | _ -> raise Unreachable

let walk_len map dirs =
  let rec walk from dirs = match Seq.uncons dirs with
  | Some (d, ds) -> (match take_step map from d with
    | Some node -> 1 + walk node ds 
    | None -> 1)
  | None -> raise Unreachable
  in
  walk (Hashtbl.find map "AAA") dirs

let part1 input = 
  let directions, map = input |> parse_string parse_input in
  walk_len map (directions |> List.to_seq |> Seq.cycle)