open Aoc2023.Util
open Aoc2023.Parsing

type node = { label: string; left: string; right: string }

let parse_input = p_new (fun ds ns -> ds, ns)
|= p_end_lf |. p_lf
|= p_any (p_new (fun l' l r -> l', { label = l'; left = l; right = r })
  |= p_ifs c_alnum +> string_of_char_list
  |. p_sps |. p_str "=" |. p_sps |. p_str "("
  |= p_ifs c_alnum +> string_of_char_list
  |. p_str "," |. p_sps
  |= p_ifs c_alnum +> string_of_char_list
  |. p_str ")" |. p_lf_opt
) +> List.to_seq +> Hashtbl.of_seq

let take_step map from = function
| 'L' -> Hashtbl.find map from.left
| 'R' -> Hashtbl.find map from.right
| _ -> raise Unreachable

let part1 input = 
  let directions, map = input |> parse_string parse_input in
  directions |> List.to_seq |> Seq.cycle 
  |> Seq.scan (take_step map) (Hashtbl.find map "AAA")
  |> Seq.take_while (fun n -> String.equal "ZZZ" n.label |> not)
  |> Seq.length