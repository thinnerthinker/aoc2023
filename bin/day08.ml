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

let uncycle eq seq =
  let first, rest = Seq.uncons seq |> Option.get in
  Seq.take_while (eq first >> not) rest |> Seq.cons first

let lcm a b =
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
  in
  a * b / gcd a b


let part1 input = 
  let directions, map = input |> parse_string parse_input in
  directions |> List.to_seq |> Seq.cycle 
  |> Seq.scan (take_step map) (Hashtbl.find map "AAA")
  |> Seq.take_while (fun n -> String.equal "ZZZ" n.label |> not)
  |> Seq.length

let part2 input = 
  let directions, map = input |> parse_string parse_input in
  Hashtbl.to_seq_keys map |> Seq.filter (fun l -> l.[2] == 'A')
  |> Seq.map (fun s ->
    directions |> List.to_seq |> Seq.cycle 
    |> Seq.scan (fun (i, _d, from) d -> i + 1, d, take_step map from d) (0, 'L', Hashtbl.find map s)
    |> Seq.filter (fun (_, _, n) -> n.label.[2] == 'Z')
    |> uncycle (fun (_, d1, n1) (_, d2, n2) -> d1 == d2 && String.equal n1.label n2.label)
    |> Seq.map (fun (i, _, _) -> i))
  |> Seq.fold_left (Seq.map_product lcm) ([1] |> List.to_seq)
  |> Seq.fold_left min Int.max_int