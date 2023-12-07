open Aoc2023.Util
open Aoc2023.Parsing

let id_of_suit s = match s with '0'..'9' -> s | 'T' -> 'A' | 'J' -> 'B' | 'Q' -> 'C' | 'K' -> 'D' | 'A' -> 'E' | _ -> raise Unreachable

let groups_of_hand = List.sort Char.compare 
>> List.to_seq >> Seq.group Char.equal >> Seq.map Seq.length 
>> List.of_seq >> List.sort Int.compare >> List.rev

let strength_of_groups = function [5] -> 'Z' | [4; 1] -> 'Y' | [3; 2] -> 'X' | [3; 1; 1] -> 'W' | [2; 2; 1] -> 'V' | [2; 1; 1; 1] -> 'U' | _ -> 'T'

let parse_input transform = p_any (p_new (fun t s -> t, s)
|= p_ifs c_alnum +> transform +> string_of_char_list 
|. p_sps
|= p_int |. p_lf_opt)

let joker_up jokers groups = match jokers with
| 0 -> groups
| 1 -> (match groups with
  | [4] -> [5]
  | [3; 1] -> [4; 1]
  | [2; 2] -> [3; 2]
  | [2; 1; 1] -> [3; 1; 1]
  | [1; 1; 1; 1] | _ -> [2; 1; 1; 1])
| 2 -> (match groups with
  | [3] -> [5]
  | [2; 1] -> [4; 1]
  | [1; 1; 1] | _ -> [3; 1; 1])
| 3 -> (match groups with
  | [2] -> [5]
  | [1; 1] | _ -> [4; 1])
| 4 | 5 | _ -> [5]


let solve transform = parse_string (parse_input transform)
>> List.sort (fun a b -> String.compare (fst a) (fst b))
>> List.mapi (fun i (_, s) -> (i + 1) * s)
>> sum

let part1 = solve ((List.map id_of_suit) >> (fun h -> (h |> groups_of_hand |> strength_of_groups) :: h))
let part2 = solve (fun hand -> 
  let hand = hand |> List.map (fun c -> if c == 'J' then '0' else c) |> List.map id_of_suit in
  let no_jokers = hand |> List.filter (Char.equal '0' >> not) in
  let strength = no_jokers |> groups_of_hand |> joker_up (List.length hand - List.length no_jokers) |> strength_of_groups in
  strength :: hand)