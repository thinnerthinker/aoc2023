open Aoc2023.Util
open Aoc2023.Parsing

let parse_card = p_new (fun w d -> w, d)
|. p_str "Card" |. p_sps |. p_int |. p_str ":" |. p_sps
|= p_any (p_int |. p_sps)
|. p_str "|" |. p_sps
|= p_any (p_int |. p_sps)

let matches_of_card (winner, drawn) = drawn 
  |> List.filter (fun d -> winner |> List.exists (fun w -> d == w))
  |> List.length 

type multicard = { range: int; count: int }

let counts_of_multicards =
  let yield_of_multicards mcs =
    (mcs |> List.filteri (fun i mc -> mc.range - i > 0) 
    |> List.map (fun mc -> mc.count) |> sum) + 1
  in
  List.fold_left (fun cs m -> ({ range = m; count = yield_of_multicards cs } :: cs)) [] 
  >> List.map (fun mc -> mc.count) 


let solve transform = parse_string (p_any (parse_card |. p_lf_opt)) >> List.map matches_of_card >> transform >> sum

let part1 = solve (List.map (fun m -> if m == 0 then 0 else Int.shift_left 1 (m - 1)))
let part2 = solve counts_of_multicards