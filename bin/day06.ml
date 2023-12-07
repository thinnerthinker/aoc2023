open Aoc2023.Parsing

let parse = p_new (fun t s -> t, s)
|. p_str "Time:" |. p_sps |= p_any (p_int |. p_sps) |. p_lf
|. p_str "Distance:" |. p_sps |= p_any (p_int |. p_sps)


let solve build_numbers input = 
  let time, dist = input |> parse_string parse
  in
  List.map2 (fun t d -> (
    let delta = sqrt (float_of_int (t * t - 4 * d)) in
    let t = float_of_int t in
    int_of_float (ceil ((t +. delta) /. 2. -. 1.) -. floor ((t -. delta) /. 2. +. 1.)) + 1
  )) (build_numbers time) (build_numbers dist)
  |> List.fold_left Int.mul 1

let part1 = solve Fun.id
let part2 = solve (fun ns -> [List.fold_left (fun a n -> a ^ string_of_int n) "" ns |> int_of_string])