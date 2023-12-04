open Util

let numbers_of_string = String.split_on_char ' ' >> List.filter (fun x -> String.length x > 0) >> List.map int_of_string

let card_of_line line = 
  let segments = String.split_on_char ':' line in
  let _label, numbers = List.nth segments 0, List.nth segments 1 in

  let numbers = String.split_on_char '|' numbers in
  let winner, drawn = List.nth numbers 0 |> numbers_of_string, List.nth numbers 1 |> numbers_of_string in

  winner, drawn

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


let part1 input = input
  |> List.map card_of_line
  |> List.map matches_of_card
  |> List.map (fun m -> if m == 0 then 0 else Int.shift_left 1 (m - 1))
  |> sum

let part2 input = input
  |> List.map card_of_line
  |> List.map matches_of_card
  |> counts_of_multicards
  |> sum