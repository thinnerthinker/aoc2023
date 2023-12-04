open Util

exception Damn of string

let numbers_of_string = String.split_on_char ' ' >> List.filter (fun x -> String.length x > 0) >> List.map int_of_string

let card_of_line line = 
  let segments = String.split_on_char ':' line in
  let _label, numbers = List.nth segments 0, List.nth segments 1 in

  (* let id = List.nth (String.split_on_char ' ' label) 1 |> int_of_string in *)
  let numbers = String.split_on_char '|' numbers in
  let winner, drawn = List.nth numbers 0 |> numbers_of_string, List.nth numbers 1 |> numbers_of_string in

  winner, drawn

let score_of_card (winner, drawn) =
  let score = drawn 
  |> List.filter (fun d -> winner |> List.exists (fun w -> d == w))
  |> List.length in

  if score == 0 then 0 else Int.shift_left 1 (score - 1)

let part1 input = input
  |> List.map card_of_line
  |> List.map score_of_card
  |> sum