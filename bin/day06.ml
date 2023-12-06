open Util

let is_blank x = String.length x == 0
let numbers_of_string = String.split_on_char ' ' >> List.filter (is_blank >> not) >> List.map int_of_string

let numbers_of_line line = List.nth (String.split_on_char ':' line) 1 |> numbers_of_string 

let hard_floor x = (if Float.is_integer x then x +. 1. else Float.floor x) |> int_of_float
let hard_ceil x = (if Float.is_integer x then x +. 1. else Float.ceil x) |> int_of_float


let solve build_numbers input = 
  let numbers = input |> List.map (numbers_of_line >> build_numbers)
  in
  List.map2 (fun t d -> (
    let delta = sqrt (float_of_int (t * t - 4 * d)) in
    hard_ceil ((float_of_int t -. delta) /. 2.) - hard_floor ((float_of_int t +. delta) /. 2.) + 1
  )) (List.nth numbers 0) (List.nth numbers 1)
  |> List.fold_left Int.mul 1

let part1 = solve Fun.id
let part2 = solve (fun ns -> [List.fold_left (fun a n -> a ^ string_of_int n) "" ns |> int_of_string])