open Util

let is_blank x = String.length x == 0
let numbers_of_string = String.split_on_char ' ' >> List.filter (is_blank >> not) >> List.map int_of_string

let numbers_of_line line = 
  List.nth (String.split_on_char ':' line) 1 |> numbers_of_string 

let hard_floor x = (if Float.is_integer x then (Float.sub x 1.) else Float.floor x) |> int_of_float
let hard_ceil x = (if Float.is_integer x then (Float.add x 1.) else Float.ceil x) |> int_of_float

let part1 input = 
  let numbers = input |> List.map numbers_of_line 
  in
  List.map2 (fun t d -> (
    let delta = sqrt (float_of_int (t * t - 4 * d)) in
    let left = hard_ceil (Float.div (Float.sub (float_of_int t) delta) 2.) in
    let right = hard_floor (Float.div (Float.add (float_of_int t) delta) 2.) in
    right - left + 1
  )) (List.nth numbers 0) (List.nth numbers 1)
  |> List.fold_left Int.mul 1
