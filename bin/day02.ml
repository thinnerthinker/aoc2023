open Util

type color = Red | Green | Blue
type cube_set_unnorm = (int * color) list
type cube_set = {r: int; g: int; b: int}
type game = int * cube_set list

exception ColorError of string

let color_of_string = function
| "red" -> Red
| "green" -> Green
| "blue" -> Blue
| _ -> raise (ColorError "Bro invalid color.")

let cube_set_unnorm_of_string str : cube_set_unnorm =
  String.split_on_char ',' str |> List.map (fun str ->
    let segments = String.split_on_char ' ' str in
    List.nth segments 1 |> int_of_string, List.nth segments 2 |> color_of_string)

let normalize_cube_set (unnorm: cube_set_unnorm) : cube_set = 
  let sum_of_color color = unnorm |> List.filter (fun c -> snd c == color) |> List.map fst |> sum in
  { r = sum_of_color Red; g = sum_of_color Green; b = sum_of_color Blue }

let game_of_line line : game = 
  let segments = String.split_on_char ':' line in
  let label, colors = List.nth segments 0, List.nth segments 1 in

  let id = List.nth (String.split_on_char ' ' label) 1 |> int_of_string in
  let color_lists = String.split_on_char ';' colors |> List.map (cube_set_unnorm_of_string >> normalize_cube_set) in

  id, color_lists

let is_game_possible (game: game) = 
  let is_impossible {r; g; b} = r > 12 || g > 13 || b > 14 in 
  snd game |> List.exists is_impossible |> not

let unzip (l: cube_set list) = 
  List.map (fun x -> x.r) l, List.map (fun x -> x.g) l, List.map (fun x -> x.b) l

let power_of_game (game: game) = 
  let r, g, b = snd game |> unzip in
  let maxc = List.fold_left max 0 in
  maxc r * maxc g * maxc b


let part1 input = input
  |> List.map game_of_line
  |> List.filter is_game_possible
  |> List.map fst
  |> sum
  
let part2 input = input
  |> List.map game_of_line
  |> List.map power_of_game
  |> sum
