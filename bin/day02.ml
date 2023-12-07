open Aoc2023.Util
open Aoc2023.Parsing

type color = Red | Green | Blue
type cube_set_unnorm = (int * color) list
type cube_set = {r: int; g: int; b: int}
type game = int * cube_set list

let color_of_string = function
| "red" -> Red
| "green" -> Green
| "blue" -> Blue
| _ -> raise Unreachable

let parse_cube_set_unnorm = p_more (p_new (fun a c -> a, c)
  |= p_int |. p_sps
  |= p_ifs c_alpha +> string_of_char_list +> color_of_string
  |. p_str_opt ", ")

let normalize_cube_set (unnorm: cube_set_unnorm) : cube_set = 
  let sum_of_color color = unnorm |> List.filter (fun c -> snd c == color) |> List.map fst |> sum in
  { r = sum_of_color Red; g = sum_of_color Green; b = sum_of_color Blue }

let parse_game : game parser = p_new (fun i l -> i, l)
|. p_str "Game "
|= p_int |. p_str ": "
|= p_any (parse_cube_set_unnorm +> normalize_cube_set |. p_str_opt "; ")

let is_game_possible (game: game) = 
  let is_possible {r; g; b} = r <= 12 && g <= 13 && b <= 14 in 
  snd game |> List.for_all is_possible

let unzip (l: cube_set list) = 
  List.map (fun x -> x.r) l, List.map (fun x -> x.g) l, List.map (fun x -> x.b) l

let power_of_game (game: game) = 
  let r, g, b = snd game |> unzip in
  let maxc = List.fold_left max 0 in
  maxc r * maxc g * maxc b


let part1 input = input
|> parse_string (p_any (parse_game |. p_lf_opt))
|> List.filter is_game_possible
|> List.map fst
|> sum

let part2 input = input
|> parse_string (p_any (parse_game |. p_lf_opt))
|> List.map power_of_game
|> sum