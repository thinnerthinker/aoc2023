open Util

let indexes_of pred = List.mapi (fun i x -> if pred x then i else -1) >> List.filter (fun x -> x >= 0)

let indexed (l: 'a list list) : (('a * int) list) list = List.mapi (fun i es -> List.map (fun e -> e, i) es) l

let part_coordinates_of_lines is_part lines = lines 
  |> List.map (indexes_of is_part)
  |> indexed
  |> List.flatten

let neighbors_of_coordinate (x, y) = 
  List.init 3 (fun dx -> List.init 3 (fun dy -> (x + dx - 1, y + dy - 1)))
  |> List.flatten

let number_ranges_of_line str = 
  let is_digit = function '0'..'9' -> true | _ -> false in
  let to_int_digit c = Char.code c - Char.code '0' in
  
  let rec numbers (s, e, n) = function
  | x :: xs when is_digit x -> numbers (s, e + 1, n * 10 + to_int_digit x) xs
  | _ :: xs -> if n != 0 then (s, e - 1, n) :: numbers (e + 1, e + 1, 0) xs else numbers (e + 1, e + 1, 0) xs
  | [] -> if n != 0 then [(s, e - 1, n)] else [] 
  in
  numbers (0, 0, 0) str

exception GearError of string

let part1 input = 
  let lines = input
  |> List.map char_list_of_string in
  
  let overlaps = lines
  |> part_coordinates_of_lines (function '0'..'9' | '.' -> false | _ -> true)
  |> List.map neighbors_of_coordinate
  |> List.flatten in

  lines
  |> List.map number_ranges_of_line
  |> indexed
  |> List.flatten
  |> List.filter (fun ((s, e, _), i) -> overlaps |> List.exists (fun (x, y) -> x >= s && x <= e && y == i))
  |> List.map (fun ((_, _, n), _) -> n)
  |> sum

let part2 input = 
  let lines = input
  |> List.map char_list_of_string in
  
  let numbers = lines
  |> List.map number_ranges_of_line
  |> indexed
  |> List.flatten in

  let numbers_from_overlaps neighborhood = numbers
    |> List.filter (fun ((s, e, _n), i) -> neighborhood 
      |> List.exists (fun (x, y) -> x >= s && x <= e && y == i)) in

  lines
  |> part_coordinates_of_lines (fun c -> c == '*')
  |> List.map (neighbors_of_coordinate >> numbers_from_overlaps)
  |> List.filter (fun l -> List.length l == 2)
  |> List.map (List.map (fun ((_, _, n), _) -> n))
  |> List.map (function | (a :: b :: _) -> a * b | _ -> raise (GearError "dumb gear bro."))
  |> sum