open Util

let is_blank x = String.length x == 0
let numbers_of_string = String.split_on_char ' ' >> List.filter (is_blank >> not) >> List.map int_of_string

let is_digit = function '0'..'9' -> true | _ -> false

let split_on pred l =
  let rec split acc = function
  | x :: xs when pred x -> (List.rev acc) :: split [] xs
  | x :: xs -> split (x :: acc) xs 
  | [] -> [List.rev acc]
  in
  split [] l

type mapping = { dest: int; src: int; len: int }
exception MappingError

let parse input = 
  let hd_segments = String.split_on_char ':' (List.hd input) in
  let seeds = List.nth hd_segments 1 |> numbers_of_string in

  let mappings = input |> List.tl |> List.tl |> split_on is_blank |> List.map List.tl
  |> List.map (List.map (numbers_of_string >> 
    (function | [d; s; l] -> { dest = d; src = s; len = l } | _ -> raise MappingError)))
  in
  seeds, mappings

let apply_mappings seed mappings = mappings
  |> List.find_opt (fun m -> seed >= m.src && seed < m.src + m.len)
  |> function | Some m -> m.dest + seed - m.src | None -> seed

let part1 input = 
  let seeds, mappings = parse input in

  seeds 
  |> List.map (fun s -> List.fold_left apply_mappings s mappings)
  |> List.fold_left min Int.max_int