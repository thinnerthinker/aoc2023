open Util

let is_blank x = String.length x == 0
let numbers_of_string = String.split_on_char ' ' >> List.filter (is_blank >> not) >> List.map int_of_string

let split_on pred l =
  let rec split acc = function
  | x :: xs when pred x -> (List.rev acc) :: split [] xs
  | x :: xs -> split (x :: acc) xs 
  | [] -> [List.rev acc]
  in
  split [] l

type mapping = { dest: int; src: int; len: int }
let end_m mapping = mapping.src + mapping.len
exception MappingError

let parse input = 
  let hd_segments = String.split_on_char ':' (List.hd input) in
  let seeds = List.nth hd_segments 1 |> numbers_of_string in

  let mappings = input |> List.tl |> List.tl |> split_on is_blank |> List.map List.tl
  |> List.map (List.map (numbers_of_string >> 
    (function | [d; s; l] -> { dest = d; src = s; len = l } | _ -> raise MappingError)))
  in
  seeds, mappings

type seed_range = { start: int; len: int }
let end_r range = range.start + range.len

let apply_mappings_to_range seed mappings = 
  let limit_to range mapping = 
    let ostart, oend = max range.start mapping.src, min (end_r range) (end_m mapping) in
    if ostart >= oend then None else Some { src = ostart; dest = mapping.dest + ostart - mapping.src; len = oend - ostart }
  in
  let mappings = mappings |> List.filter_map (limit_to seed) |> List.sort (fun x y -> Int.compare x.src y.src) in
  let holes = List.map2 (fun s e -> { start = s; len = e - s })
    (seed.start :: (List.map end_m mappings)) ((List.map (fun m -> m.src) mappings) @ [end_r seed])
    |> List.filter (fun s -> s.len > 0)
  in
  holes @ List.map (fun m -> { start = m.dest; len = m.len }) mappings


let solve build_range input = 
  let seeds, mappings = parse input in

  build_range seeds
  |> List.map (fun s -> List.fold_left (fun ss m -> 
    List.map (fun s -> apply_mappings_to_range s m) ss |> List.flatten) [s] mappings)
  |> List.flatten |> List.map (fun s -> s.start)
  |> List.fold_left min Int.max_int

let part1 = solve (List.map (fun s -> { start = s; len = 1 }))
let part2 = solve (fun seeds -> (List.map2 (fun s l -> { start = s; len = l }) 
  (List.filteri (fun i _ -> Int.rem i 2 == 0) seeds) (List.filteri (fun i _ -> Int.rem i 2 == 1) seeds)))