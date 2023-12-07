open Aoc2023.Parsing

type mapping = { dest: int; src: int; len: int }
let end_m mapping = mapping.src + mapping.len

let parse_input = p_new (fun s ms -> s, ms)
|. p_str "seeds:" |. p_sps
|= p_any (p_int |. p_sps)
|. p_lf |. p_lf
|= p_any (p_new Fun.id
  |. p_end_lf
  |= p_any (p_new (fun d s l -> { dest = d; src = s; len = l; }) 
    |= p_int |. p_sps 
    |= p_int |. p_sps 
    |= p_int |. p_lf_opt)
  |. p_lf_opt)

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
  let seeds, mappings = input |> parse_string parse_input
  in
  build_range seeds
  |> List.map (fun s -> List.fold_left (fun ss m -> 
    List.map (fun s -> apply_mappings_to_range s m) ss |> List.flatten) [s] mappings)
  |> List.flatten |> List.map (fun s -> s.start)
  |> List.fold_left min Int.max_int

let part1 = solve (List.map (fun s -> { start = s; len = 1 }))
let part2 = solve (fun seeds -> 
  let of_parity p = List.filteri (fun i _ -> Int.rem i 2 == p) seeds in
  List.map2 (fun s l -> { start = s; len = l }) (of_parity 0) (of_parity 1))