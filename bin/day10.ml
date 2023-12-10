open Aoc2023.Util
open Aoc2023.Parsing

let parse = String.split_on_char '\n' >> List.map (char_list_of_string >> Array.of_list) >> Array.of_list

let find_start graph = 
  let index = ref (-1, -1) in
  Array.iteri (fun r -> Array.iteri (fun c e -> if e == 'S' then index := r, c)) graph;
  !index

let loop graph = 
  let sr, sc = find_start graph in
  let get_opt i a = if i < 0 || i >= Array.length a then None else Some a.(i) in
  let ind_opt r c = Option.bind (get_opt r graph) (get_opt c) in
  
  let start_dir = match ind_opt (sr - 1) sc, ind_opt (sr + 1) sc, ind_opt sr (sc - 1), ind_opt sr (sc + 1) with
  | Some '|', _, _, _ | Some '7', _, _, _ | Some 'F', _, _, _ -> -1, 0
  | _, Some '|', _, _ | _, Some 'J', _, _ | _, Some 'L', _, _ -> +1, 0
  | _, _, Some '-', _ | _, _, Some 'L', _ | _, _, Some 'F', _ -> 0, -1
  | _, _, _, Some '-' | _, _, _, Some 'J' | _, _, _, Some '7' -> 0, +1
  | _ -> (raise Unreachable)
  in

  let rec walk (r, c) (dr, dc) =
    let nr, nc = r + dr, c + dc in
    (nr, nc) :: match graph.(nr).(nc) with
    | 'S' -> []
    | '|' | '-' -> walk (nr, nc) (dr, dc)
    | '7' | 'L' when dc != 0 -> walk (nr, nc) (dc, 0)
    | '7' | 'L' -> walk (nr, nc) (0, dr)
    | 'J' | 'F' when dc != 0 -> walk (nr, nc) (-dc, 0)
    | 'J' | 'F' -> walk (nr, nc) (0, -dr)
    | _ -> (raise Unreachable)
  in
  walk (sr, sc) start_dir

let node_of_dirs d1 d2 = match d1, d2 with
| (0, _), (0, _) -> '-'
| (_, 0), (_, 0) -> '|'
| (-1, 0), (0, 1) | (0, 1), (-1, 0) -> 'L'
| (1, 0), (0, 1) | (0, 1), (1, 0) -> 'F'
| (-1, 0), (0, -1) | (0, -1), (-1, 0) -> 'J'
| (1, 0), (0, -1) | (0, -1), (1, 0) -> '7'
| _ -> raise Unreachable

let keep_coords coords graph =
  let new_graph = Array.make_matrix (Array.length graph) (Array.length graph.(0)) '.' in
  List.iter (fun (r, c) -> new_graph.(r).(c) <- graph.(r).(c)) coords;
  new_graph

let enclosed_tiles = 
  let rec scan inside last = function
  | [_] | [] -> 0
  | t :: ts -> (match last, t with
    | _, '.' -> (if inside then 1 else 0) + scan inside last ts
    | _, '-' -> scan inside last ts
    | _, '|' -> scan (not inside) last ts
    | 'J', 'L' | 'L', 'J' | '7', 'F' | 'F', '7' -> scan (not inside) '.' ts
    | 'J', 'F' | 'F', 'J' | '7', 'L' | 'L', '7' -> scan inside '.' ts
    | '.', bend -> scan (not inside) bend ts
    | _ -> raise Unreachable)
  in
  List.map (scan false '.') >> sum


let part1 input = (parse input |> loop |> List.length) / 2
let part2 input = 
  let graph = parse input in
  let loop = loop graph in

  let (d1r, d1c), (sr, sc), (d2r, d2c) = List.hd loop, List.nth loop (List.length loop - 1), List.nth loop (List.length loop - 2) in
  let start = node_of_dirs (d1r - sr, d1c - sc) (d2r - sr, d2c - sc) in

  graph.(sr).(sc) <- start;

  graph
  |> keep_coords loop
  |> Array.map Array.to_list |> Array.to_list 
  |> enclosed_tiles