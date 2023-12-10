open Aoc2023.Util
open Aoc2023.Parsing

let parse_input = p_new (fun ls l -> (ls @ [l]) |> List.map Array.of_list |> Array.of_list)
|= p_any (p_end_lf) |= p_end_lf_opt

let find_start = Array.mapi (fun i row -> i, 
  row |> Array.mapi (fun i col -> if col == 'S' then Some i else None) 
  |> Array.find_opt Option.is_some |> Option.join)
>> Array.find_opt (snd >> Option.is_some) >> Option.get
>> (fun (i, j) -> i, Option.get j)

let loop graph = 
  let sr, sc = find_start graph in
  let get_opt i a = if i < 0 || i >= Array.length a then None else Some (Array.get a i) in
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
    (nr, nc) :: match Array.get (Array.get graph nr) nc with
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

let keep_coords coords = Array.mapi (fun r -> Array.mapi (fun c e -> 
    if List.exists (fun (cr, cc) -> cr == r && cc == c) coords then e else '.'))

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
    | y, x -> raise (Invalid_argument (string_of_char_list [y; x])))
  in
  List.map (scan false '.') >> sum


let part1 input = (parse_string parse_input input |> loop |> List.length) / 2
let part2 input = 
  let graph = parse_string parse_input input in
  let loop = loop graph in

  let (d1r, d1c), (sr, sc), (d2r, d2c) = List.hd loop, List.nth loop (List.length loop - 1), List.nth loop (List.length loop - 2) in
  let start = node_of_dirs (d1r - sr, d1c - sc) (d2r - sr, d2c - sc) in
  graph |> Array.mapi (fun r -> Array.mapi (fun c e -> if r == sr && c == sc then start else e))
  |> keep_coords loop
  |> Array.map Array.to_list |> Array.to_list 
  |> enclosed_tiles