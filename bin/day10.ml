open Aoc2023.Util
open Aoc2023.Parsing

let parse_input = p_new (fun ls l -> (ls @ [l]) |> List.map Array.of_list |> Array.of_list)
|= p_any (p_end_lf) |= p_end_lf_opt

let find_start = Array.mapi (fun i row -> i, 
  row |> Array.mapi (fun i col -> if col == 'S' then Some i else None) 
  |> Array.find_opt Option.is_some |> Option.join)
>> Array.find_opt (snd >> Option.is_some) >> Option.get
>> (fun (i, j) -> i, Option.get j)

let loop_len graph = 
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
    1 + match Array.get (Array.get graph nr) nc with
    | 'S' -> 0
    | '|' | '-' -> walk (nr, nc) (dr, dc)
    | '7' | 'L' when dc != 0 -> walk (nr, nc) (dc, 0)
    | '7' | 'L' -> walk (nr, nc) (0, dr)
    | 'J' | 'F' when dc != 0 -> walk (nr, nc) (-dc, 0)
    | 'J' | 'F' -> walk (nr, nc) (0, -dr)
    | _ -> (raise Unreachable)
  in
  walk (sr, sc) start_dir

let part1 input = (parse_string parse_input input |> loop_len) / 2