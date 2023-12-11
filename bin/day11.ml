open Aoc2023.Util
open Aoc2023.Parsing

let parse = String.split_on_char '\n' >> List.mapi (fun y -> char_list_of_string 
  >> List.mapi (fun x c -> if c == '#' then Some (x, y) else None)
  >> List.filter_map Fun.id)
>> List.flatten

let expand_by factor property make_new l = 
  let rec exp acc = function
  | x1 :: x2 :: xs -> 
    let d = x2 - x1 in
    let e = acc + if d <= 1 then 0 else (d - 1) * (factor - 1) in
    e :: exp e (x2 :: xs)
  | _ -> []
  in
  let l = List.sort (fun a b -> Int.compare (property a) (property b)) l in
  List.map2 (fun e x -> make_new (e + property x) x) (0 :: exp 0 (List.map property l)) l

let map_hproduct2 mapping l = 
  let rec map = function
  | x :: xs -> List.map (mapping x) xs :: map xs
  | [] -> [[]]
  in
  map l |> List.flatten


let solve factor input = 
  let galaxies = parse input 
  |> expand_by factor fst (fun x p -> x, snd p)
  |> expand_by factor snd (fun y p -> fst p, y)
  in
  map_hproduct2 (fun (x1, y1) (x2, y2) -> abs (x2 - x1) + abs (y2 - y1)) galaxies
  |> sum

let part1, part2 = solve 2, solve 1_000_000