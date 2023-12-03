open Util

let is_digit = function '0'..'9' -> true | _ -> false
let to_int_digit c = Char.code c - Char.code '0'

let find_all str key = 
  let strlen, keylen = String.length str, String.length key in
  let rec find i =
    if i + keylen > strlen then
      []
    else if String.equal (String.sub str i keylen) key then
      i :: find (i + 1)
    else find (i + 1)
  in
  find 0

let equiv = [
  (["one"; "1"], 1);
  (["two"; "2"], 2);
  (["three"; "3"], 3);
  (["four"; "4"], 4);
  (["five"; "5"], 5);
  (["six"; "6"], 6);
  (["seven"; "7"], 7);
  (["eight"; "8"], 8);
  (["nine"; "9"], 9);
] |> List.map (fun (l, d) -> List.map (fun x -> x, d) l) |> List.flatten

let first_last l = List.nth l 0, List.nth l (List.length l - 1)
let min_max l = if l == [] then (Int.max_int, Int.min_int) else first_last l
let extremum_by_key comp = List.fold_left (fun (ak, av) (k, v) -> if comp ak k then (ak, av) else (k, v))

let first_last_of_all_digits str =
  let mins, maxs = equiv 
  |> List.map (fun (ds, d) -> find_all str ds |> min_max, d)
  |> List.map (fun ((min, max), d) -> (min, d), (max, d)) 
  |> List.split in

  extremum_by_key (<) (Int.max_int, 0) mins |> snd, extremum_by_key (>) (Int.min_int, 0) maxs |> snd


let part1 input = input
  |> List.map char_list_of_string
  |> List.map (List.find_all is_digit 
    >> first_last 
    >> (fun (a, b) -> to_int_digit a * 10 + to_int_digit b))
  |> sum

let part2 input = input
  |> List.map first_last_of_all_digits
  |> List.map (fun (a, b) -> a * 10 + b)
  |> sum