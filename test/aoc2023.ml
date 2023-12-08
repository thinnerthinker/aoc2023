open Aoc

let read_input path = 
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


type solver = int * (string -> int) list

let () = 
  (* added explicit day indices so i can comment the previous days out while debugging *)
  let solvers : solver list = [ 
    1, [Day01.part1; Day01.part2];
    2, [Day02.part1; Day02.part2];
    3, [Day03.part1; Day03.part2];
    4, [Day04.part1; Day04.part2];
    5, [Day05.part1; Day05.part2];
    6, [Day06.part1; Day06.part2];
    7, [Day07.part1; Day07.part2];
    8, [Day08.part1; ];
  ] in
  
  let _ = solvers |> List.map (fun (i, ps) -> 
    let ind = string_of_int i in
    let input = read_input ("inputs/input" ^ ind ^ ".txt") in
    print_endline ("Day " ^ ind ^ ": " ^ 
      (List.map (fun p -> string_of_int (p input)) ps |> String.concat " ")
    ))
  in
  ()