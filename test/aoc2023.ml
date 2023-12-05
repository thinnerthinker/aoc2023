open Aoc

let read_all_lines path = 
  let in_channel = open_in path in
  try
    let rec read_lines () =
      try
        let line = input_line in_channel in
        line :: read_lines ()
      with End_of_file -> []
    in
    read_lines ()
  with e ->
    close_in_noerr in_channel;
    raise e


type solver = int * (string list -> int) list

let () = 
  (* added explicit day indices so i can comment the previous days out while debugging *)
  let solvers : solver list = [ 
    1, [Day01.part1; Day01.part2];
    2, [Day02.part1; Day02.part2];
    3, [Day03.part1; Day03.part2];
    4, [Day04.part1; Day04.part2];
    5, [Day05.part1; Day05.part2];
  ] in
  
  let _ = solvers |> List.map (fun (i, ps) -> 
    let ind = string_of_int i in
    let input = read_all_lines ("inputs/input" ^ ind ^ ".txt") in
    print_endline ("Day " ^ ind ^ ": " ^ 
      (List.map (fun p -> string_of_int (p input)) ps |> String.concat " ")
    ))
  in
  ()