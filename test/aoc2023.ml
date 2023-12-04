open Aoc

type solver = int * ((string list -> int) * (string list -> int)) 

let () = 
  (* added explicit day indices so i can comment the previous days out while debugging *)
  let solvers : solver list = [ 
    1, (Day01.part1, Day01.part2);
    2, (Day02.part1, Day02.part2);
    3, (Day03.part1, Day03.part2);
    4, (Day04.part1, Day04.part2);
  ] in
  
  let _ = solvers |> List.map (fun (i, (p1, p2)) -> 
    let ind = string_of_int i in
    let input = Util.read_all_lines ("inputs/input" ^ ind ^ ".txt") in
    print_endline ("Day " ^ ind ^ ": " ^ string_of_int (p1 input) ^ " " ^ string_of_int (p2 input)))
  in
  ()