open Aoc2023.Util
open Aoc2023.Parsing

let parse_sint = (p_new (fun neg i -> if Option.is_some neg then -i else i) 
  |= p_str_opt "-" |= p_int)

let parse_input = p_any (p_more (parse_sint |. p_sps) |. p_lf_opt)

let rec diff = function
| x1 :: x2 :: xs -> (x1 - x2) :: diff (x2 :: xs)
| _ -> []

let extrapolate nums = 
  let rec diffseq l =
    if List.for_all (Int.equal 0) l then [] else l :: diffseq (diff l)
  in
  diffseq (List.rev nums) |> List.map List.hd |> sum


let part1 input = parse_string parse_input input
|> List.map extrapolate
|> sum

let part2 input = parse_string parse_input input
|> List.map (List.rev >> extrapolate)
|> sum