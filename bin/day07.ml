open Aoc2023.Util
open Aoc2023.Parsing

let id_of_suit s = match s with '0'..'9' -> s | 'T' -> 'A' | 'J' -> 'B' | 'Q' -> 'C' | 'K' -> 'D' | 'A' -> 'E' | _ -> raise Unreachable
let type_of_card = List.sort Char.compare 
>> List.to_seq >> Seq.group Char.equal >> Seq.map Seq.length 
>> List.of_seq >> List.sort Int.compare >> List.rev
>> function [5] -> 'Z' | [4; 1] -> 'Y' | [3; 2] -> 'X' | [3; 1; 1] -> 'W' | [2; 2; 1] -> 'V' | [2; 1; 1; 1] -> 'U' | _ -> 'T'

let parse_input = p_any (p_new (fun t s -> t, s)
|= p_ifs c_alnum +> List.map id_of_suit +> (fun c -> type_of_card c :: c) +> string_of_char_list 
|. p_sps
|= p_int |. p_lf_opt)

let fart1 input = input 
|> parse_string parse_input

let part1 input = input 
|> parse_string parse_input
|> List.sort (fun a b -> String.compare (fst a) (fst b))
|> List.mapi (fun i (_, s) -> (i + 1) * s)
|> sum