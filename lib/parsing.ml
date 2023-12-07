open Util

let char_list_of_string = String.to_seq >> List.of_seq
let string_of_char_list = List.to_seq >> String.of_seq 


type 'a parser = char list -> 'a * char list

let parse (parser: 'a parser) str = (parser str) |> fst
let parse_string parser str = parse parser (char_list_of_string str)

let p_any parser str = 
  let rec apply acc str =
    try
      let parsed, rest = parser str in
      apply (parsed :: acc) rest
    with _ -> List.rev acc, str
  in
  apply [] str

exception EitherError
let p_either l r str = 
  try 
    let parsed, rest = l str in
    Either.Left parsed, rest
  with _ -> try
    let parsed, rest = r str in
    Either.Right parsed, rest
  with _ -> raise EitherError

let p_map mapping parser str =
  let parsed, rest = parser str in
  mapping parsed, rest

let p_feed input transform str = 
  let parsed, rest = input str in
  parse transform parsed, rest


let (|=) (l: ('a -> 'b) parser) (r: 'a parser) : 'b parser = 
  fun str ->
    let func, rest = l str in
    let parsed, rest = r rest in
    func parsed, rest

let (|.) (l: 'keep parser) (r: 'ignore parser) : 'keep parser = 
  fun str ->
    let parsed, rest = l str in
    let _, rest = r rest in
    parsed, rest

let (^?) left right = p_either left right
let (+>) parser mapping = p_map mapping parser
let (+|) = p_feed


let p_new builder str = builder, str

exception OfError
let p_of pred = function
| c :: cs when pred c -> c, cs
| _ -> raise OfError

let p_if pred = function
| c :: cs when pred c -> Some c, cs
| cs -> None, cs

let p_ifs pred str = 
  let rec take acc = function
  | x :: xs when pred x -> take (x :: acc) xs
  | xs -> List.rev acc, xs
  in
  take [] str

exception AnyError
let p_more parser = p_any parser +> function [] -> raise AnyError | p -> p

let c_digit = function '0'..'9' -> true | _ -> false
let c_alpha = function 'A'..'Z' | 'a'..'z' -> true | _ -> false
let c_alnum c = c_digit c || c_alpha c
let c_eq = Char.equal
let c_sp, c_lf = c_eq ' ', c_eq '\n'
let c_any _c = true


exception IntError
let p_int str = 
  let i, str = p_ifs c_digit str in
  match i with 
  | [] -> raise IntError 
  | i -> List.fold_left (fun acc c -> acc * 10 + Char.code c - Char.code '0') 0 i, str

let p_str_opt to_match str = 
  let rec matching pattern str' = match pattern, str' with
  | p :: ps, c :: cs when p == c -> matching ps cs
  | [], _ -> Some (), str'
  | _ -> None, str
  in
  matching (to_match |> char_list_of_string) str

exception StrError of string
let p_str to_match str = match p_str_opt to_match str with
| Some _, rest -> (), rest
| None, _ -> raise (StrError to_match)

let p_sp = p_str " "
let p_sps = p_ifs c_sp
let p_lf = p_str "\n"
let p_lf_opt = p_if c_lf

exception EndError
let p_end = function [] -> (), [] | _ -> raise EndError

let p_end_of pred str = 
  let parsed, rest = p_ifs (pred >> not) str in
  parsed, p_of pred rest |> snd
let p_end_if pred str = 
  let parsed, rest = p_ifs (pred >> not) str in
  parsed, p_if pred rest |> snd

let p_end_lf = p_end_of c_lf
let p_end_lf_opt = p_end_if c_lf
