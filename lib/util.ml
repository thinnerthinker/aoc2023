let (>>) f g x = g (f x)

let numbers_of_string = String.split_on_char ' ' >> List.filter (fun s -> String.length s > 0) >> List.map int_of_string

let sum = List.fold_left (+) 0

exception Unreachable