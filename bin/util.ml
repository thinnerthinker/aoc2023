let (>>) f g x = g (f x)

let char_list_of_string = String.to_seq >> List.of_seq
let sum = List.fold_left (+) 0

exception Unreachable