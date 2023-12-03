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

let (>>) f g x = g (f x)

let char_list_of_string = String.to_seq >> List.of_seq
let sum = List.fold_left (+) 0
