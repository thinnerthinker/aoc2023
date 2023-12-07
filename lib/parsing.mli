val char_list_of_string: string -> char list
val string_of_char_list: char list -> string

(* API largely inspired by Elm's parser package https://package.elm-lang.org/packages/elm/parser/latest/Parser, see closest LICENSE in hierarchy *)

type 'a parser

val parse: 'a parser -> char list -> 'a
val parse_string: 'a parser -> string -> 'a

val p_any: 'a parser -> 'a list parser
val p_either: 'left parser -> 'right parser -> ('left, 'right) Either.t parser
val p_map: ('a -> 'b) -> 'a parser -> 'b parser
val p_feed: char list parser -> 'a parser -> 'a parser

val (|=): ('a -> 'b) parser -> 'a parser -> 'b parser
val (|.): 'keep parser -> 'ignore parser -> 'keep parser
val (^?): 'left parser -> 'right parser -> ('left, 'right) Either.t parser
val (+>): 'a parser -> ('a -> 'b) -> 'b parser
val (+|): char list parser -> 'a parser -> 'a parser

val p_new: 'a -> 'a parser

val p_of: (char -> bool) -> char parser
val p_if: (char -> bool) -> char option parser
val p_ifs: (char -> bool) -> char list parser
val p_more: 'a parser -> 'a list parser

val c_digit: char -> bool
val c_alpha: char -> bool
val c_alnum: char -> bool
val c_eq: char -> char -> bool
val c_sp: char -> bool
val c_lf: char -> bool
val c_any: char -> bool

val p_int: int parser
val p_str_opt: string -> unit option parser
val p_str: string -> unit parser
val p_sp: unit parser
val p_sps: char list parser
val p_lf: unit parser
val p_lf_opt: char option parser
val p_end: unit parser

val p_end_of: (char -> bool) -> char list parser
val p_end_if: (char -> bool) -> char list parser

val p_end_lf: char list parser
val p_end_lf_opt: char list parser