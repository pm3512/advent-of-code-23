let get_number (line: string): int = 
  let chars = Util.to_chars line in
  let digits = List.filter_map Util.char_to_int chars in
  let last, first = (List.nth digits ((List.length digits) - 1), List.nth digits 0 ) in
  first * 10 + last

let solve_part_a lines =
  let sum, _ = List.fold_left_map (fun acc line ->
    let num = get_number line in
    (acc + num, num)
  ) 0 lines in
  (
    print_int sum;
    print_newline ()
  )

let number_word_strings = [
  "zero";
  "one";
  "two";
  "three";
  "four";
  "five";
  "six";
  "seven";
  "eight";
  "nine";
]

let indices = (List.init 10 (fun x -> x))
let number_chars = List.map string_of_int indices
let numbers = number_chars @ number_word_strings

let str_to_int num_str =
  let indices = (List.init 20 (fun x -> x)) in
  try let idx = List.find (fun idx -> (List.nth numbers idx) = num_str) indices in
  idx mod 10
with Not_found -> raise (Failure ("Not a valid number: " ^ num_str))

let find_first_last line number =
  let re = Str.regexp_string number in
  let len = String.length line in
  try
    let first = Str.search_forward re line 0 in
    let last = Str.search_backward re line (len - 1) in
    (number, first, last)
  with Not_found -> (number, len, -1)

let get_number_b (line: string): int = 
  let finder = find_first_last line in
  let len = String.length line in
  let entries = List.map finder numbers in
  let (first, last, _, _) = List.fold_left (
    fun (first, last, first_idx, last_idx) (number, cur_first_idx, cur_last_idx) ->
      let new_first = if cur_first_idx < first_idx then number else first in
      let new_first_idx = if cur_first_idx < first_idx then cur_first_idx else first_idx in
      let new_last = if cur_last_idx > last_idx then number else last in
      let new_last_idx = if cur_last_idx > last_idx then cur_last_idx else last_idx in
      (new_first, new_last, new_first_idx, new_last_idx)
  ) ("0", "0", len, -1) entries in
  let number = (str_to_int first) * 10 + (str_to_int last) in
  number
  

let solve_part_b lines = 
  let sum, _ = List.fold_left_map (fun acc line ->
    let num = get_number_b line in
    (acc + num, num)
  ) 0 lines in
  (
    print_int sum;
    print_newline ()
  )
