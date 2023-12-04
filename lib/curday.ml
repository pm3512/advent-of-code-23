module IntSet = Set.Make(Int)

let store_winning number_str =
  let numbers = Str.split (Str.regexp " +") number_str in
  List.fold_left (fun set num -> IntSet.add (int_of_string num) set) IntSet.empty numbers

let points winning number_str =
  let numbers = Str.split (Str.regexp " +") number_str in
  List.fold_left (fun pts num ->
    if IntSet.mem (int_of_string num) winning then
      match pts with
      | 0 -> 1
      | _ -> pts * 2
    else pts
  ) 0 numbers

let get_score_line ~scoring_f line =
  let parts = Str.split (Str.regexp ": ") line in
  let numbers = List.nth parts 1 in
  let parts = Str.split (Str.regexp " | ") numbers in
  let winning = store_winning (List.nth parts 0) in
  let pts = scoring_f winning (List.nth parts 1) in
  pts
  
let solve_part_a lines = 
  let line_scores = List.map (get_score_line ~scoring_f:points) lines in
  let total_score = List.fold_left (fun acc cur -> acc + cur) 0 line_scores in
  (
    print_int total_score;
    print_newline ();
  )

let count_winning winning number_str =
  let numbers = Str.split (Str.regexp " +") number_str in
  List.fold_left (fun pts num ->
    if IntSet.mem (int_of_string num) winning then
      pts + 1
    else pts
  ) 0 numbers

let solve_part_b lines = 
  let num_cards = List.length lines in
  let matches = List.map (get_score_line ~scoring_f:count_winning) lines in
  let dp = Array.init num_cards (fun _ -> 1) in
  let () = List.iteri (
    fun i count -> (
      let cur_count = dp.(i) in
      for j = i + 1 to i + count do
        dp.(j) <- dp.(j) + cur_count
      done
    )
  ) matches in
  let sum = Array.fold_left (fun acc cur -> acc + cur) 0 dp in
  (
    print_int sum;
    print_newline ();
  )