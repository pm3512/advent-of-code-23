type counts = {
  red: int;
  green: int;
  blue: int
}

let limits = {
  red = 12;
  green = 13;
  blue = 14;
}

let parse_draw (draw: string) = 
  let parts = Str.split (Str.regexp " ") draw in
  let num = int_of_string (List.nth parts 0) in
  let color = List.nth parts 1 in (num, color)

let is_valid_draw (limits: counts) (draw: string) = 
  let num, color = parse_draw draw in match color with
  | "red" -> num <= limits.red
  | "green" -> num <= limits.green
  | "blue" -> num <= limits.blue
  | _ -> raise (Failure ("Invalid color: " ^ color))

let is_valid_game (limits: counts) (line: string) =
  let game = List.nth (Str.split (Str.regexp ": ") line) 1 in
  let no_colons = Str.global_replace (Str.regexp ",") ";" game in
  let draws = Str.split (Str.regexp "; ") no_colons in
  let all_valid = List.for_all (is_valid_draw limits) draws in
  all_valid
  
let solve_part_a lines = 
  let are_valid = List.map (is_valid_game limits) lines in
  (*let () = List.iter (fun b -> print_string (string_of_bool b); print_newline ()) are_valid in *)
  let ids = List.mapi (fun i is_valid -> if is_valid then i + 1 else 0) are_valid in
  let sum = List.fold_left Int.add 0 ids in
  (
    print_int sum;
    print_newline ();
  )

let update_counts (counts: counts) (draw: string) = 
  let num, color = parse_draw draw in
  match color with
  | "red" -> {counts with red=if num > counts.red then num else counts.red}
  | "green" -> {counts with green=if num > counts.green then num else counts.green}
  | "blue" -> {counts with blue=if num > counts.blue then num else counts.blue}
  | _ -> (raise (Failure ("Invalid color: " ^ color)))

let get_counts (round: string) =
  let parts = Str.split (Str.regexp ", ") round in
  let start_counts = {red = 0; green = 0; blue = 0} in
  let final_counts = List.fold_left update_counts start_counts parts in
  final_counts

let merge_counts (counts1: counts) (counts2: counts) = {
  red = max counts1.red counts2.red;
  green = max counts1.green counts2.green;
  blue = max counts1.blue counts2.blue;
}

let get_power (line: string) = 
  let start_counts = {red = 0; green = 0; blue = 0} in
  let game = List.nth (Str.split (Str.regexp ": ") line) 1 in
  let rounds = Str.split (Str.regexp "; ") game in
  let final_counts = List.fold_left (fun acc cur -> merge_counts acc (get_counts cur)) start_counts rounds in
  final_counts.red * final_counts.green * final_counts.blue




let solve_part_b lines =
  let power = List.fold_left (fun acc cur -> acc + (get_power cur)) 0 lines in
  (
    print_int power;
    print_newline ();
  )