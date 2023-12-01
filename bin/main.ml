type part = A | B

let usage_msg = "-d <day number> -p <part (a/b)> [--test]"

let day = ref 1
let part_string = ref ""
let is_test = ref false

let speclist = 
  [
    ("-d", Arg.Set_int day, "Number of current day");
    ("-p", Arg.Set_string part_string, "Current part (a or b)");
    ("--test", Arg.Set is_test, "Whether to run on test input or real input");
  ]

let () = Arg.parse speclist (fun _ -> ()) usage_msg

let part = match !part_string with
| "a"
| "A" -> A
| "b"
| "B" -> B
| _ -> raise (Failure "Invalid part")

let part_to_string part = match part with
| A -> "a"
| B -> "b"

let input_file = String.concat "" [ "inputs/day_";
string_of_int !day;
"/";
part_to_string part;
if !is_test then "_test" else "" ;
".txt" ]

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; List.rev !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines;;

let lines = read_file input_file

let () = match part with
| A -> Aoc23.Curday.solve_part_a lines
| B -> Aoc23.Curday.solve_part_b lines

