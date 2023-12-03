let to_chars str = List.init (String.length str) (String.get str)

let char_to_int ch = match ch with
| '0' .. '9' -> (Some (int_of_char ch - (int_of_char '0')))
| _ -> None

let pair_compare (a1, a2) (b1, b2) =
  if a1 < b1 || (a1 == b1 && a2 < b2) then -1
  else if a1 = b1 && a2 = b2 then 0
  else 1