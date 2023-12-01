let to_chars str = List.init (String.length str) (String.get str)

let char_to_int ch = match ch with
| '0' .. '9' -> (Some (int_of_char ch - (int_of_char '0')))
| _ -> None