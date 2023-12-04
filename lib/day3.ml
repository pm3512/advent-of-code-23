type number_details = {
  line_idx: int;
  first: int;
  last: int;
  number: int
}

module Indices = Set.Make(struct type t = (int * int) let compare = Util.pair_compare end)
module IndicesMap = Map.Make(struct type t = (int * int) let compare = Util.pair_compare end)

let rec get_number_details ?(start=0) line_idx line =
  let regex = Str.regexp "[0-9]+" in
  match Str.search_forward regex line start with
  | i -> (
    let last = Str.match_end () in
    let len = last - i in
    {line_idx = line_idx; first = i; last = last - 1; number = int_of_string (String.sub line i len)}
    :: (get_number_details ~start:(last) line_idx line)
  )
  | exception Not_found -> []

let get_neighbors ~line_num ~first ~last =
  let len = last - first + 1 in
  let same_row = [(line_num, first - 1); (line_num, last + 1)] in
  let prev_row = List.init (len + 2) (fun i -> (line_num - 1, first - 1 + i)) in
  let next_row = List.init (len + 2) (fun i -> (line_num + 1, first - 1 + i)) in
  let all = prev_row @ same_row @ next_row in
  List.filter (fun (y, x) -> y >= 0 && x >= 0) all

let rec get_character_indices ?(start=0) line_idx line =
  let regex = Str.regexp "[*#+$/@%=&-]" in
  match Str.search_forward regex line start with
  | i -> (
    (line_idx, i) :: (get_character_indices ~start:(i + 1) line_idx line)
  )
  | exception Not_found -> []


let character_index_set lines = List.fold_left (
  fun (set: Indices.t) (line_idx, line) ->
    let char_idx = get_character_indices line_idx line in
    let newset = List.fold_left (fun (set: Indices.t) idx -> Indices.add idx set) set char_idx in
    newset
) Indices.empty lines

let line_sum set line_idx line =
  let details = get_number_details line_idx line in
  let filtered = List.filter (
    fun detail ->
      let neighbors = get_neighbors ~line_num:detail.line_idx ~first:detail.first ~last:detail.last in
      List.exists (fun neighbor -> Indices.mem neighbor set) neighbors
  ) details in
  let sum = List.fold_left (fun acc cur -> acc + cur.number) 0 filtered in
  sum


let solve_part_a lines = 
  let enumerated = List.mapi (fun i line -> (i, line)) lines in
  let set = character_index_set enumerated in
  let line_sums = List.map (fun (i, line) -> line_sum set i line) enumerated in
  let sum =List.fold_left Int.add 0 line_sums in
  (
    print_int sum;
    print_newline ();
  )
  
let rec get_character_idx_b ?(start=0) line_idx line =
  let regex = Str.regexp "*" in
  match Str.search_forward regex line start with
  | i -> (
    (line_idx, i) :: (get_character_idx_b ~start:(i + 1) line_idx line)
  )
  | exception Not_found -> []

let character_index_map lines = List.fold_left (
  fun (map: (int list) IndicesMap.t) (line_idx, line) ->
    let char_idx = get_character_idx_b line_idx line in
    let newset = List.fold_left (fun (map: (int list) IndicesMap.t) idx -> IndicesMap.add idx [] map) map char_idx in
    newset
) IndicesMap.empty lines

let line_reduce map line_idx line =
  let details = get_number_details line_idx line in
  let newmap = List.fold_left (
    fun map detail ->
      let neighbors = get_neighbors ~line_num:detail.line_idx ~first:detail.first ~last:detail.last in
      List.fold_left (
        fun (map: (int list) IndicesMap.t) neighbor ->  
          let v = IndicesMap.find_opt neighbor map in
          match v with
          | None -> map
          | Some l -> IndicesMap.add neighbor (detail.number::l) map 
      ) map neighbors
  ) map details in
  newmap

let solve_part_b lines =
  let enumerated = List.mapi (fun i line -> (i, line)) lines in
  let map_start = character_index_map enumerated in
  let map_end = List.fold_left (
    fun map (line_idx, line) -> line_reduce map line_idx line
  ) map_start enumerated in
  let filtered = IndicesMap.filter (
    fun _ l -> List.length l = 2
  ) map_end in
  let sum = IndicesMap.fold (
    fun _ l sum -> (List.nth l 0) * (List.nth l 1) + sum
  ) filtered 0 in
  (
    print_int sum;
    print_newline ();
  )
