let () = print_endline "Hello from question 3"

let read_to_lines file =
  let rec f ic =
    try
      let line = input_line ic in
      let len = String.length line in
      let mp = len / 2 in
      let arr1 = Array.init mp (fun x -> line.[x]) in
      let arr2 = Array.init mp (fun x -> line.[mp + x]) in
      (arr1, arr2) :: f ic
    with
    | End_of_file -> []
  in
  f (open_in file)
;;

let prior_of_char c =
  let t = int_of_char c in
  if t < int_of_char 'a' then t - int_of_char 'A' + 27 else t - int_of_char 'a' + 1
;;

let read_to_lines_pt2 file =
  let rec f ic =
    try
      let line = input_line ic in
      let len = String.length line in
      let arr = Array.init len (fun x -> prior_of_char line.[x]) in
      arr :: f ic
    with
    | End_of_file -> []
  in
  f (open_in file)
;;

module IntSet = Set.Make (Int)

let find_common line =
  let l, r = line in
  let common = ref IntSet.empty in
  for i = 0 to Array.length l - 1 do
    if Array.mem l.(i) r then common := IntSet.add (prior_of_char l.(i)) !common else ()
  done;
  IntSet.fold (fun x r -> x + r) !common 0
;;

let set_of_array x = IntSet.of_seq (Array.to_seq x)

let find_badge a b c =
  let x = IntSet.inter (IntSet.inter a b) c in
  IntSet.choose x
;;

let rec find_badges = function
  | a :: b :: c :: rst ->
    find_badge (set_of_array a) (set_of_array b) (set_of_array c) :: find_badges rst
  | [] -> []
  | _ -> raise (Invalid_argument "Input is not multiple of 3 lines")
;;

let file = "inputs/q03.txt"
let input = read_to_lines file

let () =
  List.map find_common input
  |> List.fold_left (fun acc x -> acc + x) 0
  |> string_of_int
  |> Printf.printf "Part 1: %s\n"
;;

let () =
  find_badges (read_to_lines_pt2 file)
  |> List.fold_left (fun acc x -> acc + x) 0
  |> string_of_int
  |> Printf.printf "Part 2: %s\n"
;;
