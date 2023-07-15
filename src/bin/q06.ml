open Base [@@warning "-33"]
open Stdio
open Aocutils

let () = print_endline "Hello from question 06"

let rec scan_chars = function
  | a :: b :: c :: d :: rst ->
    if List.contains_dup [ a; b; c; d ] ~compare:Char.compare
    then 1 + scan_chars (b :: c :: d :: rst)
    else 4
  | _ -> raise (Invalid_argument "reached end of input")
;;

let rec scan_chars_pt2 input =
  let left, _ = List.split_n input 14 in
  if List.contains_dup left ~compare:Char.compare
  then 1 + scan_chars_pt2 (List.tl_exn input)
  else 14
;;

let file = "inputs/q06.txt"
let input_list = String.to_list (read_file file)
let x = scan_chars input_list
let () = printf "Part 1: %d\n" x
let y = scan_chars_pt2 input_list
let () = printf "Part 2: %d\n" y
