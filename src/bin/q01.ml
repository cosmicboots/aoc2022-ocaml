open Aocutils
open Printf

let () = print_endline "=== Question 01 ==="
let file = "inputs/q01.txt"

let sum_elf_bag bag =
  let rec f acc = function
    | [] -> acc, []
    | "" :: xs -> acc, xs
    | x :: xs -> f (acc + int_of_string x) xs
  in
  f 0 bag
;;

let rec sum_bags bags =
  let acc, rst = sum_elf_bag bags in
  match rst with
  | [] -> [ acc ]
  | rst -> acc :: sum_bags rst
;;

let () =
  let bags = String.split_on_char '\n' (read_file file) in
  let sum = sum_bags bags in
  find_max_opt sum |> Option.get |> printf "Answer a: %d\n";
  let arr = Array.of_list (List.rev (List.sort Int.compare sum)) in
  Array.sub arr 0 3
  |> Array.fold_left (fun acc item -> acc + item) 0
  |> printf "Answer b: %d\n"
;;
