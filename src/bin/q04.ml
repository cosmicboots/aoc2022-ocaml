open Base
open Stdio
open Aocutils

let () = print_endline "Hello from question 4"

let check_pair pair ~cond =
  if String.length pair > 0
  then (
    let pairs =
      String.split pair ~on:','
      |> List.map ~f:(fun x -> List.map (String.split x ~on:'-') ~f:Int.of_string)
    in
    match pairs with
    | [ [ l1; l2 ]; [ r1; r2 ] ] -> if cond l1 l2 r1 r2 then 1 else 0
    | _ -> 0)
  else 0
;;

let cond_pt1 l1 l2 r1 r2 = (l1 <= r1 && l2 >= r2) || (l1 >= r1 && l2 <= r2)

let cond_pt2 l1 l2 r1 r2 =
  (l1 >= r1 && l1 <= r2) || (l2 >= r1 && l2 <= r2) || cond_pt1 l1 l2 r1 r2
;;

let file = "inputs/q04.txt"
let content = read_file file

let run cond =
  String.split content ~on:'\n'
  |> List.map ~f:(fun x -> check_pair x ~cond)
  |> List.fold_right ~f:( + ) ~init:0
;;

let () = printf "Part 1: %d\n" (run cond_pt1)
let () = printf "Part 2: %d\n" (run cond_pt2)
