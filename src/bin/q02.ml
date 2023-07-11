open Aocutils
open Printf

let () = print_endline "Hello from question 2"
let file = "inputs/q02.txt"

(** Convert puzzle input char into number *)
let int_of_move = function
  | 'A' | 'X' -> 1
  | 'B' | 'Y' -> 2
  | 'C' | 'Z' -> 3
  | c -> raise @@ Invalid_argument (sprintf "Invalid argument: %c" c)
;;

let calc_score (round : string) =
  if String.length round = 3
  then (
    let a = int_of_move round.[0] in
    let b = int_of_move round.[2] in
    if (a mod 3) + 1 == b then b + 6 else if a - b == 0 then b + 3 else b)
  else 0
;;

type outcome =
  | Win
  | Lose
  | Tie

let outcome_of_move = function
  | 'X' -> Lose
  | 'Y' -> Tie
  | 'Z' -> Win
  | c -> raise (Invalid_argument (sprintf "Invalid argument: %c" c))
;;

let calc_score_pt2 round =
  if String.length round = 3
  then (
    let l = int_of_move round.[0] in
    let o = outcome_of_move round.[2] in
    match o with
    | Tie -> l + 3
    | Win -> (l mod 3) + 7
    | Lose -> ((l + 1) mod 3) + 1)
  else 0
;;

let solve_part file ~part ~f =
  read_file file
  |> String.split_on_char '\n'
  |> List.map f
  |> List.fold_left (fun acc x -> acc + x) 0
  |> string_of_int
  |> printf "Part %d: %s\n" part
;;

solve_part file ~part:1 ~f:(fun x -> calc_score x);;
solve_part file ~part:2 ~f:(fun x -> calc_score_pt2 x)
