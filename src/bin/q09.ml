open Base
open Stdio
open Aocutils

let () = print_endline "Hello from question 9"

type rope =
  { head : point
  ; tail : point
  }

and point = int * int

type move =
  | Up
  | Down
  | Left
  | Right

let move_point p = function
  | Up -> fst p, snd p + 1
  | Down -> fst p, snd p - 1
  | Right -> fst p + 1, snd p
  | Left -> fst p - 1, snd p
;;

let move_of_char = function
  | 'R' -> Right
  | 'L' -> Left
  | 'U' -> Up
  | 'D' -> Down
  | c -> raise (Invalid_argument (Printf.sprintf "Invalid char: %c" c))
;;

let move_rope dir start ~history =
  match dir with
  | Up ->
    if snd start.tail < snd start.head
    then
      ( start.tail :: history
      , { head = move_point start.head Up; tail = fst start.head, snd start.tail + 1 } )
    else history, { start with head = move_point start.head Up }
  | Down ->
    if snd start.tail > snd start.head
    then
      ( start.tail :: history
      , { head = move_point start.head Down; tail = fst start.head, snd start.tail - 1 } )
    else history, { start with head = move_point start.head Down }
  | Left ->
    if fst start.tail > fst start.head
    then
      ( start.tail :: history
      , { head = move_point start.head Left; tail = fst start.tail - 1, snd start.head } )
    else history, { start with head = move_point start.head Left }
  | Right ->
    if fst start.tail < fst start.head
    then
      ( start.tail :: history
      , { head = move_point start.head Right; tail = fst start.tail + 1, snd start.head }
      )
    else history, { start with head = move_point start.head Right }
;;

let apply_fn count ~history ~init ~f =
  let rec f' (acc : int) (h : point list) (x : rope) =
    if acc = 0
    then h, x
    else (
      let hist, res = f h x in
      f' (acc - 1) hist res)
  in
  f' count history init
;;

let apply_move rope line ~history =
  let move = move_of_char line.[0] in
  let count = Int.of_string (List.nth_exn (String.split line ~on:' ') 1) in
  apply_fn count ~history ~init:rope ~f:(fun h x -> move_rope move x ~history:h)
;;

let history =
  read_file "inputs/q09.txt"
  |> String.strip
  |> String.split ~on:'\n'
  |> List.fold_left
       ~init:([], { head = 0, 0; tail = 0, 0 })
       ~f:(fun acc x ->
         let h, r = apply_move (snd acc) x ~history:(fst acc) in
         h, r)
;;

let rec remove_dups = function
  | x :: rst ->
    if List.mem rst x ~equal:(fun a b -> fst a = fst b && snd a = snd b)
    then remove_dups rst
    else x :: remove_dups rst
  | [] -> []
;;

let _ = history |> fun x -> fst x |> remove_dups |> List.length |> printf "Part 1: %d\n"
