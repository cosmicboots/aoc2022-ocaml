open Base [@@warning "-33"]
open Stdio
open Aocutils

let () = print_endline "Hello from question 07!"
let file = "inputs/q07.txt"

type node =
  | Dir of directory
  | File of file

and directory =
  { name : string
  ; dsize : int
  ; children : node list
  }
[@@warning "-69"]

and file =
  { fname : string
  ; size : int
  }
[@@warning "-69"]

let parse_file line =
  let s = String.split line ~on:' ' in
  let size = List.nth_exn s 0 in
  let fname = List.nth_exn s 1 in
  if String.(size = "dir") then None else Some (File { fname; size = Int.of_string size })
;;

let rec parse_ls children = function
  | line :: rst ->
    if Char.(line.[0] = '$')
    then children, line :: rst
    else (
      match parse_file line with
      | None -> parse_ls children rst
      | Some file -> parse_ls (file :: children) rst)
  | [] -> children, []
;;

let rec calc_size = function
  | File f -> f.size
  | Dir d ->
    List.fold_left d.children ~init:0 ~f:(fun acc x ->
      match x with
      | File f -> acc + f.size
      | Dir d -> acc + calc_size (Dir d))
;;

let rec parse_lines = function
  | [] | "$END" :: _ -> [], []
  | "$ cd .." :: rst -> [], rst
  | line :: rst ->
    let ( == ) = String.( = ) in
    let args = String.split line ~on:' ' in
    let cmd = List.nth_exn args 1 in
    if cmd == "cd"
    then (
      let dir = List.nth_exn args 2 in
      let children, rst = parse_lines rst in
      let sibs, rst = parse_lines rst in
      ( [ Dir
            { name = dir
            ; children
            ; dsize = List.fold_left children ~init:0 ~f:(fun acc x -> acc + calc_size x)
            }
        ]
        @ sibs
      , rst ))
    else if cmd == "ls"
    then (
      let children, rst = parse_ls [] rst in
      let children2, rst = parse_lines rst in
      children @ children2, rst)
    else parse_lines rst
;;

let rec find_large_dirs = function
  | Dir d ->
    (if d.dsize <= 100_000 then d.dsize else 0)
    + List.fold_left d.children ~init:0 ~f:(fun acc x -> acc + find_large_dirs x)
  | File _ -> 0
;;

let rec find_dir_cond root ~cond =
  let f d =
    List.fold_left d.children ~init:[] ~f:(fun acc x -> acc @ find_dir_cond x ~cond)
  in
  match root with
  | Dir d -> if cond d then d :: f d else f d
  | File _ -> []
;;

let content = read_file file
let nodes, _ = parse_lines (String.split content ~on:'\n')
let root = List.hd_exn nodes
let () = root |> find_large_dirs |> printf "Part 1: %d\n"
let capacity = 70_000_000
let required_space = 30_000_000

let total_size =
  match root with
  | Dir d -> d.dsize
  | File _ -> 0
;;

let _ =
  root
  |> find_dir_cond ~cond:(fun x -> x.dsize > required_space - (capacity - total_size))
  |> List.map ~f:(fun x -> x.dsize)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> printf "Part 2: %d\n"
;;
