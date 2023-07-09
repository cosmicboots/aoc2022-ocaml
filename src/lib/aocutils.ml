(** [read_file file] will read the entire [file] into a string *)
let read_file file =
  let ic = open_in file in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;
  str
;;

let format_list lst = Printf.sprintf "[%s]" (String.concat ", " lst)

(** Find the max element in a list *)
let rec find_max_opt = function
  | [] -> None
  | x :: xs ->
    (match find_max_opt xs with
     | None -> Some x
     | Some y -> if x > y then Some x else Some y)
;;
