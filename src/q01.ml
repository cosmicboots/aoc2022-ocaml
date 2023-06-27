let () = print_endline "Welcome to question 1"
let file = "inputs/q01.txt"

let () =
  let in_chan = open_in file in
  try
    let line = input_line in_chan in
    print_endline line;
    flush stdout;
    close_in in_chan
  with
  | e ->
    close_in_noerr in_chan;
    raise e
;;
