let printf = Core.printf

module Sys = Core.Sys

type command_line_args =
  { input_file : string
  ; verbose : bool
  }

let default_args = { input_file = ""; verbose = false }

let parse_args () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | _ :: input_file :: rest ->
    let verbose = List.exists (( = ) "--verbose") rest in
    { input_file; verbose }
  | _ -> default_args
;;

let main args =
  if args.verbose
  then printf "Input file: %s\n" args.input_file
  else printf "Processing...\n"
;;

let () =
  let args = parse_args () in
  main args
;;
