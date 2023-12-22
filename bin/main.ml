open Kali.Driver
module Sys = Core.Sys

type command_line_args = { input_file : string }

let default_args = { input_file = "" }

let parse_args () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | [ _; input_file ] -> { input_file }
  | _ -> default_args
;;

let main args =
  (* let file_content = *)
  (*   match read_file_to_string args.input_file with *)
  (*   | Some content -> content *)
  (*   | None -> *)
  (*     printf "Error: Could not read the file %s\n" args.input_file; *)
  (*     exit 1 *)
  (* in *)
  top args.input_file
;;

let () =
  let args = parse_args () in
  main args
;;
