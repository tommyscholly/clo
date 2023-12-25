type loc = Lexing.position * Lexing.position

let rec sublist start length lst =
  match lst with
  | [] -> []
  | hd :: tl ->
    if start > 0
    then sublist (start - 1) length tl
    else if length > 0
    then hd :: sublist 0 (length - 1) tl
    else []
;;

let extract_lines input start_line end_line =
  let lines = String.split_on_char '\n' input in
  let start_index = start_line - 1 in
  let end_index = min (List.length lines - 1) (end_line - 1) in
  if start_index < 0 || start_index > end_index
  then [ "" ]
  else sublist start_index (end_index - start_index + 1) lines
;;

let render_error file msg (l : loc) additional =
  let start_pos, end_pos = l in
  let lines = extract_lines file start_pos.pos_lnum end_pos.pos_lnum in
  print_endline (msg ^ ":\n");
  let i = ref start_pos.pos_lnum in
  List.iter
    (fun s ->
      print_endline (Format.sprintf "  %d | %s" !i s);
      i := !i + 1)
    lines;
  match additional with
  | Some s -> print_endline ("\n" ^ s)
  | None -> ()
;;
