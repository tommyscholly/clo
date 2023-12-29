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

let render_code_block file (l : loc) =
  let start_pos, end_pos = l in
  let lines = extract_lines file start_pos.pos_lnum end_pos.pos_lnum in
  let start_col = start_pos.pos_cnum - start_pos.pos_bol in
  let end_col = end_pos.pos_cnum - end_pos.pos_bol in
  let i = ref start_pos.pos_lnum in
  List.iter
    (fun s ->
      print_endline (Format.sprintf "  %d | %s" !i s);
      i := !i + 1)
    lines;
  if List.length lines > 1
  then ()
  else (
    let padding = if start_col > 0 then String.make (start_col - 1) ' ' else "" in
    let indicator = String.make (end_col - start_col) '^' in
    let front_line_padding =
      String.make (String.length (Format.sprintf "  %d | " !i) + 1) ' '
    in
    print_endline (Format.sprintf "%s%s%s" front_line_padding padding indicator))
;;

let render_error file msg (l : loc) additional (defined_here : loc option) =
  print_endline (msg ^ ":\n");
  render_code_block file l;
  let _ =
    match additional with
    | Some s -> print_endline s
    | None -> ()
  in
  match defined_here with
  | Some l ->
    print_endline ("\n" ^ "Defined here:\n");
    render_code_block file l
  | None -> ()
;;
