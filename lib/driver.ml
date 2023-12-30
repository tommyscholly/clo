open Lexer
open Lexing
open Printf

let fprintf = Core.fprintf

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let report_error msg lexbuf =
  let start_pos = lexbuf.lex_start_p in
  let end_pos = lexbuf.lex_curr_p in
  let line_num = start_pos.pos_lnum in
  let start_col = start_pos.pos_cnum - start_pos.pos_bol + 1 in
  let end_col = end_pos.pos_cnum - start_pos.pos_bol + 1 in
  let file_name = end_pos.pos_fname in
  printf
    "%s: File \"%s\", line %d, characters %d-%d:\n"
    msg
    file_name
    line_num
    start_col
    end_col;
  let line_start = start_pos.pos_bol in
  let line_end = min (Bytes.length lexbuf.lex_buffer) lexbuf.lex_curr_pos in
  let line =
    String.sub (Bytes.to_string lexbuf.lex_buffer) line_start (line_end - line_start)
  in
  printf "%s\n" line;
  let padding = String.make (start_col - 1) ' ' in
  let indicator = String.make (end_col - start_col) '^' in
  printf "%s%s\n" padding indicator
;;

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError _ ->
    report_error "Syntax Error:" lexbuf;
    exit (-1)
  | Parser.Error ->
    report_error "Syntax Error:" lexbuf;
    exit (-1)
;;

let get_in_channel name =
  try
    let in_channel = open_in name in
    in_channel
  with
  | Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
;;

let parse filename =
  let channel = get_in_channel filename in
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf filename;
  let ast = parse_with_error lexbuf in
  let read_channel = get_in_channel filename in
  let file_contents = really_input_string read_channel (in_channel_length read_channel) in
  close_in channel;
  close_in read_channel;
  ast, file_contents
;;

let typed file_contents e =
  try Typed_ast.typed_expr e with
  | Typed_ast.TypeError te ->
    let _ =
      match te.kind with
      | TEEnumVariantNonExistant ->
        Reporting.Renderer.render_error
          file_contents
          "Enum variant nonexistant"
          te.loc
          te.msg
          None
      | TECasing ->
        Reporting.Renderer.render_error file_contents "Casing error" te.loc te.msg None
      | TETypeDefAsValue ->
        Reporting.Renderer.render_error
          file_contents
          "Type definition used as value"
          te.loc
          te.msg
          None
      | TETypeRedefine ->
        Reporting.Renderer.render_error
          file_contents
          "Type definition was redefined"
          te.loc
          te.msg
          None
      | TEFieldLengthMismatch ->
        Reporting.Renderer.render_error
          file_contents
          "Field length mismatch"
          te.loc
          te.msg
          None
      | TEFieldNonExistant ->
        Reporting.Renderer.render_error
          file_contents
          "Field is non-existant"
          te.loc
          te.msg
          None
      | TETypeConstructWithoutDefine ->
        Reporting.Renderer.render_error
          file_contents
          "Type is not defined"
          te.loc
          te.msg
          None
      | TEVariableNotBound ->
        Reporting.Renderer.render_error
          file_contents
          "Variable not bound"
          te.loc
          te.msg
          None
      | TETypeMismatch loc_opt ->
        Reporting.Renderer.render_error
          file_contents
          "Type mismatch"
          te.loc
          te.msg
          loc_opt
      | TEReturnTypeMismatch ->
        Reporting.Renderer.render_error
          file_contents
          "Return type mismatch"
          te.loc
          te.msg
          None
      | TEFunctionNonExistant ->
        Reporting.Renderer.render_error
          file_contents
          "Function is non-existant"
          te.loc
          te.msg
          None
      | TEInvalidFieldAccess ->
        Reporting.Renderer.render_error
          file_contents
          "Invalid field access"
          te.loc
          te.msg
          None
    in
    exit 1
;;

let codegen_error_to_message (ety : Codegen.errorty) =
  match ety with
  | Call -> "Called function does not exist\n"
  | Defn -> "Function define error\n"
  | Redef -> "Attempt to redefine function\n"
  | UnknownVar s -> Format.sprintf "Unknown variable %s referenced\n" s
  | Args (param_args, call_args, call_name) ->
    Format.sprintf
      "Incorrect arguments for %s, expected %d got %d\n"
      call_name
      param_args
      call_args
  | UnknownType -> "Unknown type referenced\n"
  | ReturnType -> "Return type not provided\n"
  | NotSupported -> "This is not supported\n"
;;

let gen a =
  let _ =
    try Codegen.codegen_expr a with
    | Codegen.CodegenError (ety, (start_loc, end_loc)) ->
      let msg = codegen_error_to_message ety in
      printf
        "%s %d-%d:%d-%d"
        msg
        start_loc.pos_lnum
        end_loc.pos_lnum
        start_loc.pos_cnum
        end_loc.pos_cnum;
      (* report_error ety loc; *)
      exit 1
  in
  ()
;;

let top filename =
  let ast, contents = parse filename in
  let ast = List.map (typed contents) ast in
  let _ = Codegen.register_extern_functions () in
  let _ = List.iter (fun a -> gen a) ast in
  Llvm.dump_module Codegen.llvm_module;
  ()
;;
