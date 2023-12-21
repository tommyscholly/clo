open Core

let read_file_to_string filename =
  try
    let content = In_channel.read_all filename in
    Some content
  with
  | Sys_error _ -> None
;;
