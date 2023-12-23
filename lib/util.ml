let rec all_same lst =
  match lst with
  | [] -> true
  | [ _ ] -> true
  | x :: y :: rest -> if x = y then all_same (y :: rest) else false
;;
