let rec all_same lst =
  match lst with
  | [] -> true
  | [ _ ] -> true
  | x :: y :: rest -> if x = y then all_same (y :: rest) else false
;;

let split_tuples lst =
  let rec split acc_a acc_b acc_c = function
    | [] -> List.rev acc_a, List.rev acc_b, List.rev acc_c
    | (a, b, c) :: tl -> split (a :: acc_a) (b :: acc_b) (c :: acc_c) tl
  in
  split [] [] [] lst
;;
