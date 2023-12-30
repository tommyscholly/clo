open Core

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

let is_pascal_case (identifier : string) : bool =
  match identifier with
  | "" -> false
  | s ->
    let first_char = String.get s 0 in
    Char.is_uppercase first_char
    && String.for_all
         ~f:(fun c -> Char.is_lowercase c || Char.is_digit c)
         (String.sub s ~pos:1 ~len:(String.length s - 1))
;;

let to_pascal_case (s : string) : string =
  let words = String.split ~on:' ' s in
  String.concat ~sep:"" (List.map ~f:String.capitalize words)
;;

let index_of (arr : 'a array) (target : 'a) : int option =
  let rec find_index index =
    if index < Array.length arr then
      if arr.(index) = target then
        Some index
      else
        find_index (index + 1)
    else
      None
  in
  find_index 0
;;
