type bop =
  | Add
  | Mul

(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type proto = Prototype of string * string list

type expr =
  (* variant for numeric literals like "1.0". *)
  | Number of float
  (* variant for referencing a variable, like "a". *)
  | Variable of string
  (* variant for a binary operator. *)
  | Binop of bop * expr * expr
  (* variant for function calls. *)
  | Call of string * expr list
  | Function of proto * expr

