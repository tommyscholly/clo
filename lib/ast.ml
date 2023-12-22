type loc = Lexing.position * Lexing.position (* start, end *)

type bop =
  | Plus
  | Mul

(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type type_expr =
  | TInt
  | TString
  | TVoid
  | TBool
  | TCustom of string

type param = string * type_expr

type expr =
  (* variant for numeric literals like "1.0". *)
  | Int of int
  | Float of float
  | Bool of bool
  (* variant for referencing a variable, like "a". *)
  | Variable of string * loc
  (* variant for a binary operator. *)
  | Binop of bop * expr * expr * loc
  (* variant for function calls. *)
  | Call of string * expr list * loc
  | Print of string * expr list * loc
  | Let of string * type_expr * expr * loc
  | Function of fndef * loc
  | Return of expr * loc

and fndef = string * param list * type_expr option * expr list
