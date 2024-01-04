type loc = Lexing.position * Lexing.position (* start, end *)

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

type bop =
  | Plus
  | Minus
  | Mul
  | Eq

(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type type_expr =
  | TInt
  | TFloat
  | TStr
  | TVoid
  | TBool
  | TCustom of string

let string_of_type = function
  | TInt -> "int"
  | TFloat -> "float"
  | TStr -> "str"
  | TVoid -> "void"
  | TBool -> "bool"
  | TCustom s -> s
;;

type param = string * type_expr * loc
type field = Field of string * type_expr * loc

type variant =
  | EnumVar of string * type_expr option * loc
  | StructVar of string * field list * loc

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
  | Let of string * unit option * type_expr option * expr * loc
  | Function of fndef * loc
  | Return of expr * loc
  | Struct of string * field list * loc
  | Enum of string * variant list * loc
  | StructConstruct of string * construct_field list * loc
  | EnumConstruct of string * string * construct_variant option * loc
  | FieldAccess of string * string * loc (* name.field *)
  | Match of expr * match_case list * loc
  | Assignment of string * expr * loc (* ident = expr *)
  | If of if_expr

and if_expr = (expr * expr list) * expr list option * loc
and fndef = string * param list * type_expr option * expr list
and construct_field = string * expr * loc

and construct_variant =
  | UnionVariant of expr list
  | StructVariant of construct_field list

and match_case = match_case_kind * expr * loc

and match_case_kind =
  | EnumMatch of string * string * enum_match_kind option
  | DefaultMatch

and enum_match_kind =
  | UnionMatch of string list * loc
  | StructMatch of string * loc (* struct is just allocated to an identifier *)
