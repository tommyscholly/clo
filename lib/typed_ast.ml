type loc = Ast.loc
type type_expr = Ast.type_expr

type expr =
  | Int of int
  | Float of float
  | Str of string (* a constant string that cannot change *)
  | Bool of bool
  | Variable of string * loc
  | Call of call * loc
  | Binop of bop * loc (* we validate the *)
  | Let of let_expr * loc
  | Return of expr * type_expr * loc
  | TypeDef of type_def * loc

and call =
  { cname : string
  ; ctype : type_expr
  ; cargs : expr list
  }

and bop =
  { lhs : expr
  ; rhs : expr
  ; bop : Ast.bop
  ; btype : type_expr
  }

and let_expr =
  { lname : string
  ; ltype : type_expr
  ; binding : expr
  }

and fnparam = string * type_expr

and fn_def =
  { fnname : string
  ; fnparams : fnparam list
  ; fnret : type_expr (* we resolve and type check this *)
  }

and type_def =
  | EnumDef
  | StructDef of struct_def

and field = type_expr * loc

and struct_def =
  { sdname : string
  ; sdfields : field array
  }

and type_construct =
  | EnumConst
  | StructConst of struct_def
