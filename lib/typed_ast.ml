type loc = Ast.loc
type type_expr = Ast.type_expr

type type_error_kind =
  | TETypeDefAsValue
  | TETypeRedefine
  | TEFieldLengthMismatch
  | TEFieldNonExistant
  | TETypeConstructWithoutDefine

exception
  TypeError of
    { kind : type_error_kind
    ; loc : loc
    ; msg : string option
    }

type expr =
  | Int of int
  | Float of float
  | Str of string (* a constant string that cannot change *)
  | Bool of bool
  | Variable of string * type_expr * loc
  | Call of call * loc
  | Binop of bop * loc (* we validate the *)
  | Let of let_expr * loc
  | Return of expr * type_expr * loc
  | TypeDef of type_def * loc
  | TypeConstruct of
      type_construct * loc (* something like thing = StructName { field_one: string} *)

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
  { sname : string
  ; sfields : field array
  }

and type_construct =
  | EnumConst
  | StructConst of struct_def

let defined_structs = Hashtbl.create 10 (* struct_name -> HashTbl<field_name, idx> *)

let type_of = function
  | Int _ -> Ast.TInt
  | Float _ -> Ast.TFloat
  | Bool _ -> Ast.TBool
  | Str _ -> Ast.TStr
  | Return (_, ty, _) -> ty
  | TypeConstruct (c, _) ->
    (match c with
     | EnumConst -> Ast.TCustom "Enum"
     | StructConst s -> TCustom s.sname)
  | TypeDef (_, loc) -> raise (TypeError { kind = TETypeDefAsValue; loc; msg = None })
  | Variable (_, ty, _) -> ty
  | Let (l, _) -> l.ltype
  | Call (c, _) -> c.ctype
  | Binop (b, _) -> b.btype
;;

let rec typed_expr (e : Ast.expr) =
  match e with
  | Ast.Struct (name, fields, loc) ->
    let struct_field_tbl =
      match Hashtbl.find_opt defined_structs name with
      | Some _ -> raise (TypeError { kind = TETypeRedefine; loc; msg = None })
      | None -> Hashtbl.create 10
    in
    let fields = Array.of_list fields in
    let field_types = [||] in
    Array.iteri
      (fun i f ->
        let field_name, field_type, loc =
          match f with
          | Ast.Field (field_name, field_type, l) -> field_name, field_type, l
        in
        Hashtbl.add struct_field_tbl field_name i;
        Array.set field_types i (field_type, loc))
      fields;
    Hashtbl.add defined_structs name struct_field_tbl;
    TypeDef (StructDef { sname = name; sfields = field_types }, loc)
  | Ast.StructConstruct (name, fields, loc) ->
    let struct_field_tbl =
      try Hashtbl.find defined_structs name with
      | Not_found ->
        raise (TypeError { kind = TETypeConstructWithoutDefine; loc; msg = None })
    in
    let fields = Array.of_list fields in
    if Array.length fields != Hashtbl.length struct_field_tbl
    then raise (TypeError { kind = TEFieldLengthMismatch; loc; msg = None })
    else ();
    let mapped_fields = [||] in
    Array.iter
      (fun (field_name, field_expr, field_loc) ->
        let idx =
          try Hashtbl.find struct_field_tbl field_name with
          | Not_found ->
            raise (TypeError { kind = TEFieldNonExistant; loc = field_loc; msg = None })
        in
        Array.set mapped_fields idx (typed_expr field_expr, field_loc))
      fields;
    TypeConstruct (StructConst { sname = name; sfields = mapped_fields }, loc)
  | Ast.Int i -> Int i
  | Ast.Float f -> Float f
  | _ -> Int 0
;;
