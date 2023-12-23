type loc = Ast.loc
type type_expr = Ast.type_expr

type type_error_kind =
  | TETypeDefAsValue
  | TETypeRedefine
  | TEFieldLengthMismatch
  | TEFieldNonExistant
  | TETypeConstructWithoutDefine
  | TEVariableNotBound
  | TETypeMismatch
  | TEReturnTypeMismatch
  | TEFunctionNonExistant
(* this needs to hold a 'loc list' that points to every return type location *)

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
  | FnDef of fn_def * loc
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
  ; lbinding : expr
  }

and fnparam = string * type_expr

and fn_def =
  { fnname : string
  ; fnparams : fnparam list
  ; fnret : type_expr (* we resolve and type check this *)
  ; fnexprs : expr list
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
let bound_variables = Hashtbl.create 10
let string_of_type = Ast.string_of_type
let defined_functions = Hashtbl.create 10 (* fn_name -> type_expr *)

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
  | Ast.Bool b -> Bool b
  | Ast.Variable (var_name, loc) ->
    let bound_var =
      try Hashtbl.find bound_variables var_name with
      | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
    in
    Variable (var_name, type_of bound_var, loc)
  | Ast.Let (name, ty, expr, loc) ->
    Let ({ lname = name; ltype = ty; lbinding = typed_expr expr }, loc)
  | Ast.Binop (bop, lhs, rhs, loc) ->
    let lhs = typed_expr lhs in
    let rhs = typed_expr rhs in
    let lhs_type = type_of lhs in
    let rhs_type = type_of rhs in
    if lhs_type != rhs_type
    then
      raise
        (TypeError
           { kind = TETypeMismatch
           ; loc
           ; msg =
               Some
                 (Format.sprintf
                    "Left hand %s and right hand %s mismatch"
                    (string_of_type lhs_type)
                    (string_of_type rhs_type))
           })
    else ();
    Binop ({ bop; lhs; rhs; btype = lhs_type }, loc)
  | Function ((fnname, fnparams, fntype_opt, fnexprs), loc) ->
    let fnexprs = List.map typed_expr fnexprs in
    (* list of types of all the returns in the function *)
    (* need to extract the locations as well for better error reporting *)
    let returns =
      List.filter_map
        (fun e ->
          match e with
          | Return (_, ty, _) -> Some ty
          | _ -> None)
        fnexprs
    in
    let return_types_match =
      match fntype_opt with
      | Some ty -> List.for_all (fun ty' -> ty = ty') returns
      (* assert that there are no returns if the return type is void *)
      | _ -> List.length returns = 0 (* Util.all_same returns *)
    in
    let fnret =
      if not return_types_match
      then raise (TypeError { kind = TEReturnTypeMismatch; loc; msg = None })
      else (
        match fntype_opt with
        | Some ty -> ty
        | None -> TVoid)
    in
    Hashtbl.add defined_functions fnname fnret;
    FnDef ({ fnname; fnparams; fnret; fnexprs }, loc)
  | Call (fnname, args, loc) ->
    let cargs = List.map (fun e -> typed_expr e) args in
    let ctype =
      try Hashtbl.find defined_functions fnname with
      | Not_found -> raise (TypeError { kind = TEFunctionNonExistant; msg = None; loc })
    in
    Call ({ cname = fnname; cargs; ctype }, loc)
  | Print (str, args, loc) ->
    let cargs = List.map (fun e -> typed_expr e) args in
    let cargs = Str str :: cargs in
    Call ({ cname = "print"; cargs; ctype = TVoid }, loc)
  | Return (expr, loc) ->
    let texpr = typed_expr expr in
    let ty = type_of texpr in
    Return (texpr, ty, loc)
;;
