type loc = Ast.loc
type type_expr = Ast.type_expr

type type_error_kind =
  | TETypeDefAsValue
  | TETypeRedefine
  | TEFieldLengthMismatch
  | TEFieldNonExistant
  | TETypeConstructWithoutDefine
  | TEVariableNotBound
  | TETypeMismatch of loc option
  | TEReturnTypeMismatch
  | TEFunctionNonExistant
  | TEInvalidFieldAccess
  | TECasing
  | TEEnumVariantNonExistant
  | TEMatchInType
  | TEAssign
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
  | FieldAccess of field_access * loc
  | Match of match_expr * loc
  | Assignment of assignment_expr * loc
  | If of if_expr * loc

and if_expr =
  { ifcond : expr
  ; has_return : type_expr option
  ; then_block : expr list
  ; else_block : expr list option
  }

and assignment_expr =
  { aslhs : string (* id *)
  ; asrhs : expr
  ; asty : type_expr
  }

and enum_match_kind =
  | UnionMatch of (string * type_expr) list * loc
  | StructMatch of string * loc (* struct is just allocated to an identifier *)
  | TagMatch

and match_case_kind =
  | EnumMatch of enum_match_kind
  | DefaultMatch

and match_case =
  { casety : type_expr
  ; casekind : match_case_kind
  ; caseexpr : expr
  ; caseloc : loc
  }

and match_expr =
  { mexpr : expr
  ; mty : type_expr
  ; mcases : match_case list
  }

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
  ; lmut : bool
  ; ltype : type_expr
  ; lbinding : expr
  }

and fnparam = string * type_expr * loc

and fn_def =
  { fnname : string
  ; fnparams : fnparam list
  ; fnret : type_expr (* we resolve and type check this *)
  ; fnexprs : expr list
  }

and type_def =
  | EnumDef of enum_def
  | StructDef of struct_def

and enum_def =
  { ename : string
  ; evariants : variant list
  }

(* this type info isnt currently being used, as i added it in for something that didn't need it. we'll use it for type checking though later *)
and variant =
  | Tag of string
  | Union of string * type_expr list
  | Struct of string * struct_def

and field = type_expr * loc

and struct_def =
  { sname : string
  ; sfields : field array
  }

and construct_field = expr * loc

and struct_construct =
  { scname : string
  ; scfields : construct_field array
  }

and construct_data =
  | UnionData of expr list
  | StructData of construct_field array

and enum_construct =
  { ecname : string
  ; ecvariant : string
  ; ecidx : int
  ; ecdata : construct_data option
  }

and type_construct =
  | EnumConst of enum_construct
  | StructConst of struct_construct

and field_access =
  { fvarname : string
  ; ffieldidx : int
  ; ffieldtype : type_expr
  ; ftypename : string
  }

let defined_structs : (string, struct_def) Hashtbl.t =
  Hashtbl.create 10 (* struct name -> struct_def *)
;;

let defined_struct_fields =
  Hashtbl.create 10 (* struct name -> (field name, idx) hashtbl *)
;;

let bound_variables : (string, bool * type_expr) Hashtbl.t =
  Hashtbl.create 10 (* bool is for mutability *)
;;

let string_of_type = Ast.string_of_type

let defined_functions : (string, fn_def) Hashtbl.t =
  Hashtbl.create 10 (* fn name -> fn_def *)
;;

let defined_enums : (string, variant array) Hashtbl.t =
  Hashtbl.create 10 (* enum name -> array evariant *)
;;

let variant_name_to_idx : (string, (string, int) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 10 (* enum name -> (variant name, int) hashtbl *)
;;

let size_of = function
  | Ast.TInt -> 32
  | Ast.TBool -> 8
  | Ast.TFloat -> 64
  | Ast.TStr -> 32 (* ptr *)
  | Ast.TCustom _ -> 32 (* ptr *)
  | Ast.TVoid -> 0
;;

let rec struct_size ?(sum = 0) fields =
  match fields with
  | [] -> sum
  | (ty, _) :: fs ->
    let sum = sum + size_of ty in
    struct_size fs ~sum
;;

let variant_name = function
  | Tag n -> n
  | Union (n, _) -> n
  | Struct (n, _) -> n
;;

let type_of = function
  | Int _ -> Ast.TInt
  | Float _ -> Ast.TFloat
  | Bool _ -> Ast.TBool
  | Str _ -> Ast.TStr
  | Return (_, ty, _) -> ty
  | TypeConstruct (c, _) ->
    (match c with
     | EnumConst e -> Ast.TCustom e.ecname
     | StructConst s -> TCustom s.scname)
  | TypeDef (_, loc) -> raise (TypeError { kind = TETypeDefAsValue; loc; msg = None })
  | If (_, loc) -> raise (TypeError { kind = TETypeDefAsValue; loc; msg = None })
  | Variable (_, ty, _) -> ty
  | Let (l, _) -> l.ltype
  | Call (c, _) -> c.ctype
  | Binop (b, _) -> b.btype
  | FnDef (fndef, _) -> fndef.fnret
  | FieldAccess (faccess, _) -> faccess.ffieldtype
  | Match (m, _) -> m.mty
  | Assignment (a, _) -> a.asty
;;

let typecheck_field field_name struct_name fieldty loc =
  let sdef =
    try Hashtbl.find defined_structs struct_name with
    | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
  in
  let struct_fields =
    try Hashtbl.find defined_struct_fields struct_name with
    | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
  in
  let idx =
    try Hashtbl.find struct_fields field_name with
    | Not_found -> raise (TypeError { kind = TEFieldNonExistant; msg = None; loc })
  in
  let field_type, fieldloc = Array.get sdef.sfields idx in
  if field_type = fieldty
  then ()
  else
    raise
      (TypeError
         { kind = TETypeMismatch (Some fieldloc)
         ; msg =
             Some
               (Format.sprintf
                  "Field %s expected type %s, got %s"
                  field_name
                  (string_of_type field_type)
                  (string_of_type fieldty))
         ; loc
         })
;;

(* extracted because enums can contain structs *)
let make_struct name fields loc =
  let struct_field_tbl =
    match Hashtbl.find_opt defined_struct_fields name with
    | Some _ -> raise (TypeError { kind = TETypeRedefine; loc; msg = None })
    | None -> Hashtbl.create 10
  in
  let fields = Array.of_list fields in
  let field_types = Array.make (Array.length fields) (Ast.TVoid, Ast.dummy_loc) in
  Array.iteri
    (fun i f ->
      let field_name, field_type, loc =
        match f with
        | Ast.Field (field_name, field_type, l) -> field_name, field_type, l
      in
      Hashtbl.add struct_field_tbl field_name i;
      Array.set field_types i (field_type, loc))
    fields;
  Hashtbl.add defined_struct_fields name struct_field_tbl;
  let def = { sname = name; sfields = field_types } in
  Hashtbl.add defined_structs name def;
  def
;;

let get_variant_id_for_name name =
  let split = String.split_on_char ':' name in
  let enum_name = List.hd split in
  let enum_variant = List.hd (List.tl split) in
  let tbl = Hashtbl.find variant_name_to_idx enum_name in
  Hashtbl.find tbl enum_variant
;;

let map_variant tbl ename i variant =
  match variant with
  | Ast.EnumVar (name, ty_opt, _) ->
    Hashtbl.add tbl name i;
    (match ty_opt with
     | Some ty -> Union (name, [ ty ])
     | None -> Tag name)
  | Ast.StructVar (name, fields, loc) ->
    let type_name = ename ^ ":" ^ name in
    Hashtbl.add tbl name i;
    Struct (name, (make_struct type_name) fields loc)
;;

let rec typed_expr (e : Ast.expr) =
  match e with
  | Enum (ename, variants, loc) ->
    if not (Util.is_pascal_case ename)
    then
      raise
        (TypeError
           { kind = TECasing
           ; msg =
               Some
                 (Format.sprintf
                    "Enum '%s' should be PascalCase\n  FIX: Rename to %s"
                    ename
                    (Util.to_pascal_case ename))
           ; loc
           })
    else ();
    let variant_name_to_id_table = Hashtbl.create (List.length variants) in
    let evariants = List.mapi (map_variant variant_name_to_id_table ename) variants in
    let evariants_arr = Array.of_list evariants in
    Hashtbl.add defined_enums ename evariants_arr;
    Hashtbl.add variant_name_to_idx ename variant_name_to_id_table;
    TypeDef (EnumDef { ename; evariants }, loc)
  | EnumConstruct (ename, evariant, data, loc) ->
    let enum_fields =
      try Hashtbl.find defined_enums ename with
      | Not_found ->
        raise (TypeError { kind = TETypeConstructWithoutDefine; loc; msg = None })
    in
    let idx = ref None in
    Array.iteri
      (fun i v -> if variant_name v = evariant then idx := Some i else ())
      enum_fields;
    let idx =
      match !idx with
      | Some i -> i
      | None -> raise (TypeError { kind = TEEnumVariantNonExistant; msg = None; loc })
    in
    let data =
      match data with
      | Some d ->
        (match d with
         | Ast.UnionVariant es ->
           let texprs = List.map typed_expr es in
           Some (UnionData texprs)
         | Ast.StructVariant fields ->
           Some (StructData (map_fields fields (ename ^ ":" ^ evariant) loc)))
      | None -> None
    in
    TypeConstruct
      (EnumConst { ecname = ename; ecvariant = evariant; ecdata = data; ecidx = idx }, loc)
  | FieldAccess (var_name, field_name, loc) ->
    (* Hashtbl.iter *)
    (*   (fun key value -> *)
    (*     print_endline (Format.sprintf "%s -> %s" key (*string_of_type value*) value.sname)) *)
    (*   defined_structs; *)
    let _, var_type =
      try Hashtbl.find bound_variables var_name with
      | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
    in
    let var_type =
      match var_type with
      | Ast.TCustom s -> s
      | _ -> raise (TypeError { kind = TEInvalidFieldAccess; msg = None; loc })
    in
    let is_enum = String.contains var_type ':' in
    let sdef =
      try Hashtbl.find defined_structs var_type with
      | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
    in
    let struct_fields =
      try Hashtbl.find defined_struct_fields var_type with
      | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
    in
    let idx =
      try Hashtbl.find struct_fields field_name with
      | Not_found -> raise (TypeError { kind = TEFieldNonExistant; msg = None; loc })
    in
    let field_type, _ = Array.get sdef.sfields idx in
    FieldAccess
      ( { fvarname = var_name
        ; ffieldidx = (if is_enum then idx + 1 else idx)
        ; ffieldtype = field_type
        ; ftypename = var_type
        }
      , loc )
  | Struct (name, fields, loc) ->
    if not (Util.is_pascal_case name)
    then
      raise
        (TypeError
           { kind = TECasing
           ; msg =
               Some
                 (Format.sprintf
                    "Struct '%s' should be PascalCase\n  FIX: Rname to %s"
                    name
                    (Util.to_pascal_case name))
           ; loc
           })
    else ();
    let def = make_struct name fields loc in
    TypeDef (StructDef def, loc)
  | StructConstruct (name, fields, loc) ->
    let mapped_fields = map_fields fields name loc in
    TypeConstruct (StructConst { scname = name; scfields = mapped_fields }, loc)
  | Int i -> Int i
  | Float f -> Float f
  | Bool b -> Bool b
  | Variable (var_name, loc) ->
    let _, bound_var =
      try Hashtbl.find bound_variables var_name with
      | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
    in
    Variable (var_name, bound_var, loc)
  | Let (name, mut, ty, expr, loc) ->
    let mut =
      match mut with
      | Some () -> true
      | None -> false
    in
    let lbinding = typed_expr expr in
    let lbinding_type = type_of lbinding in
    let ty =
      match ty with
      | Some ty ->
        if ty <> lbinding_type
        then
          raise
            (TypeError
               { kind = TETypeMismatch None
               ; msg =
                   Some
                     (Format.sprintf
                        "Left side type %s does not match right side type %s"
                        (string_of_type ty)
                        (string_of_type lbinding_type))
               ; loc
               })
        else ty
      | None -> lbinding_type
    in
    Hashtbl.add bound_variables name (mut, lbinding_type);
    (* be able to look up bindings to find and validate field accesses *)
    Let ({ lname = name; lmut = mut; ltype = ty; lbinding }, loc)
  | Binop (bop, lhs, rhs, loc) ->
    let lhs = typed_expr lhs in
    let rhs = typed_expr rhs in
    let lhs_type = type_of lhs in
    let rhs_type = type_of rhs in
    if lhs_type != rhs_type
    then
      raise
        (TypeError
           { kind = TETypeMismatch None
           ; loc
           ; msg =
               Some
                 (Format.sprintf
                    "Left side type %s does not match right side type %s"
                    (string_of_type lhs_type)
                    (string_of_type rhs_type))
           })
    else ();
    let btype =
      match bop with
      | Ast.Eq -> Ast.TBool
      | _ -> lhs_type
    in
    Binop ({ bop; lhs; rhs; btype }, loc)
  | Function ((fnname, fnparams, fntype_opt, fnexprs), loc) ->
    Hashtbl.clear bound_variables;
    (* bound variables do not persist across functions *)
    List.iter
      (fun (pname, ptype, _) -> Hashtbl.add bound_variables pname (false, ptype))
      fnparams;
    let fnret =
      match fntype_opt with
      | Some ty -> ty
      | None -> TVoid
    in
    let dummy_fndef = { fnname; fnparams; fnret; fnexprs = [] } in
    (* we add a dummy fndef into defined functions so recursive calls can see it *)
    Hashtbl.add defined_functions fnname dummy_fndef;
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
    if not return_types_match
    then raise (TypeError { kind = TEReturnTypeMismatch; loc; msg = None })
    else ();
    let fndef = { fnname; fnparams; fnret; fnexprs } in
    Hashtbl.remove defined_functions fnname;
    Hashtbl.add defined_functions fnname fndef;
    FnDef (fndef, loc)
  | Call (fnname, args, loc) ->
    let cargs = List.map (fun e -> typed_expr e) args in
    let fndef =
      try Hashtbl.find defined_functions fnname with
      | Not_found -> raise (TypeError { kind = TEFunctionNonExistant; msg = None; loc })
    in
    (* typecheck call args *)
    let zipped_args = List.combine cargs fndef.fnparams in
    List.iter
      (fun (carg, (pname, p_ty, ploc)) ->
        let arg_ty = type_of carg in
        if arg_ty = p_ty
        then ()
        else
          raise
            (TypeError
               { kind = TETypeMismatch (Some ploc)
               ; loc
               ; msg =
                   Some
                     (Format.sprintf
                        "Parameter %s was expected of type %s, got %s"
                        pname
                        (string_of_type p_ty)
                        (string_of_type arg_ty))
               }))
      zipped_args;
    Call ({ cname = fnname; cargs; ctype = fndef.fnret }, loc)
  | Print (str, args, loc) ->
    let cargs = List.map (fun e -> typed_expr e) args in
    let cargs = Str str :: cargs in
    Call ({ cname = "printf"; cargs; ctype = TVoid }, loc)
  | Return (expr, loc) ->
    let texpr = typed_expr expr in
    let ty = type_of texpr in
    Return (texpr, ty, loc)
  | Match (expr, cases, loc) ->
    let texpr = typed_expr expr in
    let ty = type_of texpr in
    if match ty with
       | Ast.TCustom _ -> true
       | _ -> false
    then ()
    else raise (TypeError { kind = TEMatchInType; msg = None; loc });
    let map_case (case_kind, case_expr, loc) =
      let casety, casekind, bound_vars =
        match case_kind with
        (* case is an enum match *)
        | Ast.EnumMatch (enum_name, enum_variant, kind) ->
          let case_ty = Ast.TCustom (enum_name ^ ":" ^ enum_variant) in
          let variants =
            try Hashtbl.find defined_enums enum_name with
            | Not_found ->
              raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
          in
          let casekind, bound_vars =
            (* kind is an option, where if it is None, then it is "Type:Tag" with no data *)
            match kind with
            | Some k ->
              (match k with
               | Ast.UnionMatch (ss, loc) ->
                 let variant_types =
                   List.hd
                     (List.filter_map
                        (fun variant ->
                          match variant with
                          | Union (name, tys) ->
                            if name = enum_variant then Some tys else None
                          | _ -> None)
                        (Array.to_list variants))
                 in
                 let typed_ids =
                   List.map
                     (fun (s, ty) ->
                       Hashtbl.add bound_variables s (false, case_ty);
                       s, ty)
                     (List.combine ss variant_types)
                 in
                 UnionMatch (typed_ids, loc), ss
               | Ast.StructMatch (s, loc) ->
                 Hashtbl.add bound_variables s (false, case_ty);
                 StructMatch (s, loc), [ s ])
            | None -> TagMatch, []
          in
          case_ty, EnumMatch casekind, bound_vars
        | Ast.DefaultMatch -> Ast.TCustom "_", DefaultMatch, []
      in
      let caseexpr = typed_expr case_expr in
      List.iter (fun s -> Hashtbl.remove bound_variables s) bound_vars;
      { casety; caseexpr; caseloc = loc; casekind }
    in
    let mcases = List.map map_case cases in
    let mcases =
      List.sort
        (fun a b ->
          match a.casekind, b.casekind with
          | _, DefaultMatch -> -1
          | DefaultMatch, _ -> 1
          | _ -> 0)
        mcases
    in
    Match ({ mexpr = texpr; mty = ty; mcases }, loc)
  | Assignment (var_name, expr, loc) ->
    let mut, var_type =
      try Hashtbl.find bound_variables var_name with
      | Not_found -> raise (TypeError { kind = TEVariableNotBound; msg = None; loc })
    in
    if not mut
    then
      raise
        (TypeError
           { kind = TEAssign
           ; msg = Some (Format.sprintf "%s is not mutable" var_name)
           ; loc
           })
    else ();
    let texpr = typed_expr expr in
    let ty = type_of texpr in
    if ty <> var_type
    then
      raise
        (TypeError
           { kind = TETypeMismatch None
           ; msg =
               Some
                 (Format.sprintf
                    "Left side type %s does not match right side type %s"
                    (string_of_type var_type)
                    (string_of_type ty))
           ; loc
           });
    Assignment ({ aslhs = var_name; asrhs = texpr; asty = ty }, loc)
  | If (cond, then_block, else_block, loc) ->
    let cond = typed_expr cond in
    let cond_ty = type_of cond in
    if cond_ty <> Ast.TBool
    then
      raise
        (TypeError
           { kind = TETypeMismatch None
           ; msg =
               Some
                 (Format.sprintf
                    "Condition should be of type bool, is type %s"
                    (string_of_type cond_ty))
           ; loc
           });
    let has_return = ref None in
    let then_block =
      List.map
        (fun e ->
          let texpr = typed_expr e in
          (match texpr with
           | Return (_, ty, _) -> has_return := Some ty
           | _ -> ());
          texpr)
        then_block
    in
    let else_block = Option.map (List.map typed_expr) else_block in
    let has_return = !has_return in
    If ({ ifcond = cond; then_block; else_block; has_return }, loc)

and map_fields fields name loc =
  let struct_field_tbl =
    try Hashtbl.find defined_struct_fields name with
    | Not_found ->
      raise (TypeError { kind = TETypeConstructWithoutDefine; loc; msg = None })
  in
  let fields = Array.of_list fields in
  if Array.length fields != Hashtbl.length struct_field_tbl
  then raise (TypeError { kind = TEFieldLengthMismatch; loc; msg = None })
  else ();
  let mapped_fields = Array.make (Array.length fields) (Int 0, Ast.dummy_loc) in
  Array.iter
    (fun (field_name, field_expr, field_loc) ->
      let idx =
        try Hashtbl.find struct_field_tbl field_name with
        | Not_found ->
          raise (TypeError { kind = TEFieldNonExistant; loc = field_loc; msg = None })
      in
      let expr = typed_expr field_expr in
      let fieldty = type_of expr in
      typecheck_field field_name name fieldty field_loc;
      Array.set mapped_fields idx (expr, field_loc))
    fields;
  mapped_fields
;;
