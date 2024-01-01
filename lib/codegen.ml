type errorty =
  | Call
  | Defn
  | Redef
  | UnknownVar of string
  | Args of int * int * string
  | UnknownType
  | NotSupported
  | ReturnType

exception CodegenError of errorty * Ast.loc

let context = Llvm.global_context ()
let llvm_module = Llvm.create_module context "jit"
let builder = Llvm.builder context
let named_values = Hashtbl.create 10
let allocated_strings = Hashtbl.create 10
let fn_tys = Hashtbl.create 10

(* types *)
let double_type = Llvm.double_type context
let i32_type = Llvm.i32_type context
let void_type = Llvm.void_type context
let i8_type = Llvm.i8_type context
let struct_type = Llvm.struct_type context
let named_struct_type = Llvm.named_struct_type context
let strings = ref 0

let map_type_def ty loc =
  match ty with
  | Ast.TInt -> i32_type
  | Ast.TVoid -> void_type
  | Ast.TBool -> i8_type
  | Ast.TCustom name ->
    (match Llvm.type_by_name llvm_module name with
     (* | Some ty -> ty *)
     | Some _ -> Llvm.pointer_type context
     | None -> raise (CodegenError (UnknownType, loc)))
  | _ -> raise (CodegenError (UnknownType, loc))
;;

let register_extern_functions () =
  let ft = Llvm.var_arg_function_type void_type [| Llvm.pointer_type context |] in
  Hashtbl.add fn_tys "printf" ft;
  Llvm.declare_function "printf" ft llvm_module
;;

let create_struct name fields =
  let field_types = Array.map (fun (ty, l) -> map_type_def ty l) fields in
  let named_struct = named_struct_type name in
  Llvm.struct_set_body named_struct field_types false;
  named_struct
;;

let rec codegen_expr = function
  | Typed_ast.Int n -> Llvm.const_int i32_type n
  | Typed_ast.Float n -> Llvm.const_float double_type n
  | Typed_ast.Bool b -> Llvm.const_int i8_type (if b then 1 else 0)
  | Typed_ast.Str s ->
    let ptr = Hashtbl.find_opt allocated_strings s in
    (match ptr with
     | Some ptr -> ptr
     | None ->
       let str_int = !strings in
       strings := !strings + 1;
       let ptr =
         Llvm.build_global_stringptr s (Format.sprintf "string_%d" str_int) builder
       in
       Hashtbl.add allocated_strings s ptr;
       ptr)
  | Typed_ast.Variable (name, _, loc) ->
    (try Hashtbl.find named_values name with
     | Not_found -> raise (CodegenError (UnknownVar name, loc)))
  | Typed_ast.Call (call, loc) ->
    let fn_ty =
      try Hashtbl.find fn_tys call.cname with
      | Not_found -> raise (CodegenError (UnknownVar call.cname, loc))
    in
    let is_var_arg = Llvm.is_var_arg fn_ty in
    let callee =
      match Llvm.lookup_function call.cname llvm_module with
      | Some c -> c
      | None -> raise (CodegenError (Call, loc))
    in
    let params = Llvm.params callee in
    (* if var arg, we just need to have the minimum required arguments (like print requires a str), otherwise, equal the exact amount *)
    if ((not is_var_arg) && Array.length params == List.length call.cargs)
       || (is_var_arg && Array.length params <= List.length call.cargs)
    then ()
    else
      raise
        (CodegenError (Args (Array.length params, List.length call.cargs, call.cname), loc));
    let args = Array.of_list (List.map codegen_expr call.cargs) in
    let call_assign_name = if call.ctype == Ast.TVoid then "" else "calltmp" in
    Llvm.build_call fn_ty callee args call_assign_name builder
  | Typed_ast.FnDef (fndef, loc) -> codegen_fn fndef loc
  | Typed_ast.Binop (bop, _) ->
    let lhs_val = codegen_expr bop.lhs in
    let rhs_val = codegen_expr bop.rhs in
    (match bop.bop with
     | Ast.Plus -> Llvm.build_add lhs_val rhs_val "addtmp" builder
     | Ast.Mul -> Llvm.build_mul lhs_val rhs_val "multmp" builder)
  | Typed_ast.Let (let_expr, loc) ->
    let bound_expr = codegen_expr let_expr.lbinding in
    let alloc_type = map_type_def let_expr.ltype loc in
    let var = Llvm.build_alloca alloc_type let_expr.lname builder in
    let _ = Llvm.build_store bound_expr var builder in
    Hashtbl.add named_values let_expr.lname bound_expr;
    bound_expr
  | Typed_ast.TypeDef (tc, loc) ->
    (match tc with
     | Typed_ast.EnumDef ed ->
       let rec get_largest_size ?(size = 0) variants =
         match variants with
         | [] -> size
         | v :: vs ->
           (match v with
            | Typed_ast.Tag _ -> get_largest_size ~size vs
            | Typed_ast.Union (_, tys) ->
              let ty_size =
                List.fold_left
                  (fun size ty ->
                    let tsize = Typed_ast.size_of ty in
                    size + tsize)
                  0
                  tys
              in
              (* let ty_size = Typed_ast.size_of ty in *)
              if ty_size > size
              then get_largest_size ~size:ty_size vs
              else get_largest_size ~size vs
            | Typed_ast.Struct (_, sdef) ->
              let ty_size = Typed_ast.struct_size (Array.to_list sdef.sfields) in
              if ty_size > size
              then get_largest_size ~size:ty_size vs
              else get_largest_size ~size vs)
       in
       let generate_variant = function
         | Typed_ast.Tag name ->
           let tagged_variant = Llvm.named_struct_type context (ed.ename ^ ":" ^ name) in
           Llvm.struct_set_body tagged_variant [| i8_type |] false
         | Typed_ast.Union (name, tys) ->
           let union_variant = Llvm.named_struct_type context (ed.ename ^ ":" ^ name) in
           let mapped_tys = List.map (fun ty -> map_type_def ty loc) tys in
           let body = Array.of_list (i8_type :: mapped_tys) in
           Llvm.struct_set_body union_variant body false
         | Typed_ast.Struct (name, sdef) ->
           let field_types = Array.map (fun (ty, l) -> map_type_def ty l) sdef.sfields in
           let field_types = Array.append [| i8_type |] field_types in
           let struct_variant = Llvm.named_struct_type context (ed.ename ^ ":" ^ name) in
           Llvm.struct_set_body struct_variant field_types false
       in
       let enum_size = get_largest_size ed.evariants in
       let memory_length = enum_size / 8 in
       let enum = Llvm.named_struct_type context ed.ename in
       Llvm.struct_set_body
         enum
         [| i8_type; Llvm.array_type i8_type memory_length |]
         false;
       List.iter generate_variant ed.evariants;
       Llvm.const_null enum
     | Typed_ast.StructDef s ->
       let named_struct = create_struct s.sname s.sfields in
       Llvm.const_null named_struct)
  | Typed_ast.TypeConstruct (tc, loc) ->
    (match tc with
     | Typed_ast.EnumConst ec ->
       (* we dont need to bitcast anymore with opaque pointers *)
       (* we just check tag and gep to the data segment *)
       (* https://releases.llvm.org/16.0.0/docs/OpaquePointers.html *)
       let variant_type =
         match Llvm.type_by_name llvm_module (ec.ecname ^ ":" ^ ec.ecvariant) with
         | Some ty -> ty
         | None -> raise (CodegenError (UnknownType, loc))
       in
       let enum_alloc = Llvm.build_alloca variant_type ec.ecname builder in
       let gep = Llvm.build_struct_gep variant_type enum_alloc 0 "enum_tag_idx" builder in
       let _ = Llvm.build_store (Llvm.const_int i8_type ec.ecidx) gep builder in
       (* fill enum data *)
       let _ =
         match ec.ecdata with
         | Some data ->
           (match data with
            | Typed_ast.UnionData exprs ->
              List.iteri
                (fun i e ->
                  let data_gep =
                    Llvm.build_struct_gep
                      variant_type
                      enum_alloc
                      (i + 1)
                      "enum_data"
                      builder
                  in
                  let _ = Llvm.build_store (codegen_expr e) data_gep builder in
                  ())
                exprs
            | Typed_ast.StructData fields ->
              Array.iteri
                (fun i (e, _) ->
                  let field_gep =
                    Llvm.build_struct_gep
                      variant_type
                      enum_alloc
                      (i + 1)
                      "enum_field"
                      builder
                  in
                  let _ = Llvm.build_store (codegen_expr e) field_gep builder in
                  ())
                fields)
         | None -> ()
       in
       enum_alloc
     | Typed_ast.StructConst sc ->
       let struct_type = Llvm.type_by_name llvm_module sc.scname in
       let struct_type =
         match struct_type with
         | Some t -> t
         | None -> raise (CodegenError (UnknownType, loc))
       in
       let args = sc.scfields in
       let struct_alloc = Llvm.build_alloca struct_type sc.scname builder in
       Array.iteri
         (fun i (e, _) ->
           let v = codegen_expr e in
           let gep =
             Llvm.build_struct_gep struct_type struct_alloc i (string_of_int i) builder
           in
           let _ = Llvm.build_store v gep builder in
           ())
         args;
       struct_alloc)
  | Typed_ast.Return (e, _, loc) ->
    (try codegen_expr e with
     | CodegenError (_, _) -> raise (CodegenError (ReturnType, loc)))
  | Typed_ast.FieldAccess (faccess, loc) ->
    let var =
      try Hashtbl.find named_values faccess.fvarname with
      | Not_found -> raise (CodegenError (UnknownVar faccess.fvarname, loc))
    in
    let struct_type = Llvm.type_by_name llvm_module faccess.ftypename in
    let struct_type =
      match struct_type with
      | Some ty -> ty
      | None -> raise (CodegenError (UnknownType, loc))
    in
    let gep =
      Llvm.build_struct_gep
        struct_type
        var
        faccess.ffieldidx
        (string_of_int faccess.ffieldidx)
        builder
    in
    (match faccess.ffieldtype with
     | Ast.TInt | Ast.TBool ->
       Llvm.build_load (map_type_def faccess.ffieldtype loc) gep "loadfield" builder
     | _ -> raise (CodegenError (NotSupported, loc)))
  | Typed_ast.Match (m, loc) ->
    let parent_fn = Llvm.block_parent (Llvm.insertion_block builder) in
    let expr = codegen_expr m.mexpr in
    let has_default = ref false in
    let default_block = Llvm.append_block context "default" parent_fn in
    let bottom_block = Llvm.append_block context "match_finish" parent_fn in
    let case = Llvm.build_load i32_type expr "case" builder in
    let switch = Llvm.build_switch case default_block (List.length m.mcases) builder in
    let generate_case_block (case : Typed_ast.match_case) =
      let type_name = Typed_ast.string_of_type case.casety in
      let to_remove =
        match case.casekind with
        | Typed_ast.EnumMatch kind ->
          let enum_type =
            match Llvm.type_by_name llvm_module type_name with
            | Some ty -> ty
            | None -> raise (CodegenError (UnknownType, loc))
          in
          let make_match_block () =
            let block =
              Llvm.append_block
                context
                (Typed_ast.string_of_type case.casety ^ "_case")
                parent_fn
            in
            Llvm.position_at_end block builder;
            block
          in
          (match kind with
           | Typed_ast.UnionMatch (ids, _) ->
             let block = make_match_block () in
             let enum_id =
               Llvm.const_int i32_type (Typed_ast.get_variant_id_for_name type_name)
             in
             Llvm.add_case switch enum_id block;
             List.mapi
               (fun i (id, _) ->
                 let gep = Llvm.build_struct_gep enum_type expr (i + 1) id builder in
                 Hashtbl.add named_values id gep;
                 id)
               ids
           | StructMatch (name, _) ->
             let block = make_match_block () in
             let enum_id =
               Llvm.const_int i32_type (Typed_ast.get_variant_id_for_name type_name)
             in
             Llvm.add_case switch enum_id block;
             Hashtbl.add named_values name expr;
             [ name ]
           | TagMatch ->
             let block = make_match_block () in
             let enum_id =
               Llvm.const_int i32_type (Typed_ast.get_variant_id_for_name type_name)
             in
             Llvm.add_case switch enum_id block;
             [])
        | Typed_ast.DefaultMatch ->
          has_default := true;
          Llvm.position_at_end default_block builder;
          []
      in
      let _ = codegen_expr case.caseexpr in
      let _ = Llvm.build_br bottom_block builder in
      List.iter (fun id -> Hashtbl.remove named_values id) to_remove
    in
    List.iter generate_case_block m.mcases;
    print_endline (string_of_bool !has_default);
    (match !has_default with
     | true -> ()
     | false ->
       Llvm.position_at_end default_block builder;
       let _ = Llvm.build_unreachable builder in
       ());
    let blocks = Llvm.basic_blocks parent_fn in
    let last_block = Array.get blocks (Array.length blocks - 1) in
    Llvm.move_block_after last_block bottom_block;
    Llvm.position_at_end bottom_block builder;
    Llvm.const_null i32_type

and codegen_fn fndef loc =
  (* Hashtbl.clear named_values; *)
  let args = Array.of_list fndef.fnparams in
  let type_expr = fndef.fnret in
  let fn_ret_type = map_type_def type_expr loc in
  let param_names, param_types, _ = Util.split_tuples fndef.fnparams in
  let param_names = Array.of_list param_names in
  let param_types =
    Array.of_list (List.map (fun ty -> map_type_def ty loc) param_types)
  in
  let ft = Llvm.function_type fn_ret_type param_types in
  let fn =
    match Llvm.lookup_function fndef.fnname llvm_module with
    | None -> Llvm.declare_function fndef.fnname ft llvm_module
    | Some f ->
      if Array.length (Llvm.basic_blocks f) == 0
      then ()
      else raise (CodegenError (Redef, loc));
      if Array.length (Llvm.params f) == Array.length args
      then ()
      else raise (CodegenError (Redef, loc));
      f
  in
  Array.iteri
    (fun i a ->
      let n = param_names.(i) in
      Llvm.set_value_name n a;
      Hashtbl.add named_values n a)
    (Llvm.params fn);
  let bb = Llvm.append_block context "entry" fn in
  Llvm.position_at_end bb builder;
  try
    (* ew yuck mutability *)
    let ret_val = ref None in
    List.iter
      (fun e ->
        match e with
        | Typed_ast.Return (_, _, _) -> ret_val := Some (codegen_expr e)
        | _ ->
          let _ = codegen_expr e in
          ())
      fndef.fnexprs;
    let _ =
      match type_expr with
      | Ast.TVoid -> Llvm.build_ret_void builder
      | _ ->
        (match !ret_val with
         | Some r -> Llvm.build_ret r builder
         | None -> raise (CodegenError (ReturnType, loc)))
    in
    Llvm_analysis.assert_valid_function fn;
    Hashtbl.add fn_tys fndef.fnname ft;
    fn
  with
  | e ->
    Llvm.delete_function fn;
    raise e
;;
