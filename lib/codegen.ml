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
let i1_type = Llvm.i1_type context
let struct_type = Llvm.struct_type context
let named_struct_type = Llvm.named_struct_type context
let strings = ref 0

let map_type_def ty loc =
  match ty with
  | Ast.TInt -> i32_type
  | Ast.TVoid -> void_type
  | Ast.TBool -> i1_type
  | Ast.TCustom name ->
    (match Llvm.type_by_name llvm_module name with
     (* | Some ty -> ty *)
     | Some _ -> Llvm.pointer_type context
     | None -> raise (CodegenError (UnknownType, loc)))
  | _ -> raise (CodegenError (UnknownType, loc))
;;

let try_load ty loc value =
  match Llvm.string_of_lltype (Llvm.type_of value) = "ptr" with
  | false -> value
  | true ->
    (match ty with
     | Ast.TCustom _ -> value
     | _ -> Llvm.build_load (map_type_def ty loc) value "" builder)
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
    let args =
      Array.of_list
        (List.map
           (fun arg ->
             match arg with
             | Typed_ast.Variable (name, ty, _) ->
               let var_ptr = codegen_expr arg in
               let var_type = Llvm.type_of var_ptr in
               (match Llvm.string_of_lltype var_type = "ptr" with
                | false -> var_ptr
                | true ->
                  (match ty with
                   | TCustom _ -> var_ptr
                   | _ -> Llvm.build_load (map_type_def ty loc) var_ptr name builder))
             | _ -> codegen_expr arg)
           call.cargs)
    in
    let call_assign_name = if call.ctype == Ast.TVoid then "" else "calltmp" in
    Llvm.build_call fn_ty callee args call_assign_name builder
  | Typed_ast.FnDef (fndef, loc) -> codegen_fn fndef loc
  | Typed_ast.Binop (bop, loc) ->
    let lhs_val = codegen_expr bop.lhs in
    let rhs_val = codegen_expr bop.rhs in
    let rhs_val = try_load bop.btype loc rhs_val in
    let lhs_val = try_load bop.btype loc lhs_val in
    (match bop.bop with
     | Ast.Plus -> Llvm.build_add lhs_val rhs_val "add" builder
     | Ast.Minus -> Llvm.build_sub lhs_val rhs_val "sub" builder
     | Ast.Mul -> Llvm.build_mul lhs_val rhs_val "mul" builder
     | Ast.Eq -> Llvm.build_icmp Llvm.Icmp.Eq lhs_val rhs_val "eq" builder)
  | Typed_ast.Let (let_expr, loc) ->
    let bound_expr = codegen_expr let_expr.lbinding in
    let var =
      match let_expr.ltype with
      | Ast.TCustom _ -> bound_expr
      | _ ->
        let alloc_type = map_type_def let_expr.ltype loc in
        let var = Llvm.build_alloca alloc_type let_expr.lname builder in
        let _ = Llvm.build_store bound_expr var builder in
        var
    in
    Hashtbl.add named_values let_expr.lname var;
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
       let enum_alloc = Llvm.build_alloca variant_type "" builder in
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
       let struct_alloc = Llvm.build_alloca struct_type "" builder in
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
  | Typed_ast.Return (e, ty, loc) ->
    let r =
      try_load
        ty
        loc
        (try codegen_expr e with
         | CodegenError (_, _) -> raise (CodegenError (ReturnType, loc)))
    in
    Llvm.build_ret r builder
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
               (fun i (id, ty) ->
                 let gep = Llvm.build_struct_gep enum_type expr (i + 1) "" builder in
                 let load = Llvm.build_load (map_type_def ty loc) gep id builder in
                 Hashtbl.add named_values id load;
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
  | Typed_ast.Assignment (a, loc) ->
    let name = a.aslhs in
    let var =
      try Hashtbl.find named_values name with
      | Not_found -> raise (CodegenError (UnknownVar name, loc))
    in
    let rhs = codegen_expr a.asrhs in
    Llvm.build_store rhs var builder
  | Typed_ast.If (if_expr, loc) ->
    let if_cond, cond_block = if_expr.cond_blocks in
    let if_condition = codegen_expr if_cond in
    let current_block = Llvm.insertion_block builder in
    let parent_fn = Llvm.block_parent current_block in
    let then_block = Llvm.append_block context "then_block" parent_fn in
    let else_block = Llvm.append_block context "else_block" parent_fn in
    let final_block = Llvm.append_block context "if_bottom_block" parent_fn in
    let return, ret_ty =
      match if_expr.has_return with
      | Some ty -> Llvm.build_alloca (map_type_def ty loc) "ifreturn" builder, ty
      | _ -> Llvm.const_null i32_type, Ast.TVoid
    in
    let _ = Llvm.build_cond_br if_condition then_block else_block builder in
    (* generate then block *)
    Llvm.position_at_end then_block builder;
    let final_block_jumps = ref 0 in
    let gen_code_block exprs =
      let has_return = ref false in
      let _ =
        List.map
          (fun e ->
            match e with
            | Typed_ast.Return (_, _, _) ->
              has_return := true;
              codegen_expr e
            | Typed_ast.If (if_expr, _) ->
              (match if_expr.has_return with
               | Some _ -> has_return := true
               | None -> ());
              codegen_expr e
            | _ -> codegen_expr e)
          exprs
      in
      if !has_return = false
      then (
        final_block_jumps := !final_block_jumps + 1;
        let _ = Llvm.build_br final_block builder in
        ())
      else ()
    in
    gen_code_block cond_block;
    (* let _ = List.map codegen_expr cond_block in *)
    (* let _ = Llvm.build_br final_block builder in *)
    (* generate else block or define as unreachable *)
    Llvm.position_at_end else_block builder;
    (match if_expr.else_block with
     | Some exprs -> gen_code_block exprs
     | None ->
       let _ = Llvm.build_unreachable builder in
       ());
    if !final_block_jumps = 0
    then (
      Llvm.delete_block final_block;
      Llvm.const_null i32_type)
    else (
      Llvm.position_at_end final_block builder;
      match ret_ty with
      | Ast.TVoid -> return
      | _ -> Llvm.build_load (map_type_def ret_ty loc) return "" builder)
  | Typed_ast.For (for_expr, loc) ->
    let current_block = Llvm.insertion_block builder in
    let parent_fn = Llvm.block_parent current_block in
    let top_block = Llvm.append_block context "for_top" parent_fn in
    let bottom_block = Llvm.append_block context "bottom_block" parent_fn in
    let iter_ty = map_type_def for_expr.fty loc in
    let iterator = Llvm.build_alloca iter_ty for_expr.fident builder in
    Hashtbl.add named_values for_expr.fident iterator;
    (match for_expr.fkind with
     | Typed_ast.Range (start, finish) ->
       Llvm.position_at_end current_block builder;
       let const_start = Llvm.const_int iter_ty start in
       let const_finish = Llvm.const_int iter_ty finish in
       let const1 = Llvm.const_int iter_ty 1 in
       let _ = Llvm.build_store const_start iterator builder in
       let _ = Llvm.build_br top_block builder in
       Llvm.position_at_end top_block builder;
       let _ = List.map codegen_expr for_expr.fblock in
       let iter_load = Llvm.build_load iter_ty iterator "iterload" builder in
       let iter_increment = Llvm.build_add iter_load const1 "iterincrement" builder in
       let _ = Llvm.build_store iter_increment iterator builder in
       let icmp = Llvm.build_icmp Llvm.Icmp.Sle iter_increment const_finish "" builder in
       let _ = Llvm.build_cond_br icmp top_block bottom_block builder in
       ());
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
  (* used to be added after assert valid function, but now we add it prior to function gen for recursive function calls *)
  Hashtbl.add fn_tys fndef.fnname ft;
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
        | Typed_ast.If (if_expr, _) ->
          (match if_expr.has_return with
           | Some _ -> ret_val := Some (codegen_expr e)
           | _ -> ())
        | _ ->
          let _ = codegen_expr e in
          ())
      fndef.fnexprs;
    let _ =
      match type_expr with
      | Ast.TVoid -> Llvm.build_ret_void builder
      | _ ->
        (match !ret_val with
         | Some r -> r
         | None -> raise (CodegenError (ReturnType, loc)))
    in
    Llvm_analysis.assert_valid_function fn;
    fn
  with
  | e ->
    Llvm.delete_function fn;
    raise e
;;
