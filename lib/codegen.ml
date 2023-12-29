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
let fn_tys = Hashtbl.create 10

(* types *)
let double_type = Llvm.double_type context
let i32_type = Llvm.i32_type context
let void_type = Llvm.void_type context
let bool_type = Llvm.i8_type context
let struct_type = Llvm.struct_type context
let named_struct_type = Llvm.named_struct_type context
let strings = ref 0

let map_type_def ty loc =
  match ty with
  | Ast.TInt -> i32_type
  | Ast.TVoid -> void_type
  | Ast.TBool -> bool_type
  | Ast.TCustom name ->
    (match Llvm.type_by_name llvm_module name with
     | Some ty -> ty
     | None -> raise (CodegenError (UnknownType, loc)))
  | _ -> raise (CodegenError (UnknownType, loc))
;;

let register_extern_functions () =
  let ft = Llvm.var_arg_function_type void_type [| Llvm.pointer_type context |] in
  Hashtbl.add fn_tys "printf" ft;
  Llvm.declare_function "printf" ft llvm_module
;;

let rec codegen_expr = function
  | Typed_ast.Int n -> Llvm.const_int i32_type n
  | Typed_ast.Float n -> Llvm.const_float double_type n
  | Typed_ast.Bool b -> Llvm.const_int bool_type (if b then 1 else 0)
  | Typed_ast.Str s ->
    let str_int = !strings in
    strings := !strings + 1;
    Llvm.build_global_stringptr s (Format.sprintf "string_%d" str_int) builder
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
  (* | Ast.Print (str, args, loc) -> *)
  (*   let print_fn = Llvm.lookup_function "printf" llvm_module in *)
  (*   let fn = *)
  (*     match print_fn with *)
  (*     | Some fn -> fn *)
  (*     | None -> *)
  (*       Llvm.dump_module llvm_module; *)
  (*       raise (CodegenError (NotSupported, loc)) *)
  (*   in *)
  (*   let fn_ty = *)
  (*     try Hashtbl.find fn_tys "printf" with *)
  (*     | Not_found -> raise (CodegenError (UnknownVar, loc)) *)
  (*   in *)
  (*   let str_ptr = Llvm.build_global_stringptr str "format_str" builder in *)
  (*   let args = List.map (fun e -> codegen_expr e) args in *)
  (*   let args = Array.of_list (str_ptr :: args) in *)
  (*   Llvm.build_call fn_ty fn args "" builder *)
  | Typed_ast.Let (let_expr, loc) ->
    let bound_expr = codegen_expr let_expr.lbinding in
    (* let block = Llvm.insertion_block builder in *)
    (* let parent_fn = Llvm.block_parent block in *)
    (* print_endline "1"; *)
    (* let tmp_builder = Llvm.builder_at context (Llvm.instr_succ parent_fn)  in *)
    (* print_endline "2"; *)
    (* let bound_ty = Llvm.type_of bound_expr in *)
    let alloc_type = map_type_def let_expr.ltype loc in
    let var = Llvm.build_alloca alloc_type let_expr.lname builder in
    let _ = Llvm.build_store bound_expr var builder in
    Hashtbl.add named_values let_expr.lname bound_expr;
    bound_expr
  | Typed_ast.TypeDef (tc, loc) ->
    (match tc with
     | Typed_ast.EnumDef -> raise (CodegenError (NotSupported, loc))
     | Typed_ast.StructDef s ->
       let field_types = Array.map (fun (ty, l) -> map_type_def ty l) s.sfields in
       let named_struct = named_struct_type s.sname in
       Llvm.struct_set_body named_struct field_types false;
       Llvm.const_null named_struct)
  | Typed_ast.TypeConstruct (tc, loc) ->
    (match tc with
     | Typed_ast.EnumConst -> raise (CodegenError (NotSupported, loc))
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

and codegen_fn fndef loc =
  (* Hashtbl.clear named_values; *)
  let args = Array.of_list fndef.fnparams in
  let type_expr = fndef.fnret in
  let fn_ret_type = map_type_def type_expr loc in
  let (param_names, param_types, _ ) = Util.split_tuples fndef.fnparams in
  let param_names = Array.of_list param_names in
  let param_types =
    Array.of_list (List.map (fun ty -> map_type_def ty loc) (param_types))
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
  (* Llvm.dump_module llvm_module; *)
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
