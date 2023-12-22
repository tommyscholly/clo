type errorty =
  | Call
  | Defn
  | Redef
  | UnknownVar
  | Args
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

let ( ++ ) x f =
  f x;
  x
;;

let map_type_def ty loc =
  match ty with
  | Ast.TInt -> i32_type
  | Ast.TVoid -> void_type
  | Ast.TBool -> bool_type
  | _ -> raise (CodegenError (UnknownType, loc))
;;

let register_extern_functions () =
  let ft = Llvm.var_arg_function_type void_type [| Llvm.pointer_type context |] in
  Hashtbl.add fn_tys "printf" ft;
  Llvm.declare_function "printf" ft llvm_module
;;

let rec codegen_expr = function
  | Ast.Int n -> Llvm.const_int i32_type n
  | Ast.Float n -> Llvm.const_float double_type n
  | Ast.Bool b -> Llvm.const_int bool_type (if b then 1 else 0)
  | Ast.Variable (name, loc) ->
    (try Hashtbl.find named_values name with
     | Not_found -> raise (CodegenError (UnknownVar, loc)))
  | Ast.Call (callee, args, loc) ->
    let callee =
      match Llvm.lookup_function callee llvm_module with
      | Some c -> c
      | None -> raise (CodegenError (Call, loc))
    in
    let params = Llvm.params callee in
    if Array.length params == List.length args
    then ()
    else raise (CodegenError (Args, loc));
    let args = Array.of_list (List.map codegen_expr args) in
    Llvm.build_call (Llvm.double_type context) callee args "calltmp" builder
  | Ast.Function ((name, params, type_expr, body), loc) ->
    codegen_fn name params type_expr body loc
  | Ast.Binop (bop, lhs, rhs, _) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    (match bop with
     | Ast.Plus -> Llvm.build_add lhs_val rhs_val "addtmp" builder
     | Ast.Mul -> Llvm.build_mul lhs_val rhs_val "multmp" builder)
  | Ast.Print (str, args, loc) ->
    let print_fn = Llvm.lookup_function "printf" llvm_module in
    let fn =
      match print_fn with
      | Some fn -> fn
      | None ->
        Llvm.dump_module llvm_module;
        raise (CodegenError (NotSupported, loc))
    in
    let fn_ty =
      try Hashtbl.find fn_tys "printf" with
      | Not_found -> raise (CodegenError (UnknownVar, loc))
    in
    let str_ptr = Llvm.build_global_stringptr str "format_str" builder in
    let args = List.map (fun e -> codegen_expr e) args in
    let args = Array.of_list (str_ptr :: args) in
    let call = Llvm.build_call fn_ty fn args "" builder in
    (* let call = Llvm.build_call fn args "calltmp" builder in *)
    call
  | Ast.Let (name, type_expr, binding, loc) ->
    let bound_expr = codegen_expr binding in
    (* let block = Llvm.insertion_block builder in *)
    (* let parent_fn = Llvm.block_parent block in *)
    (* print_endline "1"; *)
    (* let tmp_builder = Llvm.builder_at context (Llvm.instr_succ parent_fn)  in *)
    (* print_endline "2"; *)
    (* let bound_ty = Llvm.type_of bound_expr in *)
    let alloc_type = map_type_def type_expr loc in
    let var = Llvm.build_alloca alloc_type name builder in
    let _ = Llvm.build_store bound_expr var builder in
    Hashtbl.add named_values name bound_expr;
    bound_expr
  | Ast.Return (_, loc) -> raise (CodegenError (NotSupported, loc))

and codegen_fn name params type_expr body loc =
  Hashtbl.clear named_values;
  let args = Array.of_list params in
  let type_expr =
    match type_expr with
    | Some t -> t
    | None -> Ast.TVoid
  in
  let fn_ret_type = map_type_def type_expr loc in
  let split = List.split params in
  let param_names = Array.of_list (fst split) in
  let param_types =
    Array.of_list (List.map (fun ty -> map_type_def ty loc) (snd split))
  in
  let ft = Llvm.function_type fn_ret_type param_types in
  let fn =
    match Llvm.lookup_function name llvm_module with
    | None -> Llvm.declare_function name ft llvm_module
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
        | Ast.Return (_, _) -> ret_val := Some (codegen_expr e)
        | _ ->
          let _ = codegen_expr e in
          ())
      body;
    let _ =
      match type_expr with
      | Ast.TVoid -> Llvm.build_ret_void builder
      | _ ->
        (match !ret_val with
         | Some r -> Llvm.build_ret r builder
         | None -> raise (CodegenError (ReturnType, loc)))
    in
    Llvm_analysis.assert_valid_function fn;
    fn
  with
  | e ->
    Llvm.delete_function fn;
    raise e
;;
