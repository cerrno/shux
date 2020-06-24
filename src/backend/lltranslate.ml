module L = Llvm
open Llast

module StringMap = Map.Make(String)

let translate (structs,globals,funcs) =
  (* debug use *)
  (*
  let print_llvalue llvalue msg=
    let lltype = L.type_of llvalue in
    prerr_string ("["^msg^"]"^" lltype:"^(L.string_of_lltype lltype)^";\n")
  in
   *)

  let the_context = L.global_context () in
  let the_module = L.create_module the_context "shux" and
      i32_t = L.i32_type the_context and
      i8_t = L.i8_type the_context and
      i1_t = L.i1_type the_context and
      double_t = L.double_type the_context and
      double_precision = 16 and (* adjust this to change number of digits printf will print *)
      void_t = L.void_type the_context
  in
  let str_t = L.pointer_type i8_t in

  (* BEGIN EXTERNAL CALLS DEFINITIONS 
   *
   *)

  (* LVS *)
  (* gl init call set *)
  (*
  let glClearColor_formals = Array.of_list [float_t; float_t; float_t; float_t] in
  let glClearColor_sign = L.function_type void_t glClearColor_formals in
  let glClearColor_func = L.declare_function "glClearColor" glClearColor_sign the_module in

  let glEnable_formals = Array.of_list [i32_t] in
  let glEnable_sign = L.function_type void_t glEnable_formals in
  let glEnable_func = L.declare_function "glEnable" glEnable_sign the_module in

  let glPointSize_formals = Array.of_list [float_t] in
  let glPointSize_sign = L.function_type void_t glPointSize_formals in
  let glPointSize_func = L.declare_function "glPointSize" glPointSize_sign the_module in

  let glMatrixMode_formals = Array.of_list [i32_t] in
  let glMatrixMode_sign = L.function_type void_t glMatrixMode_formals in
  let glMatrixMode_func = L.declare_function "glMatrixMode" glMatrixMode_sign the_module in

  
  (* gl render call set *)
  let glClear_formals = Array.of_list [i32_t] in
  let glClear_sign = L.function_type void_t glClear_formals in
  let glClear_func = L.declare_function "glClear" glClear_sign the_module in

  let glLoadIdentity_formals = Array.of_list [] in
  let glLoadIdentity_sign = L.function_type void_t glLoadIdentity_formals in
  let glLoadIdentity_func = L.declare_function "glLoadIdentity" glLoadIdentity_sign  the_module in

  let glOrtho_formals = Array.of_list [float_t; float_t; float_t; float_t] in
  let glOrtho_sign = L.function_type void_t glOrtho_formals in
  let glOrtho_func = L.declare_function "glOrtho" glOrtho_sign the_module in

  let glColor4f_formals = Array.of_list [float_t; float_t; float_t; float_t] in
  let glColor4f_sign = L.function_type void_t glColor4f_formals in
  let glColor4f_func = L.declare_function "glColor4f" glColor4f_sign the_module in

  (* for the render loop *)
  let glBegin_formals = Array.of_list [i32_t] in
  let glBegin_sign = L.function_type void_t glBegin_formals in
  let glBegin_func = L.declare_function "glBegin" glBegin_sign the_module in

  let glVertex2f_formals = Array.of_list [float_t; float_t] in
  let glVertex2f_sign = L.function_type void_t glVertex2f_formals in
  let glVertex2f_func = L.declare_function "glVertex2f" glVertex2f_sign the_module in

  let glVertexArray_formals = Array.of_list [i32_t; i32_t; i32_t; L.pointer_type float_t ] in
  let glVertexArray_sign = L.function_type void_t glVertexArray_formals in
  let glVertexArray_func = L.declare_function "glVertexArray" glVertexArray_sign the_module in

  let glEnd_formals = Array.of_list [i32_t] in
  let glEnd_sign = L.function_type void_t glEnd_formals in
  let glEnd_func = L.declare_function "glEnd" glEnd_sign the_module in 

  let glutSwapBuffers_formals = Array.of_list [] in
  let glutSwapBuffers_sign = L.function_type void_t glutSwapBuffers_formals in
  let glutSwapBuffers_func = L.declare_function "glutSwapBuffers" glutSwapBuffers_sign the_module in


  (* gl update/idle call set *)
  let glutPostRedisplay_formals = Array.of_list [] in
  let glutPostRedisplay_sign = L.function_type void_t glutPostRedisplay_formals in
  let glutPostRedisplay_func = L.declare_function "glutPostRedisplay" glutPostRedisplay_sign the_module in


  (* glut main call set *)
  let glutInitWindowSize_formals = Array.of_list [i32_t; i32_t] in
  let glutInitWindowSize_sign = L.function_type void_t glutInitWindowSize_formals in
  let glutInitWindowSize_func = L.declare_function "glutInitWindowSize" glutInitWindowSize_sign the_module in

  let glutInit_formals = Array.of_list [L.pointer_type i32_t; L.pointer_type (L.pointer_type i8_t)] in
  let glutInit_sign = L.function_type void_t glutInit_formals in
  let glutInit_func = L.declare_function "glutInit" glutInit_sign the_module in 

  let glutCreateWindow_formals = Array.of_list [L.pointer_type i8_t] in
  let glutCreateWindow_sign = L.function_type i32_t glutCreateWindow_formals in
  let glutCreateWindow_func = L.declare_function "glutCreateWindow" glutCreateWindow_sign the_module in
 
  let glutDisplayFunc_formals = Array.of_list [fptr_void_t] in
  let glutDisplayFunc_sign = L.function_type void_t glutDisplayFunc_formals in
  let glutDisplayFunc_func = L.declare_function "glutDisplayFunc" glutDisplayFunc_sign the_module in

  let glutIdleFunc_formals = Array.of_list [fptr_void_t] in
  let glutIdleFunc_sign = L.function_type void_t glutIdleFunc_formals in
  let glutIdleFunc_func = L.declare_function "glutIdleFunc" glutIdleFunc_sign the_module in

  let glutMainLoop_formals = Array.of_list [] in
  let glutMainLoop_sign = L.function_type void_t glutMainLoop_formals in
  let glutMainLoop_func = L.declare_function "glutMainLoop" glutMainLoop_sign the_module in

  (* END LVS *)
  (* END EXTERNAL CALLS DEFINITIONS 
   *
   *)

  (* ************************************************************* *)

  (* LVS START initgl modifications *)
  let init_gl_func_formals = Array.of_list [] in
  let init_gl_func_sign = L.function_type void_t init_gl_func_formals in
  let init_gl_func_llvalue = L.define_function "__init_gl" init_gl_func_sign the_module in
  let builder_init_gl_func = L.builder_at_end the_context (L.entry_block init_gl_func_llvalue) in
  let _ = L.build_call glClearColor_func [| L.const_float float_t 0.9; L.const_float float_t 0.9; L.const_float float_t 0.9; L.const_float float_t 1.0 |] "" builder_init_gl_func in
  let _ = L.build_call glEnable_func [| L.const_int i32_t 2832 |] "" builder_init_gl_func in
  let _ = L.build_call glPointSize_func [| L.const_float float_t 10.0 |] "" builder_init_gl_func in
  let _ = L.build_call glMatrixMode_func [| L.const_int i32_t 5889 |] "" builder_init_gl_func in
  let _ = L.build_ret_void builder_init_gl_func in
  (* LVS END initgl modifications *)
  

  (* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)
  (* begin work on actual bindings *)

  (* graphics_do_update *)
  (* user-callable update function *)
  let idle_func_formals = Array.of_list [] in
  let idle_func_sign = L.function_type void_t idle_func_formals in
  let idle_func_llvalue = L.define_function "graphics_do_update" idle_func_sign the_module in
  let builder_idle_func = L.builder_at_end the_context (L.entry_block idle_func_llvalue) in
  let _ = L.build_call glutPostRedisplay_func [||] "" builder_idle_func in
  let _ = L.build_ret_void builder_idle_func in

  (* graphics_do_render *)
  (* a display function rendering a list of 2d scalars *)
  (* LVSTOOD doesn't take parameters yet *)
  let display_func_formals = Array.of_list [] in
  let display_func_sign = L.function_type void_t display_func_formals in
  let display_func_llvalue = L.define_function "graphics_do_render" display_func_sign the_module in
  let builder_display_func = L.builder_at_end the_context (L.entry_block display_func_llvalue) in
  let _ = L.build_call glClear_func [|L.const_int i32_t 16640|] "" builder_display_func in
  let _ = L.build_call glLoadIdentity_func [||] "" builder_display_func in
  let _ = L.build_call glOrtho_func [| L.const_float float_t 0.0; L.const_float float_t 800.0; L.const_float float_t 0.0; L.const_float float_t 600.0;|] "" builder_display_func in
  (* user code here -- testing *)
  let _ = L.build_call glColor4f_func [| L.const_float float_t 0.2; L.const_float float_t 0.6; L.const_float float_t 1.0; L.const_float float_t 1.0; |] "" builder_display_func in
  let _ = L.build_call glBegin_func [| L.const_int i32_t 0 |] "" builder_display_func in
  let _ = L.build_call glVertex2f_func [| L.const_float float_t 0.0; L.const_float float_t 0.0 |] "" builder_display_func in
  let _ = L.build_call glEnd_func [| L.const_int i32_t 0 |] "" builder_display_func in
  (* end user code *)
  let _ = L.build_call glutSwapBuffers_func [||] "" builder_display_func in
  let _ = L.build_ret_void builder_display_func in

  (* graphics init *)
  (* spoof argc for glutinit *)
  let graphics_init_formals = Array.of_list [] in
  let graphics_init_sign = L.function_type void_t graphics_init_formals in
  let graphics_init_llvalue = L.define_function "graphics_init" graphics_init_sign the_module in
  let builder_graphics_init = L.builder_at_end the_context (L.entry_block graphics_init_llvalue) in
  let argc_ptr = L.build_alloca i32_t "argc" builder_graphics_init in
  let _ = L.build_store (L.const_int i32_t 0) argc_ptr builder_graphics_init in
  let _ = L.build_call glutInitWindowSize_func [| (L.const_int i32_t 800); (L.const_int i32_t 600) |] "" builder_graphics_init in
  let _ = L.build_call glutInit_func [| argc_ptr; (L.const_pointer_null (L.pointer_type (L.pointer_type i8_t))) |] "" builder_graphics_init in
  let _ = L.build_call glutCreateWindow_func [| (L.const_pointer_null (L.pointer_type i8_t)) |] "dummy" builder_graphics_init in
  let _ = L.build_ret_void builder_graphics_init in

  (* graphics_loop *)
  (* LVSTODO doesnt take parameters yet *)
  let graphics_loop_formals = Array.of_list [] in
  let graphics_loop_sign = L.function_type void_t graphics_loop_formals in
  let graphics_loop_llvalue = L.define_function "graphics_loop" graphics_loop_sign the_module in
  let builder_graphics_loop = L.builder_at_end the_context (L.entry_block graphics_loop_llvalue) in
  let _ = L.build_call glutDisplayFunc_func [| display_func_llvalue |] "" builder_graphics_loop in
  let _ = L.build_call glutIdleFunc_func [| idle_func_llvalue |] "" builder_graphics_loop in
  let _ = L.build_call init_gl_func_llvalue [||] "" builder_graphics_loop in
  let _ = L.build_call glutMainLoop_func [||] "" builder_graphics_loop in
  let _ = L.build_ret_void builder_graphics_loop in

  (* ************************************************************** *)
   *)

  let extract_type = function
      LLRegLabel (typ, str) -> typ
    | LLRegLit (typ, lit) -> typ
    | LLRegDud -> assert false in

  (* Define all the structs *)

  let rec lleasytyp_of = function (* TODO compromise *)
        LLBool -> i1_t
      | LLInt  -> i32_t
      | LLDouble -> L.double_type the_context
      | LLConstString -> str_t
      | LLVoid -> void_t
      | LLArray (typ, len) ->
         (match len with
            Some real_len -> L.array_type (lleasytyp_of typ) real_len
          | None -> assert false
         )
      | LLStruct str -> i1_t
  in

  let define_structs = (* a map from struct name to struct type *)
    let define_struct map struc =
      let (struct_name, typelist) = struc in
      let struct_typ = L.named_struct_type the_context struct_name in
      ignore (L.struct_set_body struct_typ (Array.of_list (List.map lleasytyp_of typelist)) false);
      StringMap.add struct_name struct_typ map
    in
    List.fold_left define_struct StringMap.empty structs
  in

  let rec lltyp_of = function (* TODO compromise *)
    | LLStruct str -> StringMap.find str define_structs
    | a -> lleasytyp_of a
  in

  let get_struct_by_name struct_name =
      StringMap.find struct_name define_structs
  in

  let promote lit = (* promote two  *)
    let llast_typ = extract_type lit in
    (match llast_typ with
     LLStruct str -> L.pointer_type (lltyp_of llast_typ)
     | LLArray (typ, len) -> L.pointer_type (lltyp_of typ)
     | _ -> lltyp_of llast_typ
    )
  in

  let lit_to_llvalue = function
    | LLLitBool b -> L.const_int i1_t (if b then 1 else 0)
    | LLLitInt i -> L.const_int i32_t i
    | LLLitDouble f -> L.const_float double_t f
    | LLLitString s -> (L.define_global "stringlit" (L.const_stringz the_context s) the_module)
    | _ -> assert false
  in

  (* define global variables *)
  let define_globals =
    let define_global map var =
      let (gtyp, gname, glit) = var in
      let init_val = (match glit with
                        LLLitStruct list ->
                        let struct_name = (match gtyp with LLStruct sname -> sname | _ -> assert false) in
                        let struct_lltype = get_struct_by_name struct_name in
                        let list_llvalues = List.map lit_to_llvalue list in
                        ignore(L.const_named_struct struct_lltype (Array.of_list list_llvalues));
                        assert false;
                      | LLLitArray _ -> assert false
                      | x -> lit_to_llvalue x
                     ) in
      let global_llvalue = L.define_global gname init_val the_module in
      StringMap.add gname global_llvalue map in
    List.fold_left define_global StringMap.empty globals in

  (* Define the printf function *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
  let int_format_str =
    let str_arr_ptr = L.define_global "fmti" (L.const_stringz the_context "%d\n") the_module in
    L.const_in_bounds_gep str_arr_ptr [| L.const_int i32_t 0; L.const_int i32_t 0|] and
      float_format_str =
        let formatter = "%."^(string_of_int double_precision)^"f\n" in
        let str_arr_ptr = L.define_global "fmti" (L.const_stringz the_context formatter) the_module in
    L.const_in_bounds_gep str_arr_ptr [| L.const_int i32_t 0; L.const_int i32_t 0|] in

  let define_funcs =
    let translate_func map func=
      let fname = func.llfname and
          fformals = Array.of_list (List.map promote (func.llfformals))
      in
      let func_sign = L.function_type (lltyp_of func.llfreturn) fformals in
      let func_def = L.define_function fname func_sign the_module in
     StringMap.add fname (func_def, func) map
    in
    List.fold_left translate_func StringMap.empty funcs in

  let get_func_by_name fname =
    if(StringMap.mem fname define_funcs= false)
    then (print_string (fname^" not found\n"); assert false;)
    else(
    let func_llvalue, _ = StringMap.find fname define_funcs in
    func_llvalue
    )
  in

  let _ =
    let build_func func =
      let (the_function, _) = StringMap.find func.llfname define_funcs in
      let builder = L.builder_at_end the_context (L.entry_block the_function) in

      let formals_list = Array.to_list (L.params the_function) in

      let get_reg_typ_name = function
          LLRegLabel (typ, str) -> (typ, str)
        | LLRegLit (typ,lit) -> (typ,"nonameliteral")
        | LLRegDud -> assert false in

      let build_formals = (* this is a map from formal name to its stack ptr *)
        let build_formal map formal_def formal_param =
          let (formal_type,formal_name) = get_reg_typ_name formal_def in
          (match formal_type with
             LLArray (typ, len) ->
             let double_ptr = L.build_alloca (L.type_of formal_param) "alloc_ptr_arr_formal" builder in
             ignore(L.build_store formal_param double_ptr builder);
             StringMap.add formal_name double_ptr map
           | LLStruct struct_name ->
              let double_ptr = L.build_alloca (L.type_of formal_param) "alloc_ptr_struct_formal" builder in
              ignore(L.build_store formal_param double_ptr builder);
              StringMap.add formal_name double_ptr map
           | _ -> let formal_ptr = L.build_alloca (lltyp_of formal_type) formal_name builder in
                  ignore(L.build_store formal_param formal_ptr builder);
                  StringMap.add formal_name formal_ptr map
          ) in
        List.fold_left2 build_formal StringMap.empty func.llfformals formals_list
      in

      let build_locals = (* this is a map from local name to its stack ptr *)
        let build_local map local_def =
          let (local_type, local_name) = get_reg_typ_name local_def in
          let local_ptr =
            (match local_type with
               LLArray (typ, len) ->
               let aggptr = L.build_alloca (lltyp_of local_type) local_name builder in
               let ptr_to_first = L.build_in_bounds_gep aggptr [| L.const_int i32_t 0; L.const_int i32_t 0 |]
                                                        "build_local_arr" builder in
               let double_ptr = L.build_alloca (L.type_of ptr_to_first) "alloc_ptr_arr" builder in
               ignore(L.build_store ptr_to_first double_ptr builder);
               double_ptr
             | LLStruct struct_name ->
                let struct_typ = get_struct_by_name struct_name in
                let struct_ptr = L.build_alloca struct_typ "build_local_struct" builder in
                let double_ptr = L.build_alloca (L.type_of struct_ptr) "alloc_ptr_struct" builder in
                ignore(L.build_store struct_ptr double_ptr builder);
                double_ptr
             | _ -> L.build_alloca (lltyp_of local_type) local_name builder
            ) in
          StringMap.add local_name local_ptr map in
        List.fold_left build_local StringMap.empty func.llflocals
      in

      let define_blocks =
        let define_block map block_def =
          let block_name = block_def.llbname and block_stmts = block_def.llbbody in
          let block_llvalue = L.append_block the_context block_name the_function in
          StringMap.add block_name (block_stmts, block_llvalue) map in
        List.fold_left define_block StringMap.empty func.llfblocks
      in

      let get_block_by_name bname =
        let stmts, block_llvalue = StringMap.find bname define_blocks in
        block_llvalue in

      (* helper function starts here *)
      let llvalue_of_lit typ block_builder = function
          LLLitBool bool -> L.const_int i1_t (if bool then 1 else 0)
        | LLLitInt int -> L.const_int i32_t int
        | LLLitDouble double ->L.const_float double_t double
        | LLLitString str -> L.build_global_stringptr str "globalstr" block_builder
        | LLLitArray litlist -> assert false
        | LLLitStruct litlist -> assert false
      in

      let get_reg block_builder = function
          LLRegLabel (typ, regname) ->
          let ret =
          if (StringMap.mem regname build_formals)
          then (StringMap.find regname build_formals)
          else (
            if (StringMap.mem regname build_locals)
            then (StringMap.find regname build_locals)
            else ( if(StringMap.mem regname define_globals)
                   then (StringMap.find regname define_globals)
                   else (print_string (regname^" not found\n"); assert false)
                 )
          ) in
          ret
        | LLRegLit (typ, literal) ->
           let literal_ptr = L.build_alloca (lltyp_of typ) "lit_alloc_inst" block_builder in
           ignore(L.build_store (llvalue_of_lit typ block_builder literal) literal_ptr block_builder);
           literal_ptr
        | LLRegDud -> assert false
      in

      let load_reg ptrreglabel block_builder =
        let reg = (get_reg block_builder ptrreglabel) in
        L.build_load  reg "loadinst" block_builder
      in

      let store_reg ptrreglabel val_to_store block_builder=
        L.build_store val_to_store (get_reg block_builder ptrreglabel) block_builder in

      let make_tuple list element =
        let bundle a = (a,element) in
        List.map bundle list
      in

      let transform_funclabel_list_to_llvalue label_list block_builder =
        let builder_list = List.map (fun _ -> block_builder) label_list in
        let result = List.map2 load_reg label_list builder_list in
        Array.of_list result in

      (* helper functions end here *)

      let build_terminator block_builder = function
          LLBlockReturn label -> L.build_ret (load_reg label block_builder) block_builder
        | LLBlockReturnVoid -> L.build_ret_void block_builder
        | LLBlockBr (label,brname1,brname2) ->
           L.build_cond_br (load_reg label block_builder)
                           (get_block_by_name brname1) (get_block_by_name brname2) block_builder
        | LLBlockJmp brname -> L.build_br (get_block_by_name brname) block_builder
      in

      let bind_block_to_stmt stmts = function
          "entry" -> make_tuple stmts builder
        | bname -> let (_,block_llvalue) = StringMap.find bname define_blocks in
                  make_tuple stmts (L.builder_at_end the_context block_llvalue)
      in

      let get_arr_ptr_by_labels agglabel indexlabel block_builder=
        let aggreg = load_reg agglabel block_builder and indexreg = load_reg indexlabel block_builder in
        L.build_in_bounds_gep aggreg [| indexreg |] "get_arr_inst" block_builder
      in

      let get_struct_ptr_by_labels agglabel index block_builder=
        let aggreg = load_reg agglabel block_builder in
        L.build_struct_gep aggreg index "get_struct_inst" block_builder
      in

      let build_stmt map block_stmt=
        let (stmt, block_builder) = block_stmt in
        let stmt_llvalue =
          (match stmt with
             LLBuildPrintCall label
             -> let argarr = (match (extract_type label) with
                                LLInt -> [|int_format_str; (load_reg label block_builder) |]
                              | LLDouble -> [|float_format_str; (load_reg label block_builder) |]
                              | LLConstString -> [| (load_reg label block_builder) |]
                              | _ -> [| |]) in (* TODO are we printing more things *)
             L.build_call printf_func argarr "printwhatever" block_builder
           | LLBuildCall (fname,label_list,labelret) ->
              let func_llvalue = get_func_by_name fname and
                  func_formal_array_llvalue = transform_funclabel_list_to_llvalue label_list block_builder in
              (match labelret with
                 Some label->
                 let call_ret_reg =
                   L.build_call func_llvalue func_formal_array_llvalue "call_ret" block_builder in
                 ignore(store_reg label call_ret_reg block_builder);
                 call_ret_reg
               | None -> ignore(L.build_call func_llvalue func_formal_array_llvalue "" block_builder);
                         L.const_int i32_t 0
              )
           | LLBuildBinOp (optyp,label1,label2,labelresult) ->
              let reg1 = load_reg label1 block_builder and reg2 = load_reg label2 block_builder in
              let icmpfinder = function
                  LLLT -> L.Icmp.Slt
                | LLEQ -> L.Icmp.Eq
                | LLGT -> L.Icmp.Sgt
                | LLLE -> L.Icmp.Sle
                | LLGE -> L.Icmp.Sge
                and
                  fcmpfinder = function
                  | LLFLT -> L.Fcmp.Olt
                  | LLFEQ -> L.Fcmp.Oeq
                  | LLFGT -> L.Fcmp.Ogt
                  | LLFLE -> L.Fcmp.Ole
                  | LLFGE-> L.Fcmp.Oge
                and
                  iopfinder = function
                  | LLAdd -> L.build_add
                  | LLSub -> L.build_sub
                  | LLMul -> L.build_mul
                  | LLDiv -> L.build_sdiv
                  | LLMod -> L.build_srem
                  | LLAnd -> L.build_and
                  | LLOr -> L.build_or
                and
                  fopfinder = function
                  | LLFAdd -> L.build_fadd
                  | LLFSub -> L.build_fsub
                  | LLFMul -> L.build_fmul
                  | LLFDiv -> L.build_fdiv
              in
              let regbinop = (match optyp with
                                LLIop iop -> (iopfinder iop) reg1 reg2 "iopinst" block_builder
                              | LLFop fop -> (fopfinder fop) reg1 reg2 "fopinst" block_builder
                              | LLIBop ibop -> L.build_icmp (icmpfinder ibop) reg1 reg2 "ibopinst" block_builder
                              | LLFBop fbop -> L.build_fcmp (fcmpfinder fbop) reg1 reg2 "icmpinst" block_builder
                             ) in
              store_reg labelresult regbinop block_builder
           | LLBuildArrayLoad (agglabel,indexlabel,destlabel) ->
              let elementptr = get_arr_ptr_by_labels agglabel indexlabel block_builder in
              let derefelement = L.build_load elementptr "arrload_deref" block_builder in
              store_reg destlabel derefelement block_builder
           | LLBuildArrayStore (agglabel, indexlabel, fromlabel) ->
              let elementptr = get_arr_ptr_by_labels agglabel indexlabel block_builder in
              let val_to_store = load_reg fromlabel block_builder in
              L.build_store val_to_store elementptr block_builder
           | LLBuildStructLoad (agglabel,index,destlabel) ->
              let elementptr = get_struct_ptr_by_labels agglabel index block_builder in
              let derefelement = L.build_load elementptr "structload_deref" block_builder in
              store_reg destlabel derefelement block_builder
           | LLBuildStructStore (agglabel, index, fromlabel) ->
              let elementptr = get_struct_ptr_by_labels agglabel index block_builder in
              let val_to_store = load_reg fromlabel block_builder in
              L.build_store val_to_store elementptr block_builder
           | LLBuildTerm terminator -> build_terminator block_builder terminator
           | LLBuildAssign (tolabel, fromlabel) ->
              let val_to_assign = load_reg fromlabel block_builder in
              store_reg tolabel val_to_assign block_builder
           | LLBuildNoOp -> L.const_int i32_t 0
          )
           in
        StringMap.add ("stmt"^func.llfname) stmt_llvalue map
      in
      let bundled_stmts = bind_block_to_stmt func.llfbody "entry" in
      ignore(List.fold_left build_stmt StringMap.empty bundled_stmts); (*build the entry block*)
      (* now let's build each block *)
      let build_blocks =
        let build_block block =
          let bname = block.llbname in
          let bundled_stmts = bind_block_to_stmt block.llbbody bname in
          ignore(List.fold_left build_stmt StringMap.empty bundled_stmts);
          ()
        in
        List.iter build_block func.llfblocks
      in
      let _ = build_blocks in
      () (* end of build_funcs *)
    in
    List.iter build_func funcs in

  (*let _ = Llvm_analysis.assert_valid_module the_module in*)
  the_module
