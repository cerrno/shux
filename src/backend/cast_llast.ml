open Sast
open Cast
open Llast

let pretty_print_regs l =
  let str_typ = function
    | LLBool -> "bool"
    | LLInt -> "int"
    | LLConstString -> "string"
    | LLDouble -> "double"
    | _ -> "other"
  in
  let print_reg = function
      LLRegLabel (typ,str) -> ("Label-"^(str_typ typ)^"-"^str)
    | LLRegLit (typ, lit) -> ("Lit-"^(str_typ typ))
    | LLRegDud -> "Dud"
  in
  List.map print_reg l

let cast_to_llast cast =
  let struct_tbl = Hashtbl.create 42
  in
  let rec ctyp_to_lltyp = function
      SInt -> LLInt
    | SFloat -> LLDouble
    | SBool -> LLBool
    | SArray (styp, int) -> LLArray (ctyp_to_lltyp styp, int)
    | SStruct (name, _) -> LLStruct name
    | _ -> LLInt
  in

  let rec translate_clit = function
      CLitInt i -> LLLitInt i
    | CLitFloat f -> LLLitDouble f
    | CLitBool b -> LLLitBool b
    | CLitStr s -> LLLitString s
    | _ -> LLLitInt 0 (*TODO fix this*)
  in

  let a_decl i = "a" ^ (string_of_int i) in
  let t_decl i = "t" ^ (string_of_int i) in
  let c_decl i = "c" ^ (string_of_int i) in

  let rec walk_cstmt (a_stack,t_stack,c_stack,cnt,head,llinsts) = function
    | CExpr(t, e) -> walk_cexpr a_stack t_stack c_stack cnt head llinsts e
    | CPushAnon(t, s) ->
       let v = a_decl cnt in
       let vreg = LLRegLabel (ctyp_to_lltyp t, v) in
       walk_cstmt ((v::a_stack),t_stack,c_stack,(cnt + 1),(vreg::head),llinsts) s
    | CReturn opt -> (match opt with
                        Some (typ, CPushAnon(t, cstmt)) when t=typ ->
                        let v = a_decl cnt in
                        let vreg = LLRegLabel (ctyp_to_lltyp typ, v) in
                        let (cnt, head, r1, llinsts) =
                          walk_cstmt ((v::a_stack),t_stack,c_stack,(cnt + 1),(vreg::head),llinsts) cstmt
                        in
                        let llinst = LLBuildTerm (LLBlockReturn r1) in
                        (cnt, head, r1, llinst::llinsts)
                      | Some _ -> assert false
                      | None ->
                         let llinst = LLBuildTerm LLBlockReturnVoid and
                             llreg = LLRegLit (LLInt, (LLLitInt 0)) in
                         (cnt, head, llreg, llinst::llinsts)
                     )
    | CCond (t,sl1,sl2,sl3) -> assert false
    | CBlock stmt_list ->
         let fold_block (cnt, head, llreg, llinsts) stmt  =
                walk_cstmt (a_stack, t_stack, c_stack, cnt, head, llinsts) stmt in
       List.fold_left fold_block (cnt, head, LLRegDud, llinsts) stmt_list
    | CLoop(n, stmt) -> assert false
    | CStmtDud -> assert false

  and walk_cexpr a_stack t_stack c_stack cnt head llinsts= function
      CLit (typ, lit) -> (cnt, head, LLRegLit (ctyp_to_lltyp typ,(translate_clit lit)), llinsts)
    | CId (typ, str) -> (cnt, head, LLRegLabel (ctyp_to_lltyp typ,str), llinsts)
    | CLoopCtr ->
       (cnt, head, LLRegLabel(ctyp_to_lltyp SInt, List.nth c_stack 0), llinsts)
    | CPeekAnon typ ->
       (cnt, head, LLRegLabel(ctyp_to_lltyp typ, List.nth a_stack 0), llinsts)
    | CPeek2Anon typ ->
       (cnt, head, LLRegLabel(ctyp_to_lltyp typ, List.nth a_stack 1), llinsts)
    | CPeek3Anon typ ->
       (cnt, head, LLRegLabel(ctyp_to_lltyp typ, List.nth a_stack 2), llinsts)
    | CBinop (typ, expr1, op, expr2) ->
       let (cnt,head,e1, llinsts) = walk_cexpr a_stack t_stack c_stack cnt head llinsts expr1 in
       let (cnt,head,e2, llinsts) = walk_cexpr a_stack t_stack c_stack cnt head llinsts expr2 in
       let v = t_decl cnt in
       let vreg = LLRegLabel (ctyp_to_lltyp typ, v) in
       let (cnt, head, t_stack) = (cnt + 1, vreg::head, vreg::t_stack) in
       let llinst = LLBuildBinOp (LLAdd, e1, e2, vreg) in
       (cnt, head, vreg, llinst::llinsts)
    | CUnop (typ, op, expr) ->
        let (cnt,head,e,llinsts) = walk_cexpr a_stack t_stack c_stack cnt head llinsts expr in
        let v = t_decl cnt in
        let vreg = LLRegLabel (ctyp_to_lltyp typ, v) in
        let (cnt, head, t_stack) = (cnt +1, vreg::head, vreg::t_stack) in
        let llinst = LLBuildNoOp in (* build Unop *)
        (cnt, head, vreg, llinst::llinsts)
    | CAssign (typ, expr1, expr2) ->
       let (cnt,head,e1, llinsts) = walk_cexpr a_stack t_stack c_stack cnt head llinsts expr1 in
       let (cnt,head,e2, llinsts) = walk_cexpr a_stack t_stack c_stack cnt head llinsts expr2 in
       let zerolit = LLRegLit(LLInt, (LLLitInt 0)) in (* TODO need to store both int and doubles *)
       let llinst = LLBuildBinOp (LLAdd, zerolit, e2, e1) in
       (cnt, head, e1, llinst::llinsts)
    | CAccess( typ, expr1, field_id) -> 
       let (cnt,head,vreg, llinsts) = walk_cexpr a_stack t_stack c_stack cnt head llinsts expr1 in
       let index = Hashtbl.find struct_tbl field_id  in
        assert false
    | CCall(typ, id, actuals) -> 
        let folder ((cnt,head,_,llinsts), llregs) stmt =
          let (cnt,head,llreg,llinsts) = 
            walk_cstmt (a_stack, t_stack, c_stack, cnt, head, llinsts) stmt in
          ((cnt, head, LLRegDud, llinsts), llreg::llregs )
        in let ((cnt,head,_,llinsts), llregs) =
          List.fold_left folder ((cnt, head, LLRegDud, llinsts), []) actuals in
      let (call_dest, vreg, cnt, head, t_stack) = match typ with
        | SVoid -> (None, LLRegDud, cnt, head, t_stack)
        | _ -> let v = t_decl cnt in
               let vreg = LLRegLabel (ctyp_to_lltyp typ, v) in
               (Some vreg, vreg, cnt + 1, vreg::head, vreg::t_stack) in
       let llinst = LLBuildCall(id, llregs, call_dest) in
          (cnt, head, vreg, llinst::llinsts)
    | CExCall(typ, id, actuals) -> 
        let folder ((cnt,head,_,llinsts), llregs) stmt =
          let (cnt,head,llreg,llinsts) = 
            walk_cstmt (a_stack, t_stack, c_stack, cnt, head, llinsts) stmt in
          ((cnt, head, LLRegDud, llinsts), llreg::llregs )
        in let ((cnt,head,_,llinsts), llregs) =
          List.fold_left folder ((cnt, head, LLRegDud, llinsts), []) actuals in
      let (call_dest, vreg) = match typ with
        | SVoid -> (None, LLRegDud)
        | _ -> assert false
      in
       let llinst = LLBuildCall(id, llregs, call_dest) in
          (cnt, head, vreg, llinst::llinsts)
    | CExprDud -> assert false
  in

  let translate_cdecl list = function
    | CFnDecl cfunc ->
       let fold_block (cnt, head, llreg, llinsts) stmt  =
         walk_cstmt ([], [], [], cnt, head, llinsts) stmt in
       let blocks = List.fold_left fold_block (0, [], LLRegDud, []) cfunc.cbody in
       let (_,temps,_,insts) = blocks in
       let declare_fml_lcl (SBind (ctyp, cstr, _)) =
         LLRegLabel (ctyp_to_lltyp ctyp, cstr) in
       let formals = List.map declare_fml_lcl cfunc.cformals in
       let locals = List.map declare_fml_lcl cfunc.clocals in
       {llfname=cfunc.cfname; llfformals=formals; llflocals=(temps@locals);
       llfbody=(List.rev insts); llfblocks=[];
       llfreturn= if(cfunc.cret_typ = SVoid) 
                  then (LLVoid) 
                  else (ctyp_to_lltyp cfunc.cret_typ)}::list
    | _ -> list
  in

  let define_structs =
    let define_struct list = function
      | CStructDef cstruct ->
        let folder (acc, n) (SBind(t, id, _)) =
          Hashtbl.add struct_tbl id n;
          (ctyp_to_lltyp t :: acc, n + 1)
        in let (ordered_fields, _) = List.fold_left folder ([], 0) cstruct.ssfields
        in (cstruct.ssname, ordered_fields) :: list 
      | _ -> list
    in
    List.fold_left define_struct [] cast
  in

  let define_globals =
    let define_global list = function
      | CConstDecl (SBind (ctyp, cname,_), clit) ->
         (ctyp_to_lltyp ctyp, cname, translate_clit clit) :: list
      | _ -> list
    in
    List.fold_left define_global [] cast
  in

  let translate_cprogram =
    List.fold_left translate_cdecl [] cast
  in

  (define_structs,define_globals,translate_cprogram) (*struct llglobal func*)
