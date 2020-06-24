open Sast
open Cast
open Llast

module StringMap = Map.Make(String)

let cast_to_llast cast =
  let rec ctyp_to_lltyp = function
      SInt -> LLInt
    | SFloat -> LLDouble
    | SBool -> LLBool
    | SArray (styp, int) -> LLArray (ctyp_to_lltyp styp, int)
    | SStruct (name, _) -> LLStruct name
    | SVoid -> LLInt
    | SString -> LLConstString
    | SPtr -> assert false;
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
  let then_decl i = "then" ^ (string_of_int i) and
      else_decl i = "else" ^ (string_of_int i) and
      merge_decl i = "merge" ^ (string_of_int i) in

  let for_cond i = "for_cond" ^ (string_of_int i) and
      for_body i = "for_body" ^ (string_of_int i) and
      for_merge i = "for_merge" ^ (string_of_int i) in
  
  let dud = LLRegLit (LLInt,(LLLitInt 42)) in

  let add_inst_to_branch inst bname map =
    if(StringMap.mem bname map)
    then(let branch_inst_list = StringMap.find bname map in
         StringMap.add bname (inst::branch_inst_list) map)
    else(StringMap.add bname (inst::[]) map);
  in

  let rec walk_cstmt (a_stack,c_stack,t_stack,cnt,head,blabels,llinsts) = function
    | CExpr(t, e) -> (assert (StringMap.mem "entry" llinsts)); let ret = walk_cexpr a_stack c_stack t_stack cnt head blabels llinsts e in ret
    | CPushAnon(t, s) -> 
       let v = a_decl cnt in
       let vreg = LLRegLabel (ctyp_to_lltyp t, v) in
       walk_cstmt ((v::a_stack),c_stack,t_stack,(cnt + 1),(vreg::head),blabels,llinsts) s

    | CReturn opt -> (match opt with
                        Some (typ, CPushAnon(t, cstmt)) when t=typ ->
                        let v = a_decl cnt in
                        let vreg = LLRegLabel (ctyp_to_lltyp typ, v) in
                        let (cnt, head, r1, blabels, llinsts) =
                          walk_cstmt (
                              (v::a_stack),
                              c_stack,
                              t_stack,
                              (cnt + 1),
                              (vreg::head),
                              blabels,
                              llinsts) cstmt
                        in
                        let llinst = LLBuildTerm (LLBlockReturn vreg) in
                        let llinsts = add_inst_to_branch llinst (List.hd blabels) llinsts in
                        (cnt, head, vreg, blabels, llinsts)
                      | Some _ -> assert false
                      | None ->
                         let llinst = LLBuildTerm LLBlockReturnVoid and
                             llreg = LLRegLit (LLInt, (LLLitInt 77711)) in
                         let llinsts = add_inst_to_branch llinst (List.hd blabels) llinsts in
                         (cnt, head, llreg, blabels, llinsts)
                     )

    | CCond (t,sif,sthen,selse) ->
       (
       match (sif, sthen, selse) with
         | (CPushAnon(ti, ifstmt), CPushAnon(tt, thenstmt), CPushAnon(te, elsestmt)) ->
            let v = a_decl cnt in
            let vreg = LLRegLabel (ctyp_to_lltyp ti, v) in
            let (cnt, head, rif, blabels,llinsts) =
              walk_cstmt ((v::a_stack), c_stack, t_stack, (cnt + 1), vreg::head, blabels,llinsts) ifstmt in
            let thenname = then_decl cnt
            and elsename = else_decl cnt and mergename = merge_decl cnt in
            let llinstbr = LLBuildTerm (LLBlockBr (vreg, thenname, elsename)) in
            let llinsts = add_inst_to_branch llinstbr (List.hd blabels) llinsts in
            
            let v = a_decl cnt in
            let vreg = LLRegLabel (ctyp_to_lltyp tt, v) in
            let (cnt, head, _, blabels,llinsts) =
              walk_cstmt (v::a_stack,c_stack, t_stack, (cnt + 1), vreg::head,
                          thenname::blabels, llinsts) thenstmt in
            let llthenjmp = LLBuildTerm (LLBlockJmp mergename) in
            let llinsts = add_inst_to_branch llthenjmp (List.hd blabels) llinsts in
            
            let v = a_decl cnt in
            let vreg = LLRegLabel (ctyp_to_lltyp te, v) in
            let (cnt, head, _, blabels, llinsts) =
              walk_cstmt (v::a_stack, c_stack,t_stack, (cnt + 1), vreg::head,
                          elsename::blabels, llinsts) elsestmt in
            let llelsejmp = LLBuildTerm (LLBlockJmp mergename) in
            let llinsts = add_inst_to_branch llelsejmp (List.hd blabels) llinsts in
            (cnt, head, vreg, mergename::blabels, llinsts)
         | _ -> assert false
       )
    | CLoop (cmpstmt, bodystmt) ->
       let v = c_decl cnt in
       let vreg = LLRegLabel (LLInt, v) in
       let (cnt, head, cmpreg, blabels, llinsts) =
         walk_cstmt (a_stack, v::c_stack, t_stack, (cnt+1), vreg::head, blabels, llinsts) cmpstmt in
       let condname = for_cond cnt and bodyname = for_body cnt and mergename = for_merge cnt in
       let inst_init_entry = LLBuildAssign (vreg,(LLRegLit (LLInt, LLLitInt 0))) 
       and jmp_entry = LLBuildTerm (LLBlockJmp condname) in
       let llinsts = add_inst_to_branch inst_init_entry (List.hd blabels) llinsts in
       let llinsts = add_inst_to_branch jmp_entry (List.hd blabels) llinsts in
       let result_of_binop_reg = LLRegLabel (LLInt, (v^"result")) in
       let eval_forcond = LLBuildBinOp (LLIBop LLLT, vreg, cmpreg, result_of_binop_reg) in
       let branch_forcond = LLBuildTerm (LLBlockBr (result_of_binop_reg, bodyname, mergename)) in
       let blabels = condname::blabels in
       let llinsts = add_inst_to_branch eval_forcond (List.hd blabels) llinsts in
       let llinsts = add_inst_to_branch branch_forcond (List.hd blabels) llinsts in
       let (cnt, head, _, blabels, llinsts) =
         walk_cstmt (a_stack, c_stack, t_stack, cnt, head, bodyname::blabels, llinsts) bodystmt in
       let jmp_forbody = LLBuildTerm (LLBlockJmp condname) in
       let llinsts = add_inst_to_branch jmp_forbody (List.hd blabels) llinsts in
       (cnt, head, dud, mergename::blabels, llinsts)
    | CBlock stmt_list ->
       let fold_block (cnt, head, llreg, blabels, llinsts) stmt  =
         walk_cstmt (a_stack, c_stack, t_stack, cnt, head, blabels, llinsts) stmt in
       let ret = List.fold_left fold_block (cnt, head, dud, blabels, llinsts) stmt_list
       in ret
    | _ -> assert false

  and walk_cexpr a_stack c_stack t_stack cnt head blabels llinsts= function
      CLit (typ, lit) -> (cnt, head, LLRegLit (ctyp_to_lltyp typ,(translate_clit lit)), blabels,llinsts)
    | CId (typ, str) -> (cnt, head, LLRegLabel (ctyp_to_lltyp typ,str), blabels, llinsts)
    | CBinop (typx, expr1, op, expr2) ->
       let (cnt,head,e1, blabels,llinsts) = walk_cexpr a_stack c_stack t_stack cnt head blabels llinsts expr1 in
       let (cnt,head,e2, blabels,llinsts) = walk_cexpr a_stack c_stack t_stack cnt head blabels llinsts expr2 in
       let v = t_decl cnt in
       let vreg = LLRegLabel (ctyp_to_lltyp typx, v) in
       let (cnt, head, t_stack) = (cnt + 1, vreg::head, vreg::t_stack) in
       let llinst =
         (match op with
          | CBinopInt sb -> (match sb with
                                  SAddi -> LLBuildBinOp (LLIop LLAdd, e1, e2, vreg)
                                | SSubi -> LLBuildBinOp (LLIop LLSub, e1, e2, vreg)
                                | SMuli -> LLBuildBinOp (LLIop LLMul, e1, e2, vreg)
                                | SDivi -> LLBuildBinOp (LLIop LLDiv, e1, e2, vreg)
                                | SMod -> LLBuildBinOp (LLIop LLMod, e1, e2, vreg)
                                | SEqi -> LLBuildBinOp (LLIBop LLEQ, e1, e2, vreg)
                                | SLti -> LLBuildBinOp (LLIBop LLLT, e1, e2, vreg)
                                | SLeqi -> LLBuildBinOp (LLIBop LLLE, e1, e2, vreg)
                                | SGti -> LLBuildBinOp (LLIBop LLGT, e1, e2, vreg)
                                | SGeqi -> LLBuildBinOp (LLIBop LLGE, e1, e2, vreg)
                                | _ -> assert false
                               )
          | CBinopFloat sb -> (match sb with
                                 SAddf -> LLBuildBinOp (LLIop LLAdd, e1, e2, vreg)
                               | SSubf -> LLBuildBinOp (LLIop LLSub, e1, e2, vreg)
                               | SMulf -> LLBuildBinOp (LLIop LLMul, e1, e2, vreg)
                               | SDivf -> LLBuildBinOp (LLIop LLDiv, e1, e2, vreg)
                               | SEqf -> LLBuildBinOp (LLIBop LLEQ, e1, e2, vreg)
                               | SLtf -> LLBuildBinOp (LLIBop LLLT, e1, e2, vreg)
                               | SLeqf -> LLBuildBinOp (LLIBop LLLE, e1, e2, vreg)
                               | SGtf -> LLBuildBinOp (LLIBop LLGT, e1, e2, vreg)
                               | SGeqf -> LLBuildBinOp (LLIBop LLGE, e1, e2, vreg)
                               | _ -> assert false
                              )
          | CBinopBool sb -> (match sb with
                              | SLogAnd -> LLBuildBinOp (LLIop LLAnd, e1, e2, vreg)
                              | SLogOr -> LLBuildBinOp (LLIop LLOr, e1, e2, vreg)
                             )
          | CBinopPtr ptr -> assert false
          | CBinopDud -> assert false
         ) in
       let llinsts = add_inst_to_branch llinst (List.hd blabels) llinsts in
       (cnt, head, vreg, blabels, llinsts)
    | CAssign (typ, expr1, expr2) ->
       let (cnt,head,e1, blabels, llinsts) = walk_cexpr a_stack c_stack t_stack cnt head blabels llinsts expr1 in
       
       let (cnt,head,e2, blabels,llinsts) = walk_cexpr a_stack c_stack t_stack cnt head blabels llinsts expr2 in
       (*       ignore(List.iter prerr_string (pretty_print_regs [e1;e2])); *)
       (*
       let zeroint = LLRegLit(LLInt, (LLLitInt 0))  (* TODO need to store both int and doubles *)
       and zerofloat = LLRegLit(LLDouble, (LLLitDouble 0.))
       and zerobool = LLRegLit(LLBool, (LLLitBool false)) in
       let llinst = (match typ with
                       SInt -> LLBuildBinOp (LLIop LLAdd, zeroint, e2, e1)
                     | SFloat -> LLBuildBinOp (LLFop LLFAdd, zerofloat, e2, e1)
                     | SBool ->
                        let assert_bool = function
                            LLRegLabel (typ,str) -> if(typ=LLBool) then(true) else(false)
                          | LLRegLit (typ, lit) -> if(typ=LLBool) then(true) else(false)
                          | _ -> assert false
                        in
                        assert ((assert_bool e2)=true);
                        assert ((assert_bool e1)=true);
                        LLBuildBinOp (LLIop LLAdd, zerobool, e2, e1)
                     | _ -> assert false
                    ) in
        *)
       let llinst = LLBuildAssign (e1, e2) in
       let llinsts = add_inst_to_branch llinst (List.hd blabels) llinsts in
       (cnt, head, e1, blabels, llinsts) 
    | CPeekAnon typ ->
       (match typ with
          SVoid -> (cnt, head, dud, blabels, llinsts)
        | _->
       let anon = (match a_stack  with h::t -> h | [] -> assert false) in
       (cnt, head, LLRegLabel (ctyp_to_lltyp typ, anon), blabels, llinsts)
       )
    | CPeek2Anon typ ->
       (match typ with
          SVoid -> (cnt, head, dud, blabels, llinsts)
        | _->
              let anon = (List.nth a_stack 1) in
              (cnt, head, LLRegLabel (ctyp_to_lltyp typ, anon), blabels, llinsts)
       )
    | CPeek3Anon typ ->
       (match typ with
          SVoid -> (cnt, head, dud, blabels, llinsts)
        | _->
              let anon = (List.nth a_stack 2) in
              (cnt, head, LLRegLabel (ctyp_to_lltyp typ, anon), blabels, llinsts)
       )
    | CCall (frettyp, fname, fstmts) ->
       let fold_block (cnt, head, llreg, blabels, llinsts, reglist) stmt  =
         let (cnt, head, llreg, blabels, llinsts) = walk_cstmt (a_stack, c_stack, t_stack, cnt, head, blabels, llinsts) stmt
         in
         (cnt, head, llreg, blabels,llinsts, llreg::reglist)
       in
       let (cnt, head, _, blabels, llinsts, reglist) =
         List.fold_left fold_block (cnt, head, dud, blabels, llinsts,[]) fstmts
       in
       (match frettyp with
        | SVoid ->
           (match fname with
              "print" ->
              let llcallinst = LLBuildPrintCall (List.nth reglist 0) in
              let llinsts = add_inst_to_branch llcallinst (List.hd blabels) llinsts in
              (cnt, head, dud, blabels, llinsts)
            | _ ->
               let llcallinst = LLBuildCall (fname, List.rev reglist, None) in
               let llinsts = add_inst_to_branch llcallinst (List.hd blabels) llinsts in
               (cnt, head, dud, blabels, llinsts)
           )
        | _ ->
          let v = t_decl cnt in
          let vreg = LLRegLabel ((ctyp_to_lltyp frettyp), v) in
          let (cnt, head, t_stack) = (cnt+1, vreg::head, vreg::t_stack) in
          let llcallinst = LLBuildCall ("kn_"^fname, List.rev reglist, Some vreg) in
          let llinsts = add_inst_to_branch llcallinst (List.hd blabels) llinsts in
          (cnt, head, vreg, blabels, llinsts)
       )
    | CLoopCtr ->
       let ctr = (match c_stack  with h::t -> h | [] -> assert false) in
       (cnt,head,LLRegLabel (LLInt,ctr), blabels, llinsts)
    | CExCall (frettyp, fname, fstmts) ->
       let fold_block (cnt, head, llreg, blabels, llinsts, reglist) stmt  =
         let (cnt, head, llreg, blabels, llinsts) = walk_cstmt (a_stack, c_stack, t_stack, cnt, head, blabels, llinsts) stmt
         in
         (cnt, head, llreg, blabels,llinsts, llreg::reglist)
       in
       let (cnt, head, _, blabels, llinsts, reglist) =
         List.fold_left fold_block (cnt, head, dud, blabels, llinsts,[]) fstmts
       in
       (match fname with
              "print" ->
              let llcallinst = LLBuildPrintCall (List.nth reglist 0) in
              let llinsts = add_inst_to_branch llcallinst (List.hd blabels) llinsts in
              (cnt, head, dud, blabels, llinsts)
            | _ ->
               assert false
       )
    | _ -> assert false
  in

  let translate_cdecl list = function
    | CFnDecl cfunc ->
       let fold_block (cnt, head, llreg, blabel, llinsts) stmt  =
         walk_cstmt ([],[] ,[], cnt, head, blabel,llinsts) stmt in
       let initmap = StringMap.add "entry" [] StringMap.empty in
       let blocks = List.fold_left fold_block (0, [], LLRegDud,["entry"], initmap) cfunc.cbody in
       let (_,temps,_,blabels,insts) = blocks in
       let declare_fml_lcl (SBind (ctyp, cstr, _)) =
         LLRegLabel (ctyp_to_lltyp ctyp, cstr) in
       let formals = List.map declare_fml_lcl cfunc.cformals in
       let locals = List.map declare_fml_lcl cfunc.clocals in

       let block_list_gen key insts block_list =
         (match key with
          | "entry" -> block_list
          | str -> {llbname=str;llbbody= (List.rev insts)}::block_list
         ) in
       let block_list = StringMap.fold block_list_gen insts [] in
       {llfname=cfunc.cfname;llfformals=formals;llflocals=(temps@locals);llfbody=(List.rev (StringMap.find "entry" insts));llfreturn=
        if(cfunc.cret_typ = SVoid) then (LLVoid) else (ctyp_to_lltyp cfunc.cret_typ);llfblocks=block_list}::list
    | _ -> list
  in

  let define_structs =
    let unbind (SBind (ctyp, _, _))=
      ctyp_to_lltyp ctyp
    in
    let define_struct list = function
      | CStructDef cstruct ->
         let struct_name = cstruct.ssname and
             field_list = cstruct.ssfields in
         (struct_name, List.map unbind field_list)::list
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
