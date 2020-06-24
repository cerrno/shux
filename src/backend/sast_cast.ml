open Sast
open Cast

module StringMap = Map.Make(String)


let string_of_type t =
  let rec str s = function
    | SInt -> s ^ "SInt"
    | SFloat -> s ^ "SFloat"
    | SString -> s ^ "SString"
    | SBool -> s ^ "SBool"
    | SStruct(i, b) -> s ^ "SStruct " ^ i
    | SArray(t, Some n) -> s ^ "SArray[" ^ (string_of_int n )^ "] of " ^ str "" t
    | SArray(t, None) -> s ^ "SArray[] of " ^ str "" t
    | SPtr -> s ^ "SPtr"
    | SVoid -> s ^ "SVoid"

in str "" t

let die = false
let war = false
let bug s = raise (Failure ("[BUG]: " ^ s))
(* let debug s = prerr_string ("[DEBUG]: " ^ s ^ "\n") *)
let debug s = ()
let db s = prerr_string (s ^ "\n")
let warn d s = if war then prerr_string ("[WARN]: " ^ s ^ "\n"); if die then assert false else d

let warn_t d t s = if war then prerr_string ("[WARN]: " ^ s ^ " (" ^ (string_of_type t) ^ ")\n"); if die then assert false else d

let print_type t =
  if war then prerr_string ((string_of_type t) ^ "\n")


let type_check t1 t2 s = (* default t1 *)
  if war then if t1=t2 then t1 else (print_type t1; print_type t2; warn t1 s) else t1

let string_of_binop_int = function
  | SAddi -> "SAddi"
  | SSubi -> "SSubi"
  | SMuli -> "SMuli"
  | SDivi -> "SDivi"
  | SMod -> "SMod"
  | SExpi -> "SExpi"
  | SEqi -> "SEqi"
  | SLti -> "SLti"
  | SGti -> "SGti"
  | SNeqi -> "SNeqi"
  | SLeqi -> "SLeqi"
  | SGeqi -> "SGeqi"

let print_binop o =
  let string_of_binop = function
    | SBinopInt o -> string_of_binop_int o
    | SBinopFloat o -> "float"
    | SBinopBool o -> "bool"
    | SBinopPtr o -> "ptr"
    | SBinopFn o -> "fn"
    | SBinopGn o -> "gn"
  in prerr_string ((string_of_binop o) ^ "\n")

let map_tuple l p =
  let build e = (e, p)
  in List.map build l

let map_opt f = function
  | Some(x) -> Some(f x)
  | None -> None

let styp_of_sexpr = function
  | SLit(t, _) -> t
  | SId(t, _, _) -> t
  | SLookback(t, _, _) -> t
  | SAccess(t, _, _) -> t
  | SBinop(t, _, _, _) -> t
  | SAssign(t, _, _) -> t
  | SKnCall(t, _, _) -> t
  | SGnCall(t, _, _) -> t
  | SExCall(t, _, _) -> t
  | SLookbackDefault(t, _, _, _) -> t
  | SUnop(t, _, _) -> t
  | SCond(t, _, _, _) -> t
  | SLoopCtr -> SInt
  | SPeek2Anon t -> t
  | SExprDud -> bug "somebody is using Sexpr duds"

let sast_to_cast (let_decls, f_decls) =
  let prefix_x s = "extern_" ^ s    (* extern decl *)
  in let prefix_s s = "struct_" ^ s (* struct defn *)
(*   in let prefix_l s = "let_" ^ s    (* let decl *) *)
  in let prefix_kn s = if s="main" then s else "kn_" ^ s    (* kn function *)
  in let prefix_lambda s i = "lambda_" ^ i ^ "_" ^ s
  in let prefix_gn s = "gn_" ^ s    (* gn function *)
  in let prefix_gns s = "gns_" ^ s  (* gn struct *)
  in let prefix_gnx s = "gnx_" ^ s
  in let gnc = prefix_gnx "gnc"     (* gn execution state counter name *)
  in let prefix_ref s = "ref_" ^ s  (* for arrays that return by reference *)
  in let ret_ref = "ret_ref"
  in let gns_hash = Hashtbl.create 42

  in let kn_to_fn kn =
    let walk_stmt (e, t) = 
      let rec walk_anon sexpr styp sanon = (* this will yield a reversed list *)
        let emit t v = (* set sanon register to the value of v *)
(*
          if t=SVoid then CExpr(t, v) else
*)
          CExpr(t, CAssign(t, sanon, v))

        in let push_anon t e last =  
          (* push new sanon of type t onto stack, walk e, then do last *)
          CPushAnon(t, CBlock(List.rev (last :: walk_anon e t (CPeekAnon t))))

        in let push_anon_nop t e =
          (* push new sanon of type t onto stack, walk e *)
          CPushAnon(t, CBlock(List.rev (walk_anon e t (CPeekAnon t))))

        in let rec walk_r acc rtyp rexpr =
          let walk_primitive xxx =
            let lit t l =
              let tr_lit = match l with
                | SLitInt i -> CLitInt i
                | SLitFloat f -> CLitFloat f
                | SLitBool b -> CLitBool b
                | SLitStr s -> CLitStr s
                | _ -> warn_t CLitDud t "encountered collection type literal in walk_primitive"
              in let lit = CLit(t, tr_lit)
              in emit t lit :: acc

            in let id t n =
              let id = CId(t, n)
              in emit t id :: acc

            in let walk_assign t l r =
              let emit_r = CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t))
              in push_anon t r emit_r :: acc

            in let walk_call t i a =
              let emit_arg = CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t))
              in let map_act (e, t) =
                push_anon t e emit_arg
              in let eval_call =
                CCall(t, i, List.map map_act a)
              in emit t eval_call :: acc

            in let walk_sex t i a =
              let map_act (e, t) =
                push_anon t e (CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t)))
              in let eval_call =
                CExCall(t, i, List.map map_act a)
              in emit t eval_call :: acc

            in let walk_unop t o e =
              let acc = walk_r acc t e (* leaves sanon register containing result *)
              in let unop = CUnop(t, o, sanon)
              in emit t unop :: acc

            in let walk_binop t l o r =
              let tr_binop = match o with
                (* same type *)
                | SBinopInt o -> CBinopInt o
                | SBinopFloat o -> CBinopFloat o
                | SBinopBool o -> CBinopBool o
                (* change of type *)
                | SBinopPtr o -> CBinopPtr o
                | _ -> warn CBinopDud "encountered invalid binary operator in walk_primitive"

              in let primitive xxx = (* operators whose temp value don't change type *)
                let eval_binop = CBinop(t, CPeek2Anon t, tr_binop, CPeekAnon t)
                in let emit_res = CExpr(t, CAssign(t, CPeek3Anon t, eval_binop))
                in let eval_r = push_anon (styp_of_sexpr r) r emit_res
                in push_anon (styp_of_sexpr l) l eval_r :: acc

              in let dereference xxx = (* operators whose operands are of Array t and int *)
                let eval =  (* TODO: make sure I understand what the fuck is going on here *)
                  let arr_t = styp_of_sexpr l
                  in let ind_t = type_check (styp_of_sexpr r) SInt 
                    "encountered type mismatch in dereference in walk_primitive"
                  in let eval_deref = CBinop(t, CPeek2Anon arr_t, tr_binop, CPeekAnon ind_t)
                  in let emit_deref = CExpr(t, CAssign(t, CPeek3Anon t, eval_deref))
                  in let eval_r = push_anon ind_t r emit_deref
                  in push_anon arr_t l eval_r
                in eval :: acc

              in match o with
                | SBinopPtr SIndex -> dereference ()
                | SBinopFn _ -> warn acc "encountered functional binop in walk_primitive"
                | SBinopGn _ -> warn acc "encountered generator binop in walk_primitive"
                | _ -> primitive ()

            in let walk_access t e s =
              let st_t = styp_of_sexpr e
              in let eval_access = CAccess(t, CPeekAnon st_t, s)
              in let emit_access = CExpr(t, CAssign(t, CPeek2Anon t, eval_access))
              in let eval_struct = push_anon st_t e emit_access
              in eval_struct :: acc

            in let walk_cond t iff the els =
              let cond_t = type_check (styp_of_sexpr iff) SBool
                "non-boolean conditional expression in walk_primitive"
              in let eval_merge = CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t))
              in let eval_iff = push_anon_nop cond_t iff
              in let eval_the = push_anon t the eval_merge
              in let eval_els = push_anon t els eval_merge
              in let eval_cond = CCond(t, eval_iff, eval_the, eval_els)
              in eval_cond :: acc

            in match rexpr with
              | SLit(t, l) -> lit t l
              | SId(t, n, _) -> id t n (* don't care about scope *)
              | SUnop(t, o, e) -> walk_unop t o e
              | SBinop(t, l, o, r) -> walk_binop t l o r
              | SAccess(t, e, s) -> walk_access t e s
              | SCond(t, iff, the, els) -> walk_cond t iff the els
              | SKnCall(t, i, a) -> walk_call t i a
              | SAssign(t, l, r) -> walk_assign t l r (* requires new nested walk *)
              | SLoopCtr -> emit SInt CLoopCtr :: acc
              | SPeek2Anon t -> emit t (CPeek2Anon t) :: acc
              | SExCall(t, i, a) -> walk_sex t i a

              (* should never be called like this *)
              | SGnCall(_, _, _) -> warn acc "encountered naked generator call in walk_primitive"
              | _ -> warn acc "encountered unexpected catch-all in walk_primitive"

          in let walk_array element_t element_c =
            let deref = CBinopPtr SIndex

            in let lit t l =
              let l = match l with (* unwrap to list of expressions, emit by value *)
                | SLitArray l -> l
                | _ -> warn [] "encountered non-array type literal in walk_array"
              in let at = type_check rtyp t "literal type mismatch in walk_array"
              in let et = match at with
                | SArray(t, _) -> type_check element_t t "literal element type mismatch in walk_array"
                | _ -> warn element_t "non_array type for array type in walk_array"
              in let assign e i =
                let i = CLit(SInt, CLitInt i)
                in let access =
                  CBinop(et, CPeek2Anon at, deref, i)
                in let emit =
                  CExpr(et, CAssign(et, access, CPeekAnon t))
                in push_anon et e emit
              in let for_each (acc, i) e =
                (assign e i :: acc, i + 1)
              in let (eval_lit, _) =
                List.fold_left for_each (acc, 0) l
              in eval_lit

            in let id t n = (* reference *)
              let id = CId(t, n)
              in emit t id :: acc

            in let walk_assign t l r = (* reference *)
              let emit_r = CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t))
              in push_anon t r emit_r :: acc

            in let walk_cond t iff the els = (* reference *)
              let cond_t = type_check (styp_of_sexpr iff) SBool
                "non-boolean conditional expression in walk_array"
              in let eval_merge = CExpr(t, CAssign(t, CPeek2Anon t, CPeekAnon t))
              in let eval_iff = push_anon_nop cond_t iff
              in let eval_the = push_anon t the eval_merge
              in let eval_els = push_anon t els eval_merge
              in let eval_cond = CCond(t, eval_iff, eval_the, eval_els)
              in eval_cond :: acc

            in let walk_access t e s = (* reference *)
              let st_t = styp_of_sexpr e
              in let eval_access = CAccess(t, CPeekAnon st_t, s)
              in let emit_access = CExpr(t, CAssign(t, CPeek2Anon t, eval_access))
              in let eval_struct = push_anon st_t e emit_access
              in eval_struct :: acc

            in let walk_binop t l o r =
              let dereference xxx = (* operators whose operands are of Array t and int *)
                let eval =  (* TODO: make sure I understand what the fuck is going on here *)
                  let arr_t = styp_of_sexpr l
                  in let ind_t = type_check (styp_of_sexpr r) SInt 
                    "encountered non-SInt r-operand in walk_array"
                  in let eval_deref = CBinop(t, CPeek2Anon arr_t, deref, CPeekAnon ind_t)
                  in let emit_deref = CExpr(t, CAssign(t, CPeek3Anon t, eval_deref))
                  in let eval_r = push_anon ind_t r emit_deref
                  in push_anon arr_t l eval_r
                in eval :: acc

              in let generator xxx =
                let gn_call id actuals =
                  let gn_name = prefix_gn id
                  in let gns_name = prefix_gns id
                  in let gns_fields = Hashtbl.find gns_hash gns_name
                  in let gns_typ = SStruct(gns_name, gns_fields)

                  in let init_gns =
                    let set_field (a, at) (SBind(st, id, _)) =
                      let t = type_check at st
                        "encountered generator struct type mismatch in walk_array"
                      in let get_field =
                        CAccess(t, CPeek2Anon gns_typ, id)
                      in let emit_field =
                        CExpr(t, CAssign(t, get_field, CPeekAnon t))
                      in push_anon t a emit_field
                    in let rec init_fields inits gns_fields actuals = 
                      let (f, ft) = match gns_fields with
                        | [] -> assert false
                        | f::ft -> (f, ft)
                      in match actuals with
                        | [] -> inits
                        | a::at -> init_fields (set_field a f :: inits) ft at

                    in init_fields [] gns_fields actuals
                  in let eval_cnt = (* TODO: check what t is equal to here *)
                    let cnt_t = type_check (styp_of_sexpr r) SInt
                      "encountered non-SInt in gnc evaluation in walk_array"
                    in push_anon_nop cnt_t l 
                  in let call_loop =
                    let curr = CBinop(t, CPeek3Anon rtyp, deref, CLoopCtr)
                    in let emit_val =
                      CExpr(t, CAssign(t, curr, CPeekAnon t))
                    in let set_ctr =
                      let ctr = CAccess(SInt, CPeek2Anon gns_typ, gnc)
                      in CExpr(SInt, CAssign(SInt, ctr, CLoopCtr))
                    in let call_gn =
                      push_anon t (SKnCall(t, gn_name, [ (SPeek2Anon gns_typ, gns_typ) ])) emit_val
                    in CLoop(eval_cnt, CBlock [set_ctr; call_gn])
                  in CPushAnon(gns_typ, CBlock(List.rev(call_loop :: init_gns)))
                in match r with
                | SGnCall(gn_t, id, actuals) when gn_t=element_t -> gn_call id actuals :: acc
                | SGnCall(gn_t, id, actuals) -> warn (gn_call id actuals :: acc)
                    "gn call type mismatch in walk_array"
                |  _ -> warn acc "encountered non-SGnCall in right operand of SFor"

              in let map xxx =
                let atl = styp_of_sexpr l
                in let etl = match atl with
                  | SArray(t, Some _) -> t
                  | SArray(t, None) -> warn t "left operand of map is None array type in walk_array"
                  | _ -> warn SVoid "left operand of map is not an array type in walk_array"
                in let atr = t
                in let (etr, kn_i, kn_c) = match r with
                  | SId(t, i, SKnLambda c) -> (t, i, c)
                  | _ -> warn (SVoid, "", []) "right operand of map call incorrect in walk_array"
                in let (etr, cnt) = match atr with 
                  | SArray(t, Some cnt) when etr=t -> (etr, cnt)
                  | _ -> warn_t (etr, 0) atr "map kernel return type mismatch in walk_array"
                in let for_each = 
                  CExpr(SInt, CLit(SInt, CLitInt cnt))
                in let curr = 
                  CBinop(etr, CPeek3Anon atr, deref, CLoopCtr)
                in let emit =
                  CExpr(etr, CAssign(etr, curr, CPeekAnon etr))
                in let closure =
                  List.map (fun (SBind(t, i, s)) -> (SId(t, i, s), t)) kn_c
                in let make_call =
                  SKnCall(etr, kn_i, (SPeek2Anon etl, etl) :: closure)
                in let do_map =
                  push_anon etr make_call emit
                in let map_loop = CLoop(for_each, do_map)
                in push_anon atl l map_loop :: acc

              in let filter xxx =
                (*
                let at = type_check (styp_of_sexpr l) t 
                  "type mismatch of filtered lhs in walk_array"
                in
                *)
                acc

              in match o with
                | SBinopPtr SIndex -> dereference ()
                | SBinopGn SFor -> generator ()
                | SBinopFn SMap -> map ()
                | SBinopFn SFilter -> filter (* our worst nightmare *) ()
                | _ -> warn acc "encountered invalid binop for walk_array"

            in let walk_call t i a =
              let map_act (e, t) =
                push_anon_nop t e
              in let ret_ref =
                CExpr(t, CPeekAnon t) (* just pass it in by reference *)
              in let eval_call =
                CCall(t, i, ret_ref :: List.map map_act a)
              in CExpr(t, eval_call) :: acc (* no need for emit, use side effect *)

            in match rexpr with
              | SLit(t, l) -> lit t l
              | SId(t, n, _) -> id t n
              | SBinop(t, l, o, r) -> walk_binop t l o r
              | SAccess(t, e, s) -> walk_access t e s
              | SCond(t, iff, the, els) -> walk_cond t iff the els
              | SKnCall(t, i, a) -> walk_call t i a
              | SAssign(t, l, r) -> walk_assign t l r
              | SPeek2Anon t -> emit t (CPeek2Anon t) :: acc

              (* no array type unary operators *)
              | SLoopCtr -> warn acc "encountered SLoopCtr in walk_array"
              | SUnop(t, o, e) -> warn acc "encountered unop in walk_array"
              (* should never be called like this *)
              | SGnCall(_, _, _) -> warn acc "encountered naked SGnCall in walk_array"
              | _ -> warn acc "encountered unexpected catch-all expression in walk_array"

          in let walk_struct id members =
            let id t n = (* reference *)
              let id = CId(t, n)
              in emit t id :: acc

            in let lit t l =
              let (id, l) = match l with
                | SLitStruct(id, l) -> (id, l)
                | _ -> warn ("", []) "encountered non-struct type literal in walk_struct"
              in let map_struct acc (f, e) =
                let t = styp_of_sexpr e
                in let access =
                  CAccess(t, CPeek2Anon rtyp, f)
                in let emit =
                  CExpr(t, CAssign(t, access, CPeekAnon t))
                in push_anon t e emit :: acc
              in List.fold_left map_struct acc l

            in let walk_call t i a =
              let map_act (e, t) =
                push_anon_nop t e
              in let ret_ref =
                CExpr(t, CPeekAnon t) (* just pass it in by reference *)
              in let eval_call =
                CCall(t, i, ret_ref :: List.map map_act a)
              in CExpr(t, eval_call) :: acc (* no need for emit, use side effect *)

            in match rexpr with 
              | SLit(t, l) -> lit t l
              | SId(t, n, _) -> id t n
              | SKnCall(t, i, a) -> walk_call t i a
              | SPeek2Anon t -> emit t (CPeek2Anon t) :: acc
              | _ -> warn acc "encountered unexpected catch-all expression in walk_struct"

          in let walk_ptr xxx = match rexpr with
            | SId(t, i, _) -> emit (type_check t rtyp "walk_ptr type mismatch") (CId(t, i)) :: acc
            | _ -> warn acc "encountered non SId for SPtr type"

          in match rtyp with
            | SArray(t, n) -> debug "walk_r on array type"; walk_array t n
            | SStruct(i, b) -> debug "walk_r on struct type"; walk_struct (prefix_s i) b
            | SPtr ->  walk_ptr ()
            | _ -> debug "walk_r on primitive type"; walk_primitive ()

        in let walk_l ltyp lexpr =
          let rec lvalue_tr typ ass anon =
             let primitive_assign xxx =
              let assign_to e = CExpr(typ, CAssign(typ, e, CPeek2Anon typ))
              in let nop e = e
              in let access t f e = CAccess(t, e, f)
              in let index t l = CBinop(t, l, CBinopPtr SIndex, CPeekAnon SInt)
              in let get_index r = walk_r [] SInt r

              in let rec do_assign f = function
                | SId(t, n, _) -> [ assign_to (f(CId(t, n))) ]
                | SPeek2Anon t -> [ assign_to (f(CPeek3Anon t)) ] (* need to look beyond the Push *)
                | SAccess(t, e, i) -> do_assign (access t i) e
                | SBinop(t, l, SBinopPtr SIndex, r) -> do_assign (index t) l @ get_index r
                | _ -> warn [ CStmtDud ] "encountered non-lvalue in lvalue_tr"
              in let stmts = List.map (do_assign nop) ass
              in let stmts = List.map List.rev stmts
              in let stmts = List.flatten stmts
              in CPushAnon(SInt, CBlock stmts) (* push some space for potential index *)

            in let array_assign t n =
              let index t a = CBinop(t, anon, CBinopPtr SIndex, CLoopCtr)
              in let get_anon = index t anon
              in let get_cond = CExpr(SInt, CLit(SInt, (CLitInt n)))
              in let map_ass a =
                 SBinop(t, a, SBinopPtr SIndex, SLoopCtr)
              in let get_index =
                List.map map_ass ass
              in CLoop(get_cond, lvalue_tr t get_index get_anon)

            in let struct_assign id binds =
              let map_binds (SBind(t, n, _)) =
                let map_ass a =
                  SAccess(t, a, n)
                in let access_ass = 
                  List.map map_ass ass
                in let access_anon =
                  CAccess(t, anon, n)
                in (t, access_ass, access_anon)
              in let for_each_field =
                List.map map_binds binds
              in let translate (t, ass, anon) =
                lvalue_tr t ass anon
              in let translate_each_field =
                List.map translate for_each_field
              in CBlock translate_each_field

            in match typ with
              | SArray(t, Some n) -> array_assign t n
(*               | SArray(_, None) -> warn CStmtDud "encountered None-size array type in lvalue_tr" *)
              | SStruct(i, b) -> struct_assign (prefix_s i) b
              | SPtr -> warn CStmtDud "encountered pointer type in lvalue_tr"
              | SVoid -> if ass=[] then CBlock [] else
                warn CStmtDud "encountered assignment to void type in lvalue_tr"
              | _ -> if ass=[] then CBlock[] else primitive_assign ()

          in let rec walk ass = function
            | SAssign(t, l, r) when t=ltyp -> walk (l :: ass) r
            | SAssign(_, _, _) -> warn [] "encountered assignment type mismatch in lvalue_tr"
            | e -> lvalue_tr ltyp ass sanon :: walk_r [] ltyp e (* reversed *)
          in walk [] lexpr

        in walk_l styp sexpr
      in CPushAnon(t, CBlock(List.rev (walk_anon e t (CPeekAnon t)))) (* in order *)

    in let walk_ret = function
      | Some (e, t) -> CReturn (Some (t, (walk_stmt (e, t)))) 
      | None -> CReturn None

    in let fn_decl kn = CFnDecl 
      { cfname = prefix_kn kn.skname; cret_typ = kn.skret_typ;
        cformals = kn.skformals; clocals = kn.sklocals;
        cbody = List.rev (walk_ret kn.skret_expr :: List.rev_map walk_stmt kn.skbody) }

    in let rec hoist_lambdas kn =
      let hoist n { slret_typ; slformals; sllocals; slbody; slret_expr; slinherit } = 
        hoist_lambdas
        { skname = prefix_lambda kn.skname n; skret_typ = slret_typ;
          skformals = slformals @ slinherit; sklocals = sllocals; skbody = slbody; 
          skret_expr = slret_expr }

      in let rec fish acc p = function
        | SLit(_, SLitKn(l)) -> (hoist p l) :: acc
        | SBinop(_, l, _, r) -> fish [] (p ^ "l") l @ fish acc (p ^ "r") r
        | SAssign(_, l, r) -> fish [] (p ^ "l") l @ fish acc (p ^ "r") r
        | SCond(_, i, t, e) -> fish [] (p ^ "i") i @ fish [] (p ^ "t") t @ fish acc (p ^ "e") e
        | SUnop(_, _, e) -> fish acc (p ^ "u") e
        | SKnCall(_, _, a) -> (List.concat (List.map (fish [] (p ^ "k")) (List.map fst a))) @ acc
        | SGnCall(_, _, a) -> (List.concat (List.map (fish [] (p ^ "g")) (List.map fst a))) @ acc
        | SAccess(_, e, _) -> fish acc (p ^ "a") e
        | _ -> acc
      in let rec bait p = function
        | SLit(t, SLitKn(l)) -> SId(t, prefix_lambda kn.skname p, SKnLambda l.slinherit)
        | SBinop(t, l, o, r) -> SBinop(t, bait (p ^ "l") l, o, bait (p ^ "r") r)
        | SAssign(t, l, r) -> SAssign(t, bait (p ^ "l") l, bait (p ^ "r") r)
        | SCond(ty, i, t, e) -> SCond(ty, bait (p ^ "i") i, bait (p ^ "t") t, bait (p ^ "e") e)
        | SUnop(t, o, e) -> SUnop(t, o, bait (p ^ "u") e)
        | SKnCall(t, s, a) -> SKnCall(t, s, List.map (fun (a, t) -> (bait (p ^ "k") a, t)) a)
        | SGnCall(t, s, a) -> SGnCall(t, s, List.map (fun (a, t) -> (bait (p ^ "g") a, t)) a)
        | SAccess(t, e, s) -> SAccess(t, bait (p ^ "a") e, s)
        | s -> s

      in let walk_stmt (lambdas, body, p) (e, t) =
        (fish lambdas p e, ((bait p e), t) :: body, p ^ "x")
      in let (lambdas, body, _) = List.fold_left walk_stmt ([], [], "z") kn.skbody
      in let body = List.rev body
      in List.rev (fn_decl {kn with skbody = body} :: List.concat lambdas)
    in hoist_lambdas kn

  in let walk_kn kn =
    let kn = { kn with skname = kn.skname }

    in let ret_id t = SId(t, ret_ref, SLocalVar)
    in let ret_bind t = SBind(t, ret_ref, SLocalVar)
    in let tr t id s = match s with
      | SLocalVal | SLocalVar -> (t, prefix_ref id, s)
      | _ -> (t, id, s)
    in let walk_binds (SBind(t, id, s)) =
      let (t, id, s) = tr t id s in SBind(t, id, s)
    in let walk_body (e, t) =
      let rec walk = function
        | SId(t, id, s) -> let (t, id, s) = tr t id s in SId(t, id, s)
        | SBinop(t, l, o, r) -> SBinop(t, walk l, o, walk r)
        | SAssign(t, l, r) -> SAssign(t, walk l, walk r)
        | SCond(t, iff, the, els) -> SCond(t, walk iff, walk the, walk els)
        | SUnop(t, o, e) -> SUnop(t, o, walk e)
        | SAccess(t, e, s) -> SAccess(t, walk e, s)
        | SKnCall(t, s, a) -> SKnCall(t, s, List.map (fun (e, t) -> (walk e, t)) a)
        | SGnCall(t, s, a) -> SGnCall(t, s, List.map (fun (e, t) -> (walk e, t)) a)
        | s -> s
      in (walk e, t)
    in let walk_ret ret = 
      let assign_ret (e, t) = (SAssign(t, ret_id t, e), t)
      in match ret with
        | Some(e, t) -> Some(assign_ret(walk_body (e, t)))
        | None -> warn None "encountered None return expr in reference-returning kn in walk_kn"
    in let ref_kn xxx = { kn with skbody = List.map walk_body kn.skbody;
                      skformals = ret_bind kn.skret_typ :: List.map walk_binds kn.skformals;
                      sklocals = List.map walk_binds kn.sklocals;
                      skret_expr = walk_ret kn.skret_expr }
    in let kn = match kn.skret_typ with
      | SArray(_, Some _) -> ref_kn ()
      | SArray(t, None) -> warn kn "encountered kn that returns None sized array type in walk_kn"
      | SStruct(_, _) -> ref_kn ()
      | _ -> kn

      (*
    in let local_bindings = 
      let fold_map acc (SBind(t, i, s)) = StringMap.add i (t, s) acc
      in List.fold_left fold_map StringMap.empty (kn.skformals @ kn.sklocals)
*)
    in let kn_infer xxx = 
      let infer_array (e, t) =
        let co l r =
          let r_none xxx = match r with
            | SArray(t, Some n) -> r
            | SArray(t, None) -> bug "L R array type both None"
            | _ -> assert false
          in let r_some ln = match r with
            | SArray(t, Some n) when ln=n -> l
            | SArray(t, Some n) -> bug "L R array Some size value mismatch"
            | SArray(t, None) -> l
            | _ -> assert false
          in match l with 
            | SArray(t, Some n) -> r_some n
            | SArray(t, None) -> r_none ()
            | _ -> assert false

        in let rec walk_r l_typ = function
          | SLit(t, l) -> let it = co l_typ t in (SLit(it, l), it)
          | SId(t, i, s) -> let it = co l_typ t in (SId(it, i, s), it)
          | SKnCall(t, id, a) -> let it = co l_typ t in (SKnCall(it, id, a), it)
          | SGnCall(t, id, a) -> let it = co l_typ t in (SGnCall(it, id, a), it)
          | SExCall(t, id, a) -> let it = co l_typ t in (SExCall(it, id, a), it)
          | SPeek2Anon t -> let it = co l_typ t in (SPeek2Anon it, it)
(*           | SBinop(t, l, SBinopGn o, r) -> let (l, it) = walk_r l_typ l in (SBinop(it, l, SBinopGn o, r), it) *)
          | SBinop(t, l, SBinopFn o, r) -> let (l, it) = walk_r l_typ l in (SBinop(it, l, SBinopFn o, r), it)
(*           | SBinop(t, l, SBinopPtr o, r) -> let (l, it) = walk_r l_typ l in (SBinop(it, l, SBinopPtr o, r), it) *)
          | e -> let t = co l_typ (styp_of_sexpr e) in (e, t)

        in let coerce l lr_typ =
          let ll_typ = styp_of_sexpr l
          in let (t, rn) = match lr_typ with
            | SArray(t, Some n) -> (t, n)
            | SArray(t, None) -> (t, 0)
            | _ -> assert false
          in let n = match ll_typ with 
            | SArray(t, Some n) -> if n<rn then Some n else if rn=0 then None else Some rn
            | SArray(t, None) -> if rn=0 then None else Some rn
            | _ -> assert false
          in SArray(t, n)
        in let rec walk_l l_typ = function
          | SAssign(t, l, r) -> SAssign(t, l, walk_l (coerce l l_typ) r)
          | e -> let (e, it) = walk_r l_typ e in e
        in match t with 
          | SArray(_) -> (walk_l t e, t)
          | _ -> (e, t)

      in { kn with skbody = List.map infer_array kn.skbody;
            skret_expr = map_opt infer_array kn.skret_expr }
    in kn_to_fn (kn_infer ())

  in let walk_gn gn = 
    let prefix_gnv s = "gnv_" ^ s         (* for local vars *)
    in let gns_arg = prefix_gnx "arg"            (* gn execution state argument name *)

    in let st_fields =
      let a_decl = function
        | SBind(t, n, SLocalVal) -> SBind(SArray(t, Some gn.sgmax_iter), n, SStructField)
        | SBind(t, n, s)-> warn (SBind(t, n, s)) "encountered non-SLocalVal binding in gn formals or local vals in walk_gn"
      in let ctr_decl =
        SBind(SInt, gnc, SLocalVar)
      in ctr_decl :: List.map a_decl (gn.sgformals @ gn.sglocalvals)

    in let gns_typ_name = prefix_gns gn.sgname
    in let gns_typ = SStruct(gns_typ_name, st_fields) (* struct type name *)
    in let defn_cstruct = 
      Hashtbl.add gns_hash gns_typ_name st_fields;
      CStructDef { ssname = gns_typ_name; ssfields = st_fields }

    in let gn_to_kn =
      let st_id = SId(gns_typ, gns_arg, SLocalVar)
      in let st_element t id = SAccess(t, st_id, id)
      in let st_var t = st_element (SArray(t, Some gn.sgmax_iter))
      in let st_cnt = st_element SInt gnc
      in let wrap_int n = SLit(SInt, SLitInt n)

      in let prefix_var = function
        | SBind(t, n, SLocalVar) -> SBind(t, prefix_gnv n, SLocalVar)
        | SBind(t, n, s)-> warn (SBind(t, n, s)) "encountered non-SLocalVar in gn local vars in walk_gn"

      in let lb_st t id n =
        (* should be gnx_arg.id[(gnx_ctr - n) % max_iter] *)
        let idx = SBinop(SInt, st_cnt, SBinopInt SSubi, wrap_int n)
        in let idx = SBinop(SInt, idx, SBinopInt SMod, wrap_int gn.sgmax_iter)
        in SBinop(t, st_var t id, SBinopPtr SIndex, idx)

      in let lb_cmp n = SBinop(SBool, wrap_int n, SBinopInt SLeqi, st_cnt)

      in let rec lookback (e, t) =
        let sid t id = function
          | SGlobal as s -> SId(t, id, s) (* global prefixing will happen in walk_kn *)
          | SKnLambda _ as s -> SId(t, id, s)
          | SLocalVar as s -> SId(t, prefix_gnv id, s)
          | SLocalVal -> lb_st t id 0
          | SStructField as s -> warn (SId(t, id, s)) 
            "encountered SStructField binding scope in walk_gn"
          
        in let rec lb = function
          | SId(t, id, s) -> sid t id s
          | SLookback(t, id, n) -> lb_st t id n
          | SLookbackDefault(t, n, f, e) -> SCond(t, lb_cmp n, lb f, lb e)
          | SAccess(t, e, id) -> SAccess(t, lb e, id)
          | SBinop(t, l, o, r) -> SBinop(t, lb l, o, lb r)
          | SAssign(t, l, r) -> SAssign(t, lb l, lb r)
          | SKnCall(t, id, a) -> SKnCall(t, id, List.map lookback a)
          | SGnCall(t, id, a) -> SGnCall(t, id, List.map lookback a)
          | SUnop(t, o, e) -> SUnop(t, o, lb e)
          | SCond(t, i, f, e) -> SCond(t, lb i, lb f, lb e)
          | e -> e
        in (lb e, t)
      in { skname = prefix_gn gn.sgname; skret_typ = gn.sgret_typ;
            skformals = [ SBind(gns_typ, gns_arg, SLocalVar) ];
            sklocals = List.map prefix_var gn.sglocalvars; 
            skbody = List.map lookback gn.sgbody; 
            skret_expr = map_opt lookback gn.sgret_expr }

    in defn_cstruct :: kn_to_fn gn_to_kn

  in let walk_fns f_decls =
    let rec walk = function
      | [] -> []
      | SGnDecl(g)::t -> let r = walk_gn g in r @ walk t
      | SKnDecl(k)::t -> let r = walk_kn k in r @ walk t
      | SExDud(_)::t -> warn (walk t) "came across SEx booty call juicy"
    in walk f_decls
(*   in let let_map = Hashtbl.create 42 *)
  in let walk_static let_decls =
    (*
    let interp_expr t e =
      let interp_lit = function
        | SLitInt i -> CLitInt i
        | SLitFloat f -> CLitFloat f
        | SLitBool b -> CLitBool b
        | SLitStr s -> CLitStr s
        | SLitArray l -> assert false
        | SLitStruct(id, l) -> assert false
        | _ -> assert false
      in let interp_primitive xxx = match e with
        | SLit(t, l) -> interp_lit l
        | SId(t, i, s) -> CLitDud
        | SBinop(t, l, o, r) -> assert false
        | _ -> assert false
      in let interp_array at n =
        CLitDud
      in let interp_struct id binds =
        CLitDud
      in match t with
        | SArray(t, Some n) -> interp_array t n
        | SArray(t, None) -> warn CLitDud "None size array encountered in let decls"
        | SStruct(i, b) -> interp_struct i b
        | SPtr | SVoid -> warn CLitDud "invalid type encountered in let declarations"
        | _ -> interp_primitive ()
    in let assign_let n t e =
      let let_val = interp_expr t e
      in let _ = Hashtbl.add let_map n let_val
      in CLitDud
      *)
    let walk = function
(*       | SLetDecl(SBind(t, n, s), e) -> CConstDecl(SBind(t, n, s), interp_expr t e) *)
      | SLetDecl(SBind(t, n, s), e) -> CConstDecl(SBind(t, n, s), CLitDud)
      | SStructDef s -> CStructDef {s with ssname = prefix_s s.ssname}
      | SExternDecl x -> CExternDecl {x with sxalias = prefix_x x.sxalias}
    in walk let_decls

  (* function entry point: walk entire program *)
  in let walk_program l f =
    let r = List.map walk_static l in r @ walk_fns f
  in walk_program let_decls f_decls
