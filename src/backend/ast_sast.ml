open Ast
open Sast
open Semant

type svar = {
    id : string;
    scope : Sast.sscope;
    svar_type : Sast.styp;
}

(* keep track of names and types *) 
type strans_env = {
    variables : svar list VarMap.t;
    sfn_decl : sfn_decl VarMap.t;
    sstruct_map : sstruct_def VarMap.t
}

let print_styp = function
  | SInt -> "int"
  | SFloat -> "float"
  | SString -> "string"
  | SBool -> "bool"
  | SStruct(b,e) -> "struct"
  | SArray(s,i) -> "array"
  | SPtr -> "ptr"
  | SVoid  -> "void"

let print_bind_name = function
    | SBind(s, name, scope) -> print_string name

let rec to_styp senv = function
    | Some x -> (match x with
    | Int -> SInt
    | Float -> SFloat
    | String -> SString
    | Bool -> SBool
    | Vector(i) -> SArray(SFloat, Some i)
    | Struct(ns) ->  let s = flatten_ns_list ns in
                     let sstruct_binds = (VarMap.find s senv.sstruct_map).ssfields 
									   in SStruct(s, sstruct_binds)
    | Array(t,i) -> let n_styp = to_styp senv (Some t) in  SArray(n_styp, i)
    | Ptr -> SPtr
    | Void -> SVoid)
    | None -> SVoid

(* iorf: true for int, false for float *) 
and to_sbin_op iorf = function
    | Add -> if iorf then SBinopInt SAddi else SBinopFloat SAddf 
    | Sub -> if iorf then SBinopInt SSubi else SBinopFloat SSubf
    | Mul -> if iorf then SBinopInt SMuli else SBinopFloat SMulf
    | Div -> if iorf then SBinopInt SDivi else SBinopFloat SDivf
    | Exp -> if iorf then SBinopInt SExpi else SBinopFloat SExpf
    | Eq  -> if iorf then SBinopInt SEqi else SBinopFloat SEqf
    | Lt  -> if iorf then SBinopInt SLti else SBinopFloat SLtf
    | Gt  -> if iorf then SBinopInt SGti else SBinopFloat SGtf
    | Neq -> if iorf then SBinopInt SNeqi else SBinopFloat SNeqf
    | Leq -> if iorf then SBinopInt SLeqi else SBinopFloat SLeqf
    | Geq -> if iorf then SBinopInt SGeqi else SBinopFloat SGeqf
    | Mod -> SBinopInt SMod
    | LogAnd -> SBinopBool SLogAnd
    | LogOr -> SBinopBool SLogOr
    | Filter -> SBinopFn SFilter
    | Map -> SBinopFn SMap
    | For -> SBinopGn SFor
    | Index -> SBinopPtr SIndex
    | Do -> SBinopGn SFor (* will never be called *) 

and get_styp_from_sexpr = function
    | SLit(s,_) -> s
    | SId(s,_,_) -> s
    | SLookback(s,_,_) -> s
    | SAccess(s,_,_) -> s
    | SBinop(s,_,_,_) -> s
    | SAssign(s,_,_) -> s
    | SKnCall(s,_,_) -> s
    | SGnCall(s,_,_) -> s
    | SLookbackDefault(s,_,_,_) -> s
    | SUnop(s,_,_) -> s
    | SCond(s,_,_,_) -> s
    | _ -> SVoid (* jooooohn *)

and to_slit senv = function
    | LitInt(i) -> SLitInt(i)
    | LitFloat(f) -> SLitFloat(f)
    | LitBool(b) -> SLitBool(b)
    | LitStr(s) -> SLitStr(s)
    | LitKn(l) -> SLitKn(to_slambda senv l)
    | LitVector(el) -> SLitArray(List.map (get_sexpr senv) el)
    | LitArray(e) -> SLitArray(List.map (get_sexpr senv) e)
    | LitStruct(s,e) ->
          let translate_structfield senv = function 
              | StructField(name, expr) ->(name, get_sexpr senv expr)
          in SLitStruct(flatten_ns_list s, List.map (translate_structfield senv) e)

and slit_to_styp senv = function
    | SLitInt(i) -> SInt
    | SLitFloat(f) -> SFloat
    | SLitBool(b) -> SBool
    | SLitStr(s) -> SString
    | SLitKn(l) -> l.slret_typ
    | SLitArray(elist) -> SArray(get_styp_from_sexpr (List.hd elist), Some (List.length elist))
    | SLitStruct(name, slist) -> 
          let sdef = VarMap.find name senv.sstruct_map
          in SStruct(name, sdef.ssfields)

          
and to_sunop iorf = function
    | LogNot -> SLogNot
    | Neg -> if iorf then SNegi else SNegf
    | Pos -> raise (Failure "Pos isnt supposed to exist in SAST") 

and to_sbind env = function
    | Bind(m, t, s) -> SBind(to_styp env (Some t), s, mut_to_scope m)


and get_inherited body decls retexpr =
    let uniq l = 
        let same (t1,s1) (t2,s2) = t1=t2 && s1=s2  
        in let rec exists s b = function
            | [] -> b
            | hd::tl -> if (same hd s) then exists s true tl
                                       else exists s b tl
        in let rec uniq_rec uniques = function
             | [] -> uniques
             | hd::tl -> 
                if (exists hd false uniques) 
                         then uniq_rec uniques tl
                         else uniq_rec (hd::uniques) tl
        in uniq_rec [] l
    in let match_sbind typ name b binding = 
        if b then b else (match binding with 
            | SBind(t, s, scope) -> t=typ && s=name)
    in let rec add_inherit decls inherits = function
        | [] -> inherits
        | (t,s)::tl -> if (List.fold_left (match_sbind t s) false decls)
                       then add_inherit decls inherits tl
                       else let new_bind = SBind(t, s, SLocalVal)
                       in let new_inherits = new_bind::inherits
                       in add_inherit decls new_inherits tl
    in let rec detect_names = function
        | SId(t,s,c) -> [(t,s)]
				| SAccess(t, se, name) -> 
						let expr_names = detect_names se
            in (t,name)::expr_names
        | SBinop(st, se1, bop, se2) -> 
            let names1 = detect_names se1
            and names2 = detect_names se2
            in names1@names2
        | SAssign(st, se1, se2) -> 
            let names1 = detect_names se1
            and names2 = detect_names se2
            in names1@names2
        | SKnCall(st, kname, form_list) -> 
            let exprs = List.map fst form_list
            in let name_list = List.map detect_names exprs
            in List.flatten name_list
        | SGnCall(st, gname, form_list) -> 
            let exprs = List.map fst form_list
            in let name_list = List.map detect_names exprs
            in List.flatten name_list
       | SUnop(t, sun, se) -> detect_names se
       | SCond(t, se1, se2, se3) -> 
            let name_list = List.map detect_names [se1;se2;se3]
            in List.flatten name_list
       | _ -> []
    in let rec get_inherited_rec decls inherits = function
        | [] -> inherits
        | hd::tl -> get_inherited_rec decls ((detect_names (fst hd))::inherits) tl
    in let inherited_body = get_inherited_rec decls [] body
    in let full_body = (detect_names retexpr)::inherited_body
    in let flat_names = List.flatten full_body
    in let uniq_names = uniq flat_names
    in let inherits = add_inherit decls [] uniq_names
		in inherits

and to_slambda senv l = 
    let lformals = translate_fn_formals l.lformals senv
    in let aret = l.lret_expr
    in let rec hoist_lambda (vdecls, exprs) = function
        | [] -> (List.rev vdecls, List.rev exprs)
        | VDecl(b,e)::tl -> (match e with
            | Some e -> let id = get_bind_name b
                        in let asn = Assign(Id([id]), e)
                        in hoist_lambda (VDecl(b, Some e)::vdecls, asn::exprs) tl
            | None -> hoist_lambda (VDecl(b,e)::vdecls, exprs) tl)
        | Expr(e)::tl -> hoist_lambda (vdecls, e::exprs) tl
    in let vdecl_to_local senv = function
        | VDecl(b,e) -> to_sbind senv b
        | Expr(e) -> raise (Failure "Lambda hoisting failed")
    in let (decls, abody) = hoist_lambda ([],[]) l.lbody
    in let llocals = List.map (vdecl_to_local senv) decls
    in let l_env = List.fold_left place_formals senv [llocals;lformals]
    in let body_intermediate = List.map (get_sexpr l_env) abody 
    in let lbody = List.map (fun x -> (x, get_styp_from_sexpr x)) body_intermediate
    in (match aret with
        | Some x -> let sret_expr = get_sexpr l_env x
                    in let inherited = get_inherited lbody (llocals@lformals) sret_expr
                    in let sret_typ = get_styp_from_sexpr sret_expr
                    in { slret_typ = sret_typ; slformals = lformals; 
                         sllocals = llocals; slinherit = inherited;
                         slbody = lbody; slret_expr = Some (sret_expr, sret_typ) }
        | None -> let inherited = get_inherited lbody (llocals@lformals) SExprDud
                  in
                  { slret_typ = SVoid; slformals = lformals; 
                    sllocals = llocals; slinherit = inherited;
                    slbody = lbody; slret_expr = None; })

and mut_to_scope = function 
    | Mutable -> SLocalVar
    | Immutable -> SLocalVal

and get_sfn_name = function
    | SGnDecl(g) -> g.sgname
    | SKnDecl(k) -> k.skname
    | SExDud(e) -> e.skname

and translate_struct_defs env struct_def = 
    {ssname  = struct_def.sname; ssfields = List.map (to_sbind env) struct_def.fields }

and get_struct_bind name = function
    | [] -> assert(false)
    | SBind(t,n,scope)::tl -> if name=n then SBind(t,n,scope)
                               else get_struct_bind name tl

(* translate expr -> sexpr. uses senv for bookkeeping *)
and get_sexpr senv = function
    | Lit(a) -> let sliteral = to_slit senv a 
                in SLit(slit_to_styp senv sliteral, sliteral)
    | Id(ns) -> let s = flatten_ns_list ns
                in if VarMap.mem s senv.variables then
                let v = List.hd (VarMap.find s senv.variables)
			            in SId(v.svar_type, s, v.scope)
                else if VarMap.mem s senv.sfn_decl
                 then SId(SPtr, s, SLocalVal)
                else assert(false)
    | Lookback(nstr, i) -> let str = flatten_ns_list nstr
                           in let var = List.hd (VarMap.find str senv.variables)
                           in let typ = var.svar_type
          in SLookback(typ, str, i)
    | Binop(e1, bin_op, e2) -> 
        let st1 = get_sexpr senv e1 in (match bin_op with
            | Add | Sub | Mul | Div | Mod | Exp -> 
		            let sbinop = (match get_styp_from_sexpr st1 with
					          | SInt -> to_sbin_op true bin_op
					          | SFloat -> to_sbin_op false bin_op
					          | _ -> raise (Failure "Not Integer/Float type on binop")) in 
		                SBinop(get_styp_from_sexpr st1, st1, sbinop, get_sexpr senv e2)
             | Eq | Lt | Gt | Neq | Leq | Geq ->
                let sbinop = (match get_styp_from_sexpr st1 with
					          | SInt -> to_sbin_op true bin_op
					          | SFloat -> to_sbin_op false bin_op
					          | _ -> raise (Failure "Not Integer/Float type on binop")) in
                   SBinop(SBool, st1, sbinop, get_sexpr senv e2)
             | LogAnd | LogOr -> SBinop(SBool, st1, to_sbin_op true bin_op, get_sexpr senv e2)
             | Filter -> let expr2 = (match e2 with
                           | Id(nn) -> let n = flatten_ns_list nn 
                                       in SId(SBool, n, SKnLambda([]))
                           | _ -> get_sexpr senv e2)
                   in SBinop(SArray(get_styp_from_sexpr st1, None), st1, SBinopFn SFilter, expr2)
             | Map ->  let expr2 = (match e2 with
                           | Id(nn) -> let n = flatten_ns_list nn
                                        in let kn = VarMap.find n senv.sfn_decl
                                        in let kn_typ = (match kn with 
                                            | SGnDecl(sgn) -> assert(false)
                                            | SExDud(s) -> assert(false)
                                            | SKnDecl(skn) -> skn.skret_typ)
                                        in SId(kn_typ, n, SKnLambda([]))
                           | _ -> get_sexpr senv e2)
                            in SBinop(SArray(get_styp_from_sexpr expr2, None),
											        st1, SBinopFn SMap, expr2)
             | Index -> let st1type = get_styp_from_sexpr st1 in 
                  (match st1type with
                 | SArray(s, i) -> SBinop(s, st1, SBinopPtr SIndex, get_sexpr senv e2)
                 | _ -> raise (Failure "Indexing needs sarray"))
             | For -> SBinop(SArray(get_styp_from_sexpr (get_sexpr senv e2), None), st1, SBinopGn SFor, get_sexpr senv e2)
             | Do -> let st2 = get_sexpr senv e2
                     in let st2_typ = get_styp_from_sexpr st2
                     in let st1_typ = get_styp_from_sexpr st1
                     in if st1_typ != SInt then assert(false) else (* needs to be an integer. semant should already have handled this *)
                     let fake_for = SBinop(SArray(st2_typ, None), st1, SBinopGn SFor, st2)
                     in let index = SBinop(st1_typ,st1, SBinopInt SSubi, SLit(SInt, SLitInt(-1)))
                     in SBinop(st2_typ, fake_for, SBinopPtr SIndex, index))
             | Assign(e1, e2) -> let st1 = get_sexpr senv e1 in 
								SAssign(get_styp_from_sexpr st1, st1, get_sexpr senv e2)
             | Call(s, elist) -> (match s with
                 | Some ns -> let s = flatten_ns_list ns
                              in let sexpr_list = List.map (get_sexpr senv) elist
					 	                  in let call_formals = List.map (fun x -> (x, get_styp_from_sexpr x)) sexpr_list
		                          in let f = VarMap.find s senv.sfn_decl in (match f with 
		                 | SGnDecl(gn) -> SGnCall(gn.sgret_typ, s, call_formals)
		                 | SKnDecl(kn) -> SKnCall(kn.skret_typ, s, call_formals)
                     | SExDud(en) -> SExCall(en.skret_typ, s, call_formals))
                 | None -> SGnCall(SInt, "_", []))
             | Uniop(u, e) -> (match u with
                 | LogNot -> let st1 = get_sexpr senv e in 
                         SUnop(get_styp_from_sexpr st1, to_sunop true u, st1)
                 | Neg -> let st1 = get_sexpr senv e in 
				              let sunop = (match get_styp_from_sexpr st1 with
						         | SInt -> to_sunop true u
						         | SFloat -> to_sunop false u
						         | _ -> raise (Failure "Not Integer/Float type on unnop"))
				             in SUnop(get_styp_from_sexpr st1, sunop, st1)
                 | Pos -> get_sexpr senv e)
            | LookbackDefault(e1, e2) -> 
						    let se1 = get_sexpr senv e1
						    in let se2 = get_sexpr senv e2
						    and i = (match e1 with
                   | Lookback(str, i) -> i
                   | _ -> raise (Failure "LookbackDefault not preceded by Lookback expression"))
               in SLookbackDefault(get_styp_from_sexpr se1, i, se1, se2)
            | Cond(e1, e2, e3) -> 
						   let se1 = get_sexpr senv e1
						   and se2 = get_sexpr senv e2
					     and se3 = get_sexpr senv e3
						   in SCond(get_styp_from_sexpr se2, se1, se2, se3)
            | Access(e,  str) -> let sex = get_sexpr senv e
                                 in let styp = get_styp_from_sexpr sex
                                 in let sbinds = (match styp with
                                     | SStruct(s, sbind) -> sbind
                                     | _ -> assert(false))
                                 in let bind = get_struct_bind str sbinds
                                 in let t  = (match bind with
                                     | SBind(t, s, scope) -> t)
								               in SAccess(t, sex , str)

(* this translates letdecls and also builds an environment for further translation *) 
and translate_letdecl senv globals = 
    let global_mapper (sglobals, senv) = function
       | LetDecl(b,e) -> 
						let name = get_bind_name b
						and st = to_styp senv (Some (get_bind_typ b))
						in let new_bind = SBind(st, name, SGlobal)
						in let new_var = { id = name; scope = SGlobal;
															 svar_type = st }
						in let new_varmap = 
								if VarMap.mem name senv.variables
										then let varlist = VarMap.find name senv.variables
												 in VarMap.add name (new_var::varlist) senv.variables
										else VarMap.add name [new_var] senv.variables
						in let new_env = { variables = new_varmap; sfn_decl = senv.sfn_decl; 
															 sstruct_map = senv.sstruct_map } 
						in (SLetDecl(new_bind, get_sexpr senv e)::sglobals, new_env)
       | StructDef(s) -> 
						let to_struct_binds senv b = 
										let name = get_bind_name b
										and t = get_bind_typ b
										in SBind(to_styp senv (Some t), name, SStructField)
						in let sdef  = {ssname = s.sname; ssfields = List.map (to_struct_binds senv) s.fields}
						in let new_structs = VarMap.add s.sname sdef senv.sstruct_map
						in let new_env = { variables = senv.variables; sfn_decl = senv.sfn_decl; 
															 sstruct_map = new_structs }
						in ((SStructDef sdef)::sglobals, new_env) 
       | ExternDecl(e) -> 
						let sret_type = to_styp senv e.xret_typ
						and new_formals = List.map (to_sbind senv) e.xformals (* they are all immutables *)
						in let new_extern = { sxalias = e.xalias; 
																	sxfname = e.xfname;
																	sxret_typ = sret_type;
																	sxformals = new_formals; }

						in let new_func = { skname = e.xalias; skret_typ = sret_type;
																skformals = new_formals; sklocals = [];
																skbody = []; skret_expr = None } 
						in let new_fnmap = VarMap.add new_func.skname (SExDud new_func) senv.sfn_decl
						in let new_env = { variables = senv.variables; sfn_decl = new_fnmap; 
															 sstruct_map = senv.sstruct_map }
						in ((SExternDecl new_extern)::sglobals, new_env)
    in let (sglob, new_env) = List.fold_left global_mapper ([], senv) globals in (List.rev sglob, new_env)

(* used in kn and fn translations *) 
and translate_fn_formals formals senv = 
   List.map (fun (Bind(m,t,s)) -> SBind(to_styp senv (Some t), s, SLocalVal)) formals

(* pushing formal arguments to senv *)
and push_formal svar senv = 
    let new_variables = 
       if VarMap.mem svar.id senv.variables then
          let old_varlist = VarMap.find svar.id senv.variables
          in VarMap.add svar.id (svar::old_varlist) senv.variables
       else
          VarMap.add svar.id [svar] senv.variables
    in { variables = new_variables; sfn_decl = senv.sfn_decl; 
         sstruct_map = senv.sstruct_map } 

(* place a list of SBinds into an environment and return new env *)
and place_formals senv formals = 
    let place_formal senv = function
        | SBind(t, name, s) -> 
           let svar = { id = name; scope = s; svar_type = t } 
           in push_formal svar senv
    in List.fold_left place_formal senv formals

(* traverse an expression tree and find the maximum lookback value *)
and lookback_walk exprs = 
    (* get max integer from a list of integers *)
    let get_max l = 
        let rec get_max_helper maximum = function
            | [] -> maximum
            | hd:: tl -> 
                if hd > maximum then get_max_helper hd tl
                                else get_max_helper maximum tl
        in get_max_helper 0 l
    in let rec lookback_rec_walk maxi = function
        | Lit(l) -> maxi
        | Id(sl) -> maxi
        | Lookback (sl, i) -> if i > maxi then i else maxi
        | Binop(e1, bin_op, e2) -> 
             let i1 = lookback_rec_walk maxi e1
             in let i2 = lookback_rec_walk maxi e2
             in get_max [i1;i2;maxi]
        | Assign(e1, e2) -> 
             let i1 = lookback_rec_walk maxi e1
             in let i2 = lookback_rec_walk maxi e2
             in get_max [i1;i2;maxi]
        | Call(str, elist) ->
             let ilist = List.map (lookback_rec_walk maxi) elist
             in get_max ilist
        | Uniop(u,e) -> let i = lookback_rec_walk maxi e
                        in if i > maxi then i else maxi
        | LookbackDefault(e1, e2) -> 
              let i1 = lookback_rec_walk maxi e1
							in let i2 = lookback_rec_walk maxi e2
							in get_max [i1;i2;maxi]
        | Cond(e1,e2,e3) ->
             let i1 = lookback_rec_walk maxi e1
             in let i2 = lookback_rec_walk maxi e2
             in let i3 = lookback_rec_walk maxi e3
             in get_max [i1;i2;i3;maxi]
        | Access(e1, str) -> let i = lookback_rec_walk maxi e1
                             in if i > maxi then i else maxi
     in let ivalues = List.map (lookback_rec_walk 0) exprs
     in get_max ivalues
 
and translate_kn_decl senv kn = 
    let name = kn.fname 
    and ret_typ = to_styp senv kn.ret_typ
    and knformals = translate_fn_formals kn.formals senv
    in let rec hoist_body (vdecls, local_exprs) = function
        | [] -> (List.rev vdecls, List.rev local_exprs)
        | VDecl(b,e)::tl -> (match e with 
		        | Some e -> let id = get_bind_name b
						     		    in let asn = Assign(Id([id]), e)
								        in hoist_body (VDecl(b,Some e)::vdecls, asn::local_exprs) tl
		        | None -> hoist_body (VDecl(b,e)::vdecls, local_exprs) tl)
        | Expr(e)::tl -> hoist_body (vdecls, e::local_exprs) tl
    in let vdecl_to_local senv = function
        | VDecl(b,e) -> to_sbind senv b
        | Expr(e) -> raise (Failure "hoisting failed. vdecl_to_local should only accept VDecl") 
    in let (vdecls, local_exprs) = hoist_body ([],[]) kn.body
    in let klocals = List.map (vdecl_to_local senv) vdecls
    in let kn_env = List.fold_left place_formals senv [knformals;klocals]
    in let body_intermediate = List.map (get_sexpr kn_env) local_exprs
    in let kbody = List.map (fun x -> (x, get_styp_from_sexpr x)) body_intermediate
    in (match kn.ret_expr with
        | Some x -> let kret_expr = (get_sexpr kn_env x, ret_typ)
						        in { skname = name; skret_typ = ret_typ; skformals = knformals;
								         sklocals = klocals; skbody = kbody; skret_expr = Some kret_expr } 
        | None -> { skname = name; skret_typ = ret_typ; skformals = knformals;
				        		sklocals = klocals; skbody = kbody; skret_expr = None })

and translate_gn_decl senv gn = 
    let name = gn.fname
    and ret_typ = to_styp senv gn.ret_typ
    and gnformals = translate_fn_formals gn.formals senv
    in let rec hoist_body (vals, vars, exprs) = function
        | [] -> (List.rev vals, List.rev vars, List.rev exprs)
        | VDecl(b,e)::tl -> let mut = get_bind_mut b in 
            (match e with
              | Some e -> let id = get_bind_name b
                          in let asn = Assign(Id([id]), e)
                          in if mut = Mutable then
                               hoist_body (vals, VDecl(b, Some e)::vars, asn::exprs) tl
                          else hoist_body (VDecl(b, Some e)::vals, vars, asn::exprs) tl
             | None -> if mut = Mutable then
                            hoist_body(vals, VDecl(b, e)::vars, exprs) tl
                       else hoist_body(VDecl(b, e)::vals, vars, exprs) tl)
        | Expr(e)::tl -> hoist_body (vals, vars, e::exprs) tl
    in let vdecl_to_local senv = function
        | VDecl(b,e) -> to_sbind senv b
        | Expr(e) -> raise (Failure "hoisting failed. vdecl_to_local should only accept VDecl") 
    in let (vals, vars, expr) = hoist_body ([], [], []) gn.body
    in let gvals = List.map (vdecl_to_local senv) vals
    in let gvars = List.map (vdecl_to_local senv) vars
    in let gn_env = List.fold_left place_formals senv [gnformals;gvals;gvars]
    in let gmax_iter = (match gn.ret_expr with 
                          | Some x -> lookback_walk (x::expr)
                          | None -> lookback_walk expr)
    in let body_intermediate = List.map (get_sexpr gn_env) expr
    in let gbody = List.map (fun x -> (x, get_styp_from_sexpr x)) body_intermediate
    in (match gn.ret_expr with
       | Some x -> let gret_expr = (get_sexpr gn_env x, ret_typ)
          in { sgname = name; sgret_typ = ret_typ; sgmax_iter = gmax_iter; sgformals = gnformals;
               sglocalvals = gvals; sglocalvars = gvars; sgbody = gbody; 
               sgret_expr = Some gret_expr } 
       | None ->  { sgname = name; sgret_typ = ret_typ; sgmax_iter = gmax_iter; sgformals = gnformals;
               sglocalvals = gvals; sglocalvars = gvars; sgbody = gbody; 
               sgret_expr = None })
           
and translate_fndecls senv sfn_decls = function
    | [] -> List.rev sfn_decls
    | hd::tl -> let translate_decl senv f = 
               (match f.fn_typ with
                   | Kn -> SKnDecl (translate_kn_decl senv f)
                   | Gn -> SGnDecl (translate_gn_decl senv f))
      in let new_fn = translate_decl senv hd
      in let new_fnmap = VarMap.add (get_sfn_name new_fn) new_fn senv.sfn_decl
      in let new_senv = { variables = senv.variables; sfn_decl = new_fnmap;
                          sstruct_map = senv.sstruct_map } 
      in translate_fndecls new_senv (new_fn::sfn_decls) tl

let if_letdecls = function
    | LetDecl(b, e) -> true
    | _ -> false

let empty_senv = { variables = VarMap.empty; sfn_decl = VarMap.empty;
                   sstruct_map = VarMap.empty; }

let translate_to_sast (ns, globals, functions) = 
    let (sglobals, senv) = translate_letdecl empty_senv globals in 
    let sfn_list = translate_fndecls senv [] functions in
    (sglobals, sfn_list)
