type ctyp = Sast.styp

type canon_val = ctyp

type cbind = Sast.sbind

type cbin_op =
  | CBinopInt of Sast.sbin_op_i
  | CBinopFloat of Sast.sbin_op_f
  | CBinopBool of Sast.sbin_op_b
  | CBinopPtr of Sast.sbin_op_p
  | CBinopDud

type cun_op = Sast.sun_op

type clit =
  | CLitInt of int
  | CLitFloat of float
  | CLitBool of bool
  | CLitStr of string
  | CLitArray of clit list
  | CLitStruct of (string * clit) list
  | CLitDud

type cexpr =
  | CLit of ctyp * clit
  | CId of ctyp * string
  | CLoopCtr                    (* access the counter inside a CLoop *)
  | CPeekAnon of ctyp           (* access the temp value of a CBlock *)
  | CPeek2Anon of ctyp           (* access the temp value of a CBlock *)
  | CPeek3Anon of ctyp           (* access the temp value of a CBlock *)
  | CBinop of ctyp * cexpr * cbin_op * cexpr
  | CAccess of ctyp * cexpr * string
  | CAssign of ctyp * cexpr * cexpr
  | CCall of ctyp * string * cstmt list
  | CExCall of ctyp * string * cstmt list
  | CUnop of ctyp * cun_op * cexpr
  | CExprDud

and cstmt =
  | CExpr of ctyp * cexpr
  | CCond of ctyp * (* if *) cstmt * (* then *) cstmt * (* else *) cstmt
  | CPushAnon of ctyp * cstmt
(*  ctyp tmp; { /* push tmp to AStack */
 *    cstmt where CPeekAnon := tmp /* peek AStack */
 *  }
 *  /* pop AStack */
 *)
  | CBlock of cstmt list
(* {
 * cstmt
 * cstmt
 * ...
 * }
 *)
  | CLoop of cstmt (* int *) * cstmt
(*  int cond = cexpr;
 *  int ctr;
 *  /* push (ctr, cond) to LStack */
 *  for (ctr = 0; ctr < cexpr; ctr++) {
 *    cstmt where CLoopCtr := ctr /* fst (peek LStack) */
 *  }
 *  /* pop LStack */
 *)
  | CReturn of (ctyp * cstmt) option
(* return cstmt
 *)
  | CStmtDud

type cfn_decl = {
  cfname      : string;
  cret_typ    : ctyp;
  cformals    : cbind list;
  clocals     : cbind list;
  cbody       : cstmt list;
}

type cstruct_def = Sast.sstruct_def

type cdecl =
  | CFnDecl of cfn_decl
  | CStructDef of cstruct_def
  | CConstDecl of cbind * clit
  | CExternDecl of Sast.sextern_decl
  | CDeclDud

type cprogram = cdecl list
