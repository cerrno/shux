type styp =
  | SInt
  | SFloat
  | SString
  | SBool
  | SStruct of string
  | SArray of styp * int option
  | SPtr
  | SVoid 

type sscope = (* replacing Mutable vs Immutable *)
  | SLocalVal | SLocalVar | SGlobal

type sbind = SBind of styp * string * sscope

type sbin_op_i =
  | SAddi | SSubi | SMuli | SDivi | SMod | SExpi
  | SEqi | SLti | SNeqi | SLeqi | SGeqi

type sbin_op_f =
  | SAddf | SSubf | SMulf | SDivf | SExpf
  | SEqf | SLtf | SNeqf | SLeqf | SGeqf

type sbin_op_b =
  | SLogAnd | SLogOr

type sbin_op_p =
  | SIndex

type sbin_op_fn =
  | SFilter | SMap
  | SFor | SDo

type sbin_op = 
  | SBinopInt of sbin_op_i
  | SBinopFloat of sbin_op_f
  | SBinopBool of sbin_op_b
  | SBinopPtr of sbin_op_p
  | SBinopFn of sbin_op_fn

type sun_op = 
  | SLogNot | SNegi | SNegf

type slit =
  | SLitInt of int
  | SLitFloat of float
  | SLitBool of bool
  | SLitStr of string
  | SLitKn of slambda
  | SLitArray of sexpr list 
  | SLitStruct of (string * sexpr) list

and sexpr =
  | SLit of styp * slit
  | SId of styp * string * sscope
  | SLookback of styp * string * int
  | SAccess of styp * sexpr * string
  | SBinop of styp * sexpr * sbin_op * sexpr
  | SAssign of styp * sexpr * sexpr
  | SKnCall of styp * string * sexpr list
  | SGnCall of styp * string * sexpr list
  | SLookbackDefault of styp * sexpr * sexpr
  | SUnop of styp * sun_op * sexpr
  | SCond of styp * sexpr * sexpr * sexpr

and slambda = {
  slret_typ   : styp;
  slformals   : sbind list;
  sllocals    : sbind list;         (* no lookback, const-ness not enforced *)
  slbody      : (sexpr * styp) list;
  slret_expr  : (sexpr * styp);
}

type skn_decl = {
  skname      : string;
  skret_typ   : styp;
  skformals   : sbind list;
  sklocals    : sbind list;         (* do not have lookback *)
  skbody      : (sexpr * styp) list;
  skret_expr  : (sexpr * styp);
}

type sgn_decl = {
  sgname      : string;
  sgret_typ   : styp;
  sgmax_iter  : int;
  sgformals   : sbind list;
  sglocalvals : sbind list;
  sglocalvars : sbind list;
  sgbody      : (sexpr * styp) list;
  sgret_expr  : (sexpr * styp);
}

type sfn_decl =
  | SGnDecl of sgn_decl
  | SKnDecl of skn_decl

type sstruct_def = {
  ssname      : string;
  ssfields    : sbind list;
}

type sextern_decl = {
	sxalias			: string;
  sxfname     : string;
  sxret_typ   : styp;
  sxformals   : sbind list;
}

type slet_decl =
  | SLetDecl of sbind * sexpr
  | SStructDef of sstruct_def
  | SExternDecl of sextern_decl

and sprogram = slet_decl list * sfn_decl list 