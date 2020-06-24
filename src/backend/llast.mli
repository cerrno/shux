type lltyp =
  | LLBool (* i1 *)
  | LLInt (* i32 *)
  | LLDouble (* double_type *)
  | LLConstString (* name and content, only used for representing strings, simply i8* *)
  | LLArray of lltyp * int option (* inside formal we need to declare int*; inside local we declare int[len] *)
  | LLStruct of string
  | LLVoid (* only used for declaring function types *)

type lllit =
  | LLLitBool of bool
  | LLLitInt of int
  | LLLitDouble of float
  | LLLitString of string
  | LLLitArray of lllit list
  | LLLitStruct of lllit list

type llreg =
  | LLRegLabel of lltyp * string(* register can store a name and an lltyp value *)
  | LLRegLit of lltyp * lllit
  | LLRegDud

type llops_iop =
  | LLAdd
  | LLSub
  | LLMul
  | LLDiv
  | LLMod
  | LLAnd
  | LLOr

type llops_fop =
  | LLFAdd
  | LLFSub
  | LLFMul
  | LLFDiv

type llops_ibop =
  | LLLT
  | LLEQ
  | LLGT
  | LLLE
  | LLGE

type llops_fbop =
  | LLFLT
  | LLFEQ
  | LLFGT
  | LLFGE
  | LLFLE

type llops_typ =
  | LLIop of llops_iop
  | LLFop of llops_fop
  | LLIBop of llops_ibop
  | LLFBop of llops_fbop

type llblock_term = (* a block must be terminated by either a jump or a return *)
  | LLBlockReturn of llreg
  | LLBlockReturnVoid
  | LLBlockBr of llreg * string * string (* 1st of boolean, 2nd and 3 of label type *)
  | LLBlockJmp of string (* register must be a branch label type *)

type llstmt =
  | LLBuildBinOp of llops_typ * llreg * llreg * llreg
  | LLBuildCall of string * llreg list * llreg option(* 1 storing func def, 2 storing list of formals, 3 storing retval, void functions dont have return val *)
  | LLBuildPrintCall of llreg (* register storing the integer that you want to print *)
  | LLBuildArrayLoad of llreg * llreg * llreg (* 1 has the arr label, 2 has index, 3 has dest label *)
  | LLBuildArrayStore of llreg * llreg * llreg (* 1 has the arr label, 2 has index, 3 has source label *)
  | LLBuildStructLoad of llreg * int * llreg (* 1 has the struct label, 2 has the index, 3 has dest label *)
  | LLBuildStructStore of llreg * int * llreg (* 1 has the struct label, 2 has the index, 3 has source label *)
  | LLBuildTerm of llblock_term
  | LLBuildAssign of llreg * llreg (* assign from the right to the left*)
  | LLBuildNoOp


type llblock = {
    llbname : string;
    llbbody : llstmt list;
}

type llfunc_def = {
    llfname : string;
    llfformals : llreg list;
    llflocals : llreg list;
    llfbody : llstmt list;
    llfreturn : lltyp;
    llfblocks : llblock list;
}

type llglobal = lltyp * string * lllit
type llstruct_def = string * lltyp list

type llprog = llstruct_def list * llglobal list * llfunc_def list

