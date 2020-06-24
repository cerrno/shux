open Core.Std

type typ =
  | Int
  | Float
  | String
  | Bool
  | Struct of string list (* struct identifier *)
  | Array of typ * int option
  | Vector of int (* number of elements in vector *)
  | Ptr
  | Void

type mut =
  | Mutable
  | Immutable

type bind = Bind of mut * typ * string

type fn_typ =
  | Kn
  | Gn

type bin_op =
  | Add | Sub | Mul | Div | Mod | Exp
  | Eq | Lt | Gt | Neq | Leq | Geq
  | LogAnd | LogOr
  | Filter | Map
  | Index
  | For | Do

type un_op =
  | LogNot | Neg | Pos

type lambda = {
  lformals  : bind list;
  lbody     : stmt list;
  lret_expr : expr option;
}

and lit =
  | LitInt of int
  | LitFloat of float
  | LitBool of bool
  | LitStr of string
  | LitKn of lambda
  | LitVector of expr list
  | LitArray of expr list (* include optional type annotation here? *)
  | LitStruct of string list * struct_field list (* should this be more sophisticated? *)

and struct_field = StructField of string * expr

and expr =
  | Lit of lit
  | Id of string list
  | Lookback of string list * int
  | Binop of expr * bin_op * expr
  | Assign of expr * expr
  | Call of string list option * expr list
  | Uniop of un_op * expr
  | LookbackDefault of expr * expr
  | Cond of expr * expr * expr (* technically Ternop *)
  | Access of expr * string

and stmt =
  | VDecl of bind * expr option
  | Expr of expr

type fn_decl = {
  fname     : string;
  fn_typ    : fn_typ;
  ret_typ   : typ option;
  formals   : bind list;
  body      : stmt list;
  ret_expr  : expr option;
}

type struct_def = {
  sname     : string;
  fields    : bind list;
}

type extern_decl = {
  xalias    : string; (* what we call inside shux *)
  xfname    : string; (* what we link to outside shux *)
  xret_typ  : typ option;
  xformals  : bind list;
}

type let_decl =
  | LetDecl of bind * expr
  | StructDef of struct_def
  | ExternDecl of extern_decl

type ns_decl = {
  nname     : string;
  nbody     : program;
}

and program = ns_decl list * let_decl list * fn_decl list
