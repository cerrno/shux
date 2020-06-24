open Ast

(* Pretty printing *)
let nop x = x

(* option helpers *)
let is_some = function
  | Some _ -> true
  | None -> false

let string_of_opt_default d f = function
  | Some s -> f s
  | None -> d

let string_of_opt f s = string_of_opt_default "" f s

let rec string_of_id = function
  | [] -> ""
  | [s] -> s
  | s::l -> s ^ "->" ^ string_of_id l

let rec _string_of_typ = function
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool" 
  | Ptr -> "_ptr"
  | Struct t -> "struct " ^ string_of_id t 
  | Array(t, i) -> _string_of_typ t ^ "[" ^ (match i with Some(n) -> string_of_int n | None -> "") ^ "]"
  | Vector t -> "vector<" ^ string_of_int t ^ ">"
  | Void -> "__void__" (* should not be used *)

let string_of_typ x = string_of_opt _string_of_typ x

let string_of_fn_typ = function
  | Kn -> "kn"
  | Gn -> "gn"

type op_typ = Infix | Prefix | PostfixPair

let _fix = function
  | Add | Sub | Mul | Div | Mod | Exp 
  | Eq | Lt | Gt | Neq | Leq | Geq | LogAnd | LogOr
  | Filter | Map -> Infix
  | For | Do -> Prefix
  | Index -> PostfixPair

let string_of_binop = function
  | Add -> " + "
  | Sub -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Mod -> " %"
  | Exp  -> " ^ "
  | Eq -> " == "
  | Lt -> " < "
  | Gt -> " > "
  | Neq -> " != "
  | Leq -> " <= "
  | Geq -> " >= "
  | LogAnd -> " && "
  | LogOr -> " || "
  | Filter -> " :: "
  | Map -> " @ "
  | For -> "for "
  | Do -> "do "
  | _ -> "" (* should raise error *)

let string_of_binop_match = function
  | Index -> ("[", "]")
  | _ -> ("", "") (* should raise error *)

let string_of_binop_expr f l o r =
  match _fix o with
  | Infix -> f l ^ string_of_binop o ^ f r
  | Prefix -> string_of_binop o ^ f l ^ f r 
  | PostfixPair -> match string_of_binop_match o with (o, c) -> f l ^ o ^ f r ^ c
let string_of_asn_expr f l r =
  f l ^ " = " ^ f r

let string_of_unop = function
  | LogNot -> "!"
  | Neg -> "-"
  | Pos -> "+"

let string_of_uniop_expr f o e = string_of_unop o ^ f e

let string_of_cond_expr f i t e =
  f i ^ " ? " ^ f t ^ " : " ^ f e

let string_of_lookback_default_expr f l e =
  f l ^ " : " ^ f e

let string_of_mut = function
  | Immutable -> ""
  | Mutable -> "var "

let string_of_bind = function
  | Bind(mut, typ, id) -> string_of_mut mut ^ _string_of_typ typ ^ " " ^ id

let string_of_list f l o s c e =
  match (List.map f l) with
  | [] -> if e then o ^ c else ""
  | l -> o ^ String.concat s l ^ c

let rec string_of_struct_field = function
  | StructField(n, e) -> "\t." ^ n ^ " = " ^ string_of_expr e

and string_of_lambda = function {lformals = fs; lbody = b; lret_expr = re } ->
  string_of_list string_of_bind fs "(" ", " ")" false ^
  string_of_list string_of_stmt b "{\n" "\n\t" "" true ^
  string_of_opt string_of_expr re ^ "\n}"

and string_of_lit = function
  | LitInt(l) -> string_of_int l
  | LitFloat(l) -> string_of_float l
  | LitBool(l) -> string_of_bool l
  | LitStr(l) -> "\"" ^ l ^ "\""
  | LitKn(l) -> string_of_lambda l
  | LitVector(l) -> string_of_list string_of_expr l "<" ", " ">" true
  | LitArray(l) -> string_of_list string_of_expr l "[" ", " "]" true
  | LitStruct(id, l) -> string_of_id id ^ string_of_list string_of_struct_field l "{" ";\n" "}" true

and string_of_expr = function
 | Lit l -> string_of_lit l
 | Id s -> string_of_id s
 | Lookback(s, i) -> string_of_id s ^ ".." ^ string_of_int i
 | Access(e, i) -> string_of_expr e ^ "." ^ i
 | Uniop(o, e) -> string_of_uniop_expr string_of_expr o e
 | Assign(l, r) -> string_of_asn_expr string_of_expr l r
 | Binop(e1, o, e2) -> string_of_binop_expr string_of_expr e1 o e2
 | Call(s, el) -> string_of_opt_default "_" string_of_id s ^ 
                  string_of_list string_of_expr el "(" ", " ")" (is_some s)
 | LookbackDefault(l, e) -> string_of_lookback_default_expr string_of_expr l e
 | Cond(i, t, e) -> string_of_cond_expr string_of_expr i t e

and string_of_vdecl bind expr = 
  string_of_bind bind ^ " " ^ string_of_expr expr

and string_of_stmt = function 
  | VDecl (bind, expr) -> string_of_opt (string_of_vdecl bind) expr ^ ";\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"

let string_of_fdecl = function { fn_typ = fn; fname = id; formals = fs;
                                 ret_typ = rt; body = b; ret_expr = re } ->
  string_of_fn_typ fn ^ " " ^ id ^ string_of_list string_of_bind fs "(" ", " ")" true ^
  string_of_typ rt ^ string_of_list string_of_stmt b "\n{\n" "\n\t" "" true ^
  "\t" ^ string_of_opt string_of_expr re ^ "\n}\n"

let string_of_struct_def = function { sname = n; fields = f } ->
  "struct " ^ n ^ string_of_list string_of_bind f "{\n\t" ";\n\t" "}" true

let string_of_extern = function { xalias = a; xfname = n; xformals = f; xret_typ = r } ->
  "extern " ^ a ^ string_of_list string_of_bind f "(" ", " ")" true ^ string_of_typ r ^ ";"

let string_of_let = function
  | LetDecl(bind, expr) -> string_of_bind bind ^ " " ^ string_of_expr expr ^ ";"
  | StructDef(s) -> string_of_struct_def s 
  | ExternDecl(s) -> string_of_extern s

let rec string_of_ns = function { nname = n; nbody = b} ->
  "ns " ^ n ^ " {\n" ^ string_of_program b ^ "\n}"

and string_of_program (ns_list, let_list, fn_list) =
  String.concat "\n" (List.map string_of_ns ns_list) ^ "\n" ^
  String.concat "\n" (List.map string_of_let let_list) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl fn_list) 
