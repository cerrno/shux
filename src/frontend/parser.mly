%{
  open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK DBL_QUOTE SNG_QUOTE
%token SEMI COMMA DOTDOT DOT PLUS MINUS TIMES DIVIDE MOD EXPONENT
%token ASSIGN ADD_ASN SUB_ASN MUL_ASN DIV_ASN MOD_ASN EXP_ASN
%token LOG_AND LOG_OR LOG_NOT LT GT EQ NEQ LEQ GEQ
%token QUES COLON FILTER MAP ARROW IF THEN ELIF ELSE FOR WHILE DO UNDERSCORE
%token NS GN KN STRUCT LET EXTERN VAR INT_T FLOAT_T STRING_T BOOL_T VECTOR_T PTR_T PASS

%token <bool> BOOL_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <string> ID
%token EOF

%start program

%type <Ast.program> program

%%

program:
  | ns EOF                                  { $1 }

ns:
  | ns_section let_section fn_section       { ($1, $2, $3) }


/* namespace declaration rules */
ns_section:
  | ns_decls                                { List.rev $1 }
  | /* nothing */                           { [] }

ns_decls:
  | ns_decls ns_decl                        { $2::$1 }
  | ns_decl                                 { [$1] }

ns_decl:
  | NS ID ASSIGN LBRACE ns RBRACE           { {nname = $2; nbody = $5} }


/* let declaration rules */
let_section:
  | let_decls                               { List.rev $1 }
  | /* nothing */                           { [] }

let_decls:
  | let_decls let_decl                      { $2::$1 }  
  | let_decl                                { [$1] }

let_decl:
  | LET typ ID ASSIGN expr SEMI             { LetDecl((Bind(Immutable, $2, $3), $5)) }
  | STRUCT ID LBRACE struct_def RBRACE      { StructDef({sname = $2; fields = List.rev $4}) }
  | EXTERN ID LPAREN formals RPAREN 
    ret_typ SEMI                            { ExternDecl({xalias = $2; xfname = $2; 
                                              xret_typ = $6; xformals = $4;}) }

struct_def:
  | struct_def val_decl SEMI                { $2::$1 }
  | val_decl SEMI                           { [$1] }


/* function declaration rules */
fn_section:
  | fn_decls                                { List.rev $1 }
  | /* nothing */                           { [] }

fn_decls: 
  | fn_decls fn_decl                        { $2::$1 }
  | fn_decl                                 { [$1] } 

fn_decl:
  | fn_type ID LPAREN formals RPAREN ret_typ 
    LBRACE statements ret_expr RBRACE       { {fname = $2; fn_typ = $1; ret_typ = $6;
                                              formals = $4; body = List.rev $8; ret_expr = $9} } 
  | fn_type ID LPAREN formals RPAREN ret_typ 
    LBRACE ret_expr RBRACE                  { {fname = $2; fn_typ = $1; ret_typ = $6;
                                              formals = $4; body = []; ret_expr = $8} } 
ret_typ:
  | typ                                     { Some($1) }
  | /* void return type */                  { None }

ret_expr:
  | expr                                    { Some($1) } 
  | /* nothing */                           { None }


/* function body rules */
statements:                 
  | statements statement SEMI               { $2::$1 }
  | statement SEMI                          { [$1] } 

statement:
  | decl                                    { $1 }
  | expr                                    { Expr($1) } 

decl:
  | decl_mod typ ID                         { VDecl(Bind($1, $2, $3), None) }
  | decl_mod typ ID ASSIGN expr             { VDecl(Bind($1, $2, $3), Some($5)) }

expr:
  | asn_expr                                { $1 } 

asn_expr:
  | unary_expr ASSIGN asn_expr              { Assign($1, $3) }
  | unary_expr asn_op asn_expr              { Assign($1, Binop ($1, $2, $3)) }
  | if_expr                                 { $1 } 

if_expr:
  | id_expr COLON if_expr                   { LookbackDefault($1, $3) }
  | bool_expr QUES fn_expr COLON if_expr    { Cond($1, $3, $5) }
  | IF bool_expr THEN fn_expr ELSE if_expr  { Cond($2, $4, $6) } 
  | fn_expr                                 { $1 }

fn_expr:
  | iter_expr fn_op kns                     { Binop($1, $2, $3) } 
  | iter_expr                               { $1 } 
  | PASS                                    { Call(Some ["nop"],[])  } 

kns:
  | kn fn_op kns                            { Binop($1, $2, $3) }
  | kn                                      { $1 }

kn:
  | id_ns                                      { Id($1) }
  | lambda                                  { Lit($1) }

lambda:
  | LPAREN formals RPAREN ARROW 
    LBRACE statements ret_expr RBRACE       { LitKn({lformals = $2; lbody = List.rev $6; lret_expr = $7}) } 
  | LPAREN formals RPAREN ARROW 
    LBRACE ret_expr RBRACE                  { LitKn({lformals = $2; lbody = []; lret_expr = $6}) } 

iter_expr:
  | iter_type unit_expr gn_call             { Binop($2, $1, $3) }
  | unit_expr                               { $1 }

gn_call:
  | id_ns LPAREN actuals RPAREN             { Call(Some($1), $3) }
  | UNDERSCORE                              { Call(None, []) }

unit_expr:
  | bool_expr                               { $1 } 

bool_expr:
  | bool_or_expr                            { $1 } 

bool_or_expr:
  | bool_or_expr LOG_OR bool_and_expr       { Binop($1, LogOr, $3) } 
  | bool_and_expr                           { $1 } 

bool_and_expr:
  | bool_and_expr LOG_AND cmp_expr          { Binop($1, LogAnd, $3) }
  | cmp_expr                                { $1 }

cmp_expr:
  | eq_expr                                 { $1 }

eq_expr:
  | eq_expr eq_op relat_expr                { Binop($1, $2, $3) }
  | relat_expr                              { $1 }

relat_expr:
  | shift_expr relat_op shift_expr          { Binop($1, $2, $3) }
  | shift_expr                              { $1 }

/* unused, bit-wise operations not implemented */
shift_expr:
  | arithmetic_expr                         { $1 } 

arithmetic_expr:
  | add_expr                                { $1 } 

add_expr:
  | add_expr add_op mult_expr               { Binop($1, $2, $3) }
  | mult_expr                               { $1 }

mult_expr:
  | mult_expr mult_op unary_expr            { Binop($1, $2, $3) }
  | unary_expr                              { $1 }

unary_expr:
  | prefix_op unary_expr                    { Uniop($1, $2) }
  | postfix_expr                            { $1 } 

postfix_expr:
  | postfix_expr LBRACK expr RBRACK         { Binop($1, Index, $3) }
  | id_ns LPAREN actuals RPAREN             { Call(Some($1), $3) }
  | postfix_expr DOT ID                     { Access($1, $3) }
  | primary_expr                            { $1 }

primary_expr:
  | LPAREN expr RPAREN                      { $2 }
  | lit                                     { Lit($1) }
  | id_expr                                 { $1 }

id_expr:
  | id_ns DOTDOT INT_LIT                       { Lookback($1, $3) }
  | id_ns                                      { Id($1) }


/* function argument rules */
formals:
  | formal_list                             { $1 }
  | /* nothing */                           { [] }

formal_list:
  | val_decl COMMA formal_list              { $1::$3 }
  | val_decl                                { [$1] } 

actuals:
  | actual_list                             { $1 }
  | /* nothing */                           { [] } 

actual_list:
  | expr COMMA actual_list                  { $1::$3 }
  | expr                                    { [$1] } 


/* operator rules */
asn_op:
  | ADD_ASN                                 { Add }
  | SUB_ASN                                 { Sub }
  | MUL_ASN                                 { Mul }
  | DIV_ASN                                 { Div }
  | MOD_ASN                                 { Mod }
  | EXP_ASN                                 { Exp }

eq_op:
  | EQ                                      { Eq }
  | NEQ                                     { Neq }

relat_op:
  | LT                                      { Lt }
  | GT                                      { Gt }
  | LEQ                                     { Leq }
  | GEQ                                     { Geq }

add_op:
  | PLUS                                    { Add }
  | MINUS                                   { Sub }

mult_op:
  | TIMES                                   { Mul }
  | DIVIDE                                  { Div }
  | MOD                                     { Mod }
  | EXPONENT                                { Exp }

prefix_op:
  | PLUS                                    { Pos } /* can't return nothing */
  | MINUS                                   { Neg }
  | LOG_NOT                                 { LogNot } 

fn_op:
  | MAP                                     { Map }
  | FILTER                                  { Filter }

iter_type:
  | FOR                                     { For }
  | DO                                      { Do }


/* type rules */
val_decl:
  | typ ID                                  { Bind(Immutable, $1, $2) }

typ:
  | typ LBRACK RBRACK                       { Array($1, None) }
  | typ LBRACK INT_LIT RBRACK               { Array($1, Some($3)) }
  | unit_t                                  { $1 }

unit_t:
  | STRUCT id_ns                            { Struct($2) } /* user defined structs */
  | primitive_t                             { $1 }

primitive_t:
  | INT_T                                   { Int }
  | FLOAT_T                                 { Float }
  | STRING_T                                { String }
  | BOOL_T                                  { Bool }
  | PTR_T                                   { Ptr }
  | vector_t                                { $1 }

vector_t:
  | VECTOR_T LT INT_LIT GT                  { Vector($3) }

fn_type:
  | GN                                      { Gn }
  | KN                                      { Kn }

decl_mod:
  | VAR                                     { Mutable }
  | /* nothing, val */                      { Immutable }


/* literal syntax rules */
lit:
  | struct_lit                              { $1 } 
  | array_lit                               { $1 }
  | vector_lit                              { $1 }
  | STRING_LIT                              { LitStr($1) }
  | BOOL_LIT                                { LitBool($1) }
  | FLOAT_LIT                               { LitFloat($1) }
  | INT_LIT                                 { LitInt($1) }

struct_lit:  
  | id_ns LBRACE struct_lit_fields RBRACE   { LitStruct($1, $3) }
  | id_ns LBRACE RBRACE                     { LitStruct($1, []) }

struct_lit_fields:
  | struct_lit_field SEMI struct_lit_fields { $1::$3 }
  | struct_lit_field                        { [$1] }

struct_lit_field:
  | DOT ID ASSIGN expr                      { StructField($2, $4) }

array_lit:
  | LBRACK lit_elements RBRACK              { LitArray(List.rev $2) }
  | LBRACK RBRACK                           { LitArray([]) }

vector_lit:
  | LPAREN lit_elements COMMA expr RPAREN   { LitVector(List.rev ($4 :: $2)) }

lit_elements:
  | lit_elements COMMA expr                 { $3::$1 }
  | expr                                    { [$1] }

id_ns:
  | ID ARROW id_ns                          { $1::$3 }
  | ID                                      { [$1] }
