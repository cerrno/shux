{
  open Core.Std
  open Parser
  open Exceptions
  let lineno = ref 1
  let depth = ref 0
  let filename = ref "" (* what do with this *)

  let unescape s =
    Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

(* char class regexes *)
let whitespace = [' ' '\t' '\r']
let newline = '\n'
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])

(* type regexes *)
let string = '"' ( (ascii | escape)* as s) '"'
(* we don't support chars
let char = ''' ( ascii | digit ) '''
*)
let float = (digit+) ['.'] digit+
let int = digit+

let id = alpha (alpha | digit | '_')*

rule token = parse
  | whitespace  { token lexbuf }
  | newline     { incr lineno; token lexbuf }
  | "/*"        { incr depth; comment lexbuf }

(* parens *)
  | '('     { LPAREN } 
  | ')'     { RPAREN }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | '['     { LBRACK }
  | ']'     { RBRACK }

(* strings *)
  | '"'     { DBL_QUOTE }
  | '\''    { SNG_QUOTE }

(* separators *)
  | ';'     { SEMI }
  | ','     { COMMA }
  | ".."    { DOTDOT }
  | '.'     { DOT }

(* arithmetic operators *)
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIVIDE }
  | '%'     { MOD }
  | '^'     { EXPONENT }

(* assignment operators *)
  | '='     { ASSIGN }
  | "+="    { ADD_ASN }
  | "-="    { SUB_ASN }
  | "*="    { MUL_ASN }
  | "/="    { DIV_ASN }
  | "%="    { MOD_ASN }
  | "^="    { EXP_ASN }

(* logical operators *)
  | "&&"    { LOG_AND }
  | "||"    { LOG_OR }
  | "!"     { LOG_NOT }

(* comparison operators *)
  | '<'     { LT }
  | '>'     { GT }
  | "=="    { EQ }
  | "!="    { NEQ }
  | "<="    { LEQ }
  | ">="    { GEQ }

(* shux *)
  | '?'     { QUES }
  | ':'     { COLON }
  | "::"    { FILTER }
  | '@'     { MAP }
  | "->"    { ARROW }

(* control keywords *)
  | "if"      { IF }
  | "then"    { THEN }
  | "elif"    { ELIF }
  | "else"    { ELSE }
  | "for"     { FOR }
  | "while"   { WHILE }
  | "do"      { DO }
  | "noop"    { PASS } 
(* declarations *)
  | "ns"      { NS }
  | "gn"      { GN }
  | "kn"      { KN }
  | "struct"  { STRUCT }
  | "let"     { LET }
  | "var"     { VAR }
  | "extern"  { EXTERN }

(* types *)
  | "int"               { INT_T }
  | "scalar" | "float"  { FLOAT_T }
  | "string"            { STRING_T }
  | "bool"              { BOOL_T }
  | "vector"            { VECTOR_T }
  | "_ptr"              { PTR_T }

(* literals *)
  | "true" | "false" as tf  { BOOL_LIT(bool_of_string tf) } 
  | int as i                { INT_LIT(int_of_string i) }
  | float as f              { FLOAT_LIT(Float.of_string f) }
  | string                  { STRING_LIT(unescape s)}
  | id as n                 { ID(n) }
  | '_'                     { UNDERSCORE }
(* ye good olde *)
  | eof       { EOF }

(* The Reign of Error *)
  | '"'       { raise (Exceptions.UnmatchedQuotation(!lineno)) }
  | _ as e    { raise (Exceptions.IllegalCharacter(!filename, e, !lineno)) }

(* comments *)
and comment = parse
  | "/*"      { incr depth; comment lexbuf }
  | "*/"      { decr depth; if !depth > 0 then comment lexbuf else token lexbuf }
  | newline   { incr lineno; comment lexbuf }
  | _         { comment lexbuf }
