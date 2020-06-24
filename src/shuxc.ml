(* shux frontend entry point -- very basic *)
open Scanner
open Parser
open Semant
open Llvm
open Printf 
open Astprint
open Ast_sast
open Sast_cast
open Cast_llast
open Lltranslate

type action = Ast | LLVM

let _ = 
	let (cin, action) = 
    if Array.length Sys.argv = 1 then
       raise (Failure "Argument spec: ./shuxc [-a / -l] [program name]")
       else if Array.length Sys.argv = 3 then
			let a = match Sys.argv.(1) with
				| ("-a") -> Ast
				| ("-l") -> LLVM 
				| _ -> LLVM in
			let c = open_in Sys.argv.(2) in
			(c, a)
		else (open_in Sys.argv.(1), LLVM) in
	let lexbuf = Lexing.from_channel cin in
	let ast = Parser.program Scanner.token lexbuf in
	let check_ast = Semant.check ast in
  let sast = Ast_sast.translate_to_sast check_ast in
  let cast = Sast_cast.sast_to_cast sast in
  let llast = Cast_llast.cast_to_llast cast in
  let llvm = Lltranslate.translate llast in
	match action with
		| Ast -> print_string (Astprint.string_of_program ast)
		| LLVM -> 
    		print_string (Llvm.string_of_llmodule llvm)
