(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

open Preprocess
open Ast
open Semant
open Sast
open Parser
open Scanner
open Codegen
open Llvm

type action = Ast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let infile =
        if (Array.length Sys.argv > 2)
            then Sys.argv.(2)
            else raise(Failure("No Filename Argument Found " ^ Sys.argv.(2)))
  in
  let prepo = Preprocess.process infile in
  ignore(print_string prepo);
  let lexbuf = Lexing.from_string prepo in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check ast in
  match action with
    Ast -> print_string (Ast.string_of_program ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
  | Compile -> let m = Codegen.translate sast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
