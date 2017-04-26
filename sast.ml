open Ast

(* Expressions *)
type sexpr =
	  SLit of int
	| SBoolLit of bool
	| SMyStringLit of string
	| SVector_lit of sexpr list * typ
	| SId of string * typ
	| SBinop of sexpr * op * sexpr * typ
	| SUnop of uop * sexpr * typ
	| SAssign of sexpr * sexpr * typ
	| SCall of string * sexpr list * typ
	| SVector_access of string * sexpr * typ
	| SNoexpr

(* Statements *)
type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr
	| SReturn of sexpr
	| SIf of sexpr * sstmt * sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt

(* Function Declarations *)
type sfunc_decl = {
	sreturn_type 	: typ;
	sfname 			: string;
	sformals 		: bind list;
	slocals  		: bind list;
	sbody 			: sstmt list;
}

(* All method declarations | Main entry method *)
type sprogram = bind list * func_decl list