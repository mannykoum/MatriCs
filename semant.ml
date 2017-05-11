(*Semantic checking for MatriCs compiler*)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	     n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    (match lvaluet with 
      Vector(ltyp, lsize) -> 
        (match rvaluet with
          Vector(rtyp, rsize) -> if ltyp == rtyp then ltyp else raise err
          | _ -> if ltyp == rvaluet then ltyp else raise err)
      | _ -> if lvaluet == rvaluet then lvaluet else raise err)
  in

  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  
  if List.mem "int_to_string" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function int_to_string may not be defined")) else ();
  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();


  (* Function declaration for a named function *)
    let built_in_decls_funcs =  [
      { typ = Void; fname = "print_int"; formals = [(Int, "x")];
      locals = []; body = [] };

      { typ = Void; fname = "print_float"; formals = [(Float, "x")];
      locals = []; body = [] };

      { typ = Void; fname = "printb"; formals = [(Bool, "x")];
      locals = []; body = [] };
(*
      { typ = Void; fname = "dimlist"; formals = [(Vector(), "v")];
      locals = []; body = [] };
*)
      ]
   in 

   let built_in_decls_names = [ "print_int"; "print_float"; "printb"(*; "dimlist"*)]

 in

  let built_in_decls = List.fold_right2 (StringMap.add)
                        built_in_decls_names
                        built_in_decls_funcs
                        (StringMap.singleton "print"
                                { typ = Void; fname = "print"; formals = [(MyString, "x")];
                                locals = []; body = [] })

  in

     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)


  (* -- Top-level function -- *)
  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	     StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Function to print all symbols *)
    (* List.iter (fun (t, n) -> print_string ( string_of_typ t ^ " " ^ n)) func.formals; *)

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let match_binop t1 t2 op e =
      (match op with
      Add | Sub | Mult | Mod | Div -> 
				(match t1 with
      		Int -> 
						(match t2 with 
							Int -> Int
							| Float -> Float
							| _ -> raise (Failure ("undefined operation " ^
          			string_of_op op ^ "on type " ^ string_of_typ t2)))
      		| Float -> Float
					| _ -> raise (Failure ("undefined operation " ^
          			string_of_op op ^ "on type " ^ string_of_typ t1)))
 	   	| Equal | Neq when t1 = t2 -> Bool
  	 	| Less | Leq | Greater | Geq 
				when t1 = Int && t2 = Int -> Bool
  	 	| Less | Leq | Greater | Geq 
				when t1 = Float && t2 = Float -> Bool
   	 	| And | Or when t1 = Bool && t2 = Bool -> Bool
   	 	| _ -> raise (Failure ("illegal binary operator " ^
          string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
          string_of_typ t2 ^ " in " ^ string_of_expr e)))
		in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
  	     Literal _ -> Int
        | Fliteral _ -> Float
        | BoolLit _ -> Bool
        | Id s -> type_of_identifier s
        | MyStringLit _ -> MyString
        | Vector_lit elements -> (* This is weak *) 
(*           let rec vector_dims i = function
            | [x] when x = Vector_lit(lst) -> vector_dims i+1 lst
            | [x] -> i
            | x::tl when x = Vector_lit(lst) -> vector_dims i+1 lst
            | x::tl -> i
          in *)
          let rec check_vector_types i = function
            | [] -> raise( Failure ("empty literal not allowed"))
            | [el] -> Vector(expr el, [i+1]) (* HACKY FIX  *)
            | fst :: snd :: tail -> 
              if (expr fst) == (expr snd) then
                check_vector_types (i+1) (snd::tail)
              else raise (Failure ("unmatched element types in vector literal " ^
                string_of_typ (expr fst) ^ ", " ^ string_of_typ (expr snd))) 
          in check_vector_types 0 elements 
(*           let depth = vector_dims 1 elements in 
          while depth>0 do
            check_vector_types *)
          done
          let rec check_vector_types i = function
            | [] -> raise( Failure ("empty literal not allowed"))
            | [el] -> Vector(expr el, [i+1]) (* HACKY FIX  *)
            | fst :: snd :: tail -> 
              if (expr fst) == (expr snd) then
                check_vector_types (i+1) (snd::tail)
              else raise (Failure ("unmatched element types in vector literal " ^
                string_of_typ (expr fst) ^ ", " ^ string_of_typ (expr snd))) 
          in check_vector_types 0 elements 

    
        | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
		      (match op with
		      Add | Sub | Mod | Mult | Div -> 
						(match t1 with
		      		Int -> 
								(match t2 with 
									Int -> Int
									| Float -> Float
									| _ -> raise (Failure ("undefined operation " ^
		          			string_of_op op ^ "on type " ^ string_of_typ t2)))
		      		| Float -> Float
							| _ -> raise (Failure ("undefined operation " ^
		          			string_of_op op ^ "on type " ^ string_of_typ t1)))
		 	   	| Equal | Neq when t1 = t2 -> Bool
		  	 	| Less | Leq | Greater | Geq 
						when t1 = Int && t2 = Int -> Bool
		  	 	| Less | Leq | Greater | Geq 
						when t1 = Float && t2 = Float -> Bool
		   	 	| And | Or when t1 = Bool && t2 = Bool -> Bool
		   	 	| _ -> raise (Failure ("illegal binary operator " ^
		          string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
		          string_of_typ t2 ^ " in " ^ string_of_expr e)))
				| Unop(op, e) as ex -> let t = expr e in
        	(match op with
          	    Neg when t = Int -> Int
							|	Neg when t = Float -> Float
          	  | Not when t = Bool -> Bool
                    | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
          	  		    string_of_typ t ^ " in " ^ string_of_expr ex))
          )
        | Incrementer(e, uop) as ex -> let t = expr e in 
          (match t with
            Int -> Int 
            | _ -> raise (Failure("illegal type cannot be incremented " ^ string_of_typ t)))
        | Noexpr -> Void
        | Assign(var_ex, e) as ex -> 
          (match var_ex with
            Id var -> let lt = type_of_identifier var
                      and rt = expr e in
              check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
  				     " = " ^ string_of_typ rt ^ " in " ^ 
  				     string_of_expr ex))
            | Vector_access(vnm, idx) -> let lt = expr var_ex
                                        and rt = expr e in
              check_assign lt rt (Failure ("illegal vector assignment " ^ string_of_typ lt ^
               " = " ^ string_of_typ rt ^ " in " ^ 
               string_of_expr ex))
            | _ -> raise (Failure ("illegal assignment " ^
            string_of_expr var_ex ^ " = " ^ string_of_expr e ^ 
            " in " ^ string_of_expr ex)))
        | Call(fname, actuals) as call -> let fd = function_decl fname in
           if List.length actuals != List.length fd.formals then
             raise (Failure ("expecting " ^ string_of_int
               (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
           else
             List.iter2 (fun (ft, _) e -> let et = expr e in
                ignore (check_assign ft et
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
               fd.formals actuals;
             fd.typ
        | Vector_access(nm, ilst) -> let Vector(typ, sz) = type_of_identifier nm in
            (let rec check_i = function
              [idx] -> let idxtyp = expr idx in
                    if idxtyp != Int then raise (Failure ("array " ^ nm 
                      ^ " index not an integer"))
                    else typ
              | hd::tl -> let idxtyp = expr hd in
              if idxtyp != Int then raise (Failure ("array " ^ nm 
                   ^ " index not an integer"))
            else check_i tl in check_i ilst)
        | Dimlist s -> let nm = type_of_identifier s in
            (match nm with
              Vector(typ, szl) -> Vector(Int, [(List.length szl)])
            | _ -> raise(Failure("dims cannot be called on non-vector type "^s)))
          (* TODO: TRY IMPLEMENTING INDEX OUT OF BOUNDS 
          else let Vector(typ, sz) = type_of_identifier nm in 
            if (sz - 1) < idx then raise (Failure ("array " ^ nm 
                 ^ " index out of bounds")) *)
    in

    let check_bool_expr e = if expr e != Bool
      then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      else () 
    in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
	   Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in
    stmt (Block func.body);
  in

  List.iter check_function functions;

  let ast_to_sast func =
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
      StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Function to print all symbols *)
    (* List.iter (fun (t, n) -> print_string ( string_of_typ t ^ " " ^ n)) func.formals; *)

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let sast_to_typ = function
      SId(_, t)                   -> t
      | SLit(_)                   -> Int
      | SFlit(_)                  -> Float 
      | SBoolLit(_)               -> Bool
      | SMyStringLit(_)           -> MyString
      | SBinop(_, op, _, _, _, t) -> t
      | SAssign(_, _, t)          -> t
      | SCall(_, _, t)            -> t
      | SUnop(_, _, t)            -> t
      | SVector_access(_, _, t)   -> t
      | _ -> raise (Failure ("vector type not supported"))
    in

    (* Return the type of an expression or throw an exception *)
    let rec sexpr = function
       Literal i -> SLit(i)
      | Fliteral f -> SFlit(f)
      | BoolLit b -> SBoolLit(b)
      | Id s -> SId(s, type_of_identifier s)
      | MyStringLit st -> SMyStringLit(st)

      | Vector_lit elements ->
        let rec check_vect dimlist elm = 
          let el = List.hd elm in 
            match el with 
              Vector_lit(ellist) -> check_vect (List.length elm :: dimlist) ellist 
              | x -> ((List.length elm :: dimlist), sexpr x) 
        in let dml, xtyp = check_vect [] elements 
        and selements = List.map sexpr elements in
        (* SVector_lit(selements, (sast_to_typ xtyp), dml) *)
        let head = List.hd selements in
          (match head with
           _ -> SVector_lit(selements, (sast_to_typ xtyp), dml) 
          | SVector_lit(_, childtyp , childdml) -> SVector_lit(selements, Vector(childtyp, childdml), dml))
      | Binop(e1, op, e2) as e -> let t1 = sexpr e1 and t2 = sexpr e2 in
	      let typ1 = sast_to_typ t1 and typ2 = sast_to_typ t2 in
				let typ_of_bop = 
					(match op with
		      Add | Sub | Mult | Mod | Div -> 
						(match typ1 with
		      		Int -> 
								(match typ2 with 
									Int -> Int
									| Float -> Float
									| _ -> raise (Failure ("undefined operation " ^
		          			string_of_op op ^ "on type " ^ string_of_typ typ2)))
		      		| Float -> Float
							| _ -> raise (Failure ("undefined operation " ^
		          			string_of_op op ^ "on type " ^ string_of_typ typ1)))
		 	   	| Equal | Neq when typ1 = typ2 -> Bool
		  	 	| Less | Leq | Greater | Geq 
						when typ1 = Int && typ2 = Int -> Bool
		  	 	| Less | Leq | Greater | Geq 
						when typ1 = Float && typ2 = Float -> Bool
		   	 	| And | Or when typ1 = Bool && typ2 = Bool -> Bool
		   	 	| _ -> raise (Failure ("illegal binary operator " ^
		          string_of_typ typ1 ^ " " ^ string_of_op op ^ " " ^
		          string_of_typ typ2))) in
        SBinop(t1, op, t2, typ1, typ2, typ_of_bop)
      | Unop(op, e) -> let t = sexpr e in
        SUnop(op, t, sast_to_typ(t))
      | Incrementer(e, uop) -> 
         (match uop with
            Increment -> sexpr (Assign(e, Binop(e, Add, Literal(1))))
            | Decrement -> sexpr (Assign(e, Binop(e, Sub, Literal(1)))) )(* let t = sexpr e in 
        let type_of_e = sast_to_typ t in
        let type_of_exp = (
          match type_of_e with 
            Int -> Int
          | _ -> raise (Failure ("Illegal type " ^ string_of_typ type_of_e ^ " " ^ "cannot be incremented"))
        ) in
        SIncremeneter(e, uop, type_of_exp)
 *)
      | Noexpr -> SNoexpr 

      | Assign(var_ex, e) as ex-> 
        (match var_ex with
          Id var -> let lt = sexpr var_ex
                    and rt = sexpr e 
                    and ty = type_of_identifier var in
                    SAssign(lt, rt, ty)
          | Vector_access(vnm, idx) -> 
            let lt = sexpr var_ex
            and rt = sexpr e 
            and vty = type_of_identifier vnm in
            (match vty with
             Vector(ty, _) -> SAssign(lt, rt, ty)
            | _ -> raise(Failure("illegal assignment " ^
            string_of_expr var_ex ^ " = " ^ string_of_expr e ^ 
            " in " ^ string_of_expr ex)) (* should not reach here after check *) )
          | _ -> raise(Failure("illegal assignment " ^
            string_of_expr var_ex ^ " = " ^ string_of_expr e ^ 
            " in " ^ string_of_expr ex))) (* should not reach here after check *)
        (* let lt = sexpr var_ex in
        let rt = sexpr e in
        let ty = type_of_identifier s
          in SAssign(lt, rt, ty) *)
      | Call(fname, actuals) -> let fd = function_decl fname in
          let sactuals = List.map sexpr actuals in
          SCall(fname, sactuals, fd.typ) (* sast_to_typ(sexpr (List.hd sactuals))) *)
      | Vector_access(vname, idx) -> let Vector(typ, _) = type_of_identifier vname in 
          let sidx = List.map sexpr idx in 
          SVector_access(vname, sidx, typ)
      | Dimlist s -> let vect = type_of_identifier s in
        (match vect with 
          Vector(t,dl) -> SDimlist(s, dl)
          | _ -> raise(Failure("dims cannot be called on non-vector type "^s))
        )


    in

    let rec sstmt = function
    (*    SBlock of sstmt list   *)
      Return(e)       -> SReturn(sexpr e)
    | Block(stmt_l)     -> SBlock(List.map sstmt stmt_l)
    | Expr(e)         -> SExpr(sexpr e)
    | If(e, s1, s2)     -> SIf((sexpr e), (sstmt s1), (sstmt s2))
    | For(e1, e2, e3, s)  -> SFor((sexpr e1), (sexpr e2), (sexpr e3), (sstmt s))
    | While(e, s)     -> SWhile((sexpr e), (sstmt s))
    in
  List.map sstmt func.body;
in

  let convert_fdecl_to_sfdecl fdecl =
    {
      sfname        = fdecl.fname;
      styp          = fdecl.typ;
      sformals      = fdecl.formals;
      slocals       = fdecl.locals;
      sbody         = ast_to_sast fdecl;
    }
  in 
    
  let 
    sfdecls = List.map convert_fdecl_to_sfdecl functions 
  in 
  (globals, sfdecls)

