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

      { typ = Void; fname = "printb"; formals = [(Bool, "x")];
      locals = []; body = [] };

      ]
   in 

   let built_in_decls_names = [ "print_int"; "printb" ]

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

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
  	     Literal _ -> Int
        | BoolLit _ -> Bool
        | Id s -> type_of_identifier s
        | MyStringLit _ -> MyString
        | Vector_lit elements -> 
          let rec check_vector_types i = function
            | [] -> raise( Failure ("empty literal not allowed"))
            | [el] -> Vector(expr el, i+1)
            | fst :: snd :: tail -> 
              if (expr fst) == (expr snd) then
                check_vector_types (i+1) (snd::tail)
              else raise (Failure ("unmatched element types in vector literal " ^
                string_of_typ (expr fst) ^ ", " ^ string_of_typ (expr snd))) 
          in check_vector_types 0 elements 
        
        | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
        	(match op with
                Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
              | Equal | Neq when t1 = t2 -> Bool
              | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
              | And | Or when t1 = Bool && t2 = Bool -> Bool
              | _ -> raise (Failure ("illegal binary operator " ^
                    string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                    string_of_typ t2 ^ " in " ^ string_of_expr e))
          )
        | Unop(op, e) as ex -> let t = expr e in
        	(match op with
          	    Neg when t = Int -> Int
          	  | Not when t = Bool -> Bool
                    | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
          	  		    string_of_typ t ^ " in " ^ string_of_expr ex))
          )
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
        | Vector_access(nm, idx) -> let idxtyp = expr idx in 
          let Vector(typ, sz) = type_of_identifier nm in 
          if idxtyp != Int then raise (Failure ("array " ^ nm 
                 ^ " index not an integer"))
          (* TODO: TRY IMPLEMENTING INDEX OUT OF BOUNDS 
          else let Vector(typ, sz) = type_of_identifier nm in 
            if (sz - 1) < idx then raise (Failure ("array " ^ nm 
                 ^ " index out of bounds")) *)
            else typ
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
      SLit(_) -> Int
      | SBoolLit(_) -> Bool
      | SMyStringLit(_) -> MyString
      | SBinop(_, op, _, t)        -> 
      (match op with
        Add | Sub | Mult | Div -> t
        | Equal | Neq -> Bool
        | Less | Leq | Greater | Geq -> Bool
        | And | Or -> Bool
        | _ -> raise (Failure ("should not reach here"))
      )
      | SAssign(_, _, t)          -> t
      | SCall(_, _, t)          -> t
      | SUnop(_, _, t)          -> t
      | SVector_access(_, _, t) -> t
      | _ -> raise (Failure ("vector type not supported"))
    in

    (* Return the type of an expression or throw an exception *)
    let rec sexpr = function
       Literal i -> SLit(i)
      | BoolLit b -> SBoolLit(b)
      | Id s -> SId(s, type_of_identifier s)
      | MyStringLit st -> SMyStringLit(st)
      | Vector_lit elements -> 
        let selements = List.map sexpr elements in
        SVector_lit(selements, sast_to_typ(sexpr (List.hd elements)))    
      
      | Binop(e1, op, e2) -> let t1 = sexpr e1 and t2 = sexpr e2 in
        SBinop(t1, op, t2, sast_to_typ(SBinop(t1,op,t2,Int))) (* TODO: Ugly code *)
      | Unop(op, e) -> let t = sexpr e in
        SUnop(op, t, sast_to_typ(t))
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
          let sidx = sexpr idx in
          SVector_access(vname, sidx, typ)

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
    (*
    let fdecl_to_func_st globals fdecl =
      let ffunc_st = List.fold_left 
        (fun m (t, f) ->  f t m) StringMap.empty fdecl.formals in
      let lffunc_st = List.fold_left 
        (fun m (t, l) -> StringMap.add l t m) ffunc_st fdecl.locals in
      List.fold_left (fun m g -> StringMap.add (snd g) (fst g) m) lffunc_st globals in
    *)
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

    (*let sfdecls = List.map ast_to_sast functions in
    (globals, sfdecls)
     *)
    (*let ASTtoSAST func =
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
          SLit(_) -> Int
          | SBoolLit(_) -> Bool
          | SMyStringLit(_) -> MyString
          | SBinop(_, _, _, t)        -> t
        | SAssign(_, _, t)          -> t
        | SCall(_, _, t)          -> t
        | SUnop(_, _, t)          -> t
          | _ -> raise (Failure ("vector type not supported"))
        in

        (* Return the type of an expression or throw an exception *)
        let rec sexpr = function
             Literal i -> SLit(i)
            | BoolLit b -> SBoolLit(b)
            | Id s -> SId(s, type_of_identifier s)
            | MyStringLit st -> SMyStringLit(st)
            | Vector_lit elements -> 
              let selements = List.map sexpr elements in
              SVector_lit(selements, sast_to_typ(sexpr (List.hd elements)))    
            
            | Binop(e1, op, e2) as e -> let t1 = sexpr e1 and t2 = sexpr e2 in
              SBinop(t1, op, t2, sast_to_typ(t1))
            | Unop(op, e) as ex -> let t = sexpr e in
              SUnop(op, t, sast_to_typ(t))
            | Noexpr -> SNoexpr 

            | Assign(var, e) as ex -> let lt = sexpr var in
                                      let rt = sexpr e in
                                      let ty = type_of_identifier var
              in
              SAssign(sexpr var, rt, ty)
            | Call(fname, actuals) as call -> (* let fd = function_decl fname in *)
              let sactuals = List.map sexpr actuals in
              SCall(fname, sactuals, sast_to_typ(sexpr (List.hd sactuals)))  
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
          sstmt (Block func.body);
      in 
    ASTtoSAST (globals @ functions)
  in sast*)