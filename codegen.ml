(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
module S = Sast 

module StringMap = Map.Make(String)
 
let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MatriCs"
  and i32_t  = L.i32_type  context
  and f64_t  = L.double_type context 
  (* and i8_t   = L.i8_type   context *) 
  and i1_t   = L.i1_type   context
  and str_t  = L.pointer_type (L.i8_type (context))
  and array_t = L.array_type 
  and void_t = L.void_type context in

  let rec ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> f64_t 
    | A.Bool -> i1_t
    | A.MyString -> str_t
    | A.Void -> void_t 
    | A.Vector(typ, szl) -> match szl with 
      [] -> ltype_of_typ typ
      | [x] ->  array_t (ltype_of_typ (A.Vector(typ, []))) x
      | hd::tl -> array_t (ltype_of_typ (A.Vector(typ, tl))) hd 
  in

  let rec lconst_of_typ = function
      A.Int -> L.const_int i32_t 0
    | A.Float -> L.const_float f64_t 0.0
    | A.Bool -> L.const_int i1_t 0
    (* | A.MyString -> L.const_string str_t 0 *)
    (* | A.Void -> () *)
    | A.Vector(typ, szl) -> 
      let rec build0array i arr = 
        if i>0 then
          build0array (i-1) (Array.append [|lconst_of_typ typ|] arr)
        else
          arr
      in
      (match szl with
      | [x] -> L.const_array (ltype_of_typ typ) (build0array (x) [|lconst_of_typ typ|])
      | hd::tl ->  L.const_array (ltype_of_typ typ) (build0array (hd) [|lconst_of_typ (Vector(typ, tl))|]) )
(*                                 List. (Array.append [|L.const_of_typ typ|]) (Array.of_list)) *)
    (* | A.Vector(typ, szl) -> raise(Failure("cannot return vectors of more than 2 dimensions at the moment"))(* match szl with  *)
      [] -> lconst_of_typ typ
      | [x] ->  L.const_array (array_t (lconst_of_typ (A.Vector(typ, [])))) x
      | hd::tl -> L.const_array (array_t (lconst_of_typ (A.Vector(typ, tl)))) hd  *)
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.S.sfname
      and formal_types =
	   Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.S.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.S.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.S.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
    	let local = L.build_alloca (ltype_of_typ t) n builder in
    	ignore (L.build_store p local builder);
    	StringMap.add n local m in

      let add_local m (t, n) =
      	let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.S.sformals
        (Array.to_list (L.params the_function)) in
        List.fold_left add_local formals fdecl.S.slocals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ex =  
      let build_vect vname indices assign =  
        if assign > 0 then
          L.build_gep (lookup vname) (Array.append [|L.const_int i32_t 0|] (Array.of_list (List.map (fun e -> expr builder e) indices))) vname builder
        else 
          L.build_load (L.build_gep (lookup vname) (Array.append [|L.const_int i32_t 0|] (Array.of_list (List.map (fun e -> expr builder e) indices))) vname builder) vname builder
      in 
      let build_vect_ref vname i = 
        let rec make_empty_lst i1 l = 
          if i1 > 0 then
            make_empty_lst (i1-1) (0::l)
          else 
            l 
        in 
        let eml = make_empty_lst i [] in
        (L.build_in_bounds_gep (lookup vname) ( Array.append [|L.const_int i32_t 0|] (Array.of_list (List.map (fun e -> expr builder (S.SLit(e))) eml))) vname builder)
      in  
    (match ex with 
	   S.SLit i -> L.const_int i32_t i
      | S.SFlit f -> L.const_float f64_t f
      | S.SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | S.SMyStringLit str -> L.build_global_stringptr str "tmp" builder
      | S.SNoexpr -> L.const_int i32_t 0
      | S.SId(s, _) -> L.build_load (lookup s) s builder
      | S.SVector_lit(el, ty, diml) -> (match diml with 
         [x] -> L.const_array (ltype_of_typ ty) (Array.of_list (List.map (expr builder) el))
        | hd::tl -> L.const_array (ltype_of_typ (A.Vector(ty,tl))) (Array.of_list (List.map (expr builder) el)))
      | S.SBinop (e1, op, e2, t1, t2, t) ->
    	  let e1' = expr builder e1
    	  and e2' = expr builder e2 in
    	  let b = (match op with
    	    A.Add when t = A.Int 		-> L.build_add
				| A.Add when t = A.Float 	-> L.build_fadd 
        | A.Mod when t = A.Int    -> L.build_srem 
        | A.Mod when t = A.Float  -> L.build_frem         
				| A.Sub when t = A.Int 		-> L.build_sub 
				| A.Sub when t = A.Float 	-> L.build_fsub 
				| A.Mult when t = A.Int 	-> L.build_mul 
				| A.Mult when t = A.Float -> L.build_fmul 
				| A.Div when t = A.Int 		-> L.build_sdiv 
				| A.Div when t = A.Float 	-> L.build_fdiv 
    	  | A.And     -> L.build_and
    	  | A.Or      -> L.build_or
    	  | A.Equal   -> L.build_icmp L.Icmp.Eq
    	  | A.Neq     -> L.build_icmp L.Icmp.Ne
    	  | A.Less    -> L.build_icmp L.Icmp.Slt
    	  | A.Leq     -> L.build_icmp L.Icmp.Sle
    	  | A.Greater -> L.build_icmp L.Icmp.Sgt
    	  | A.Geq     -> L.build_icmp L.Icmp.Sge
    	  ) in
				let (e1'', e2'') = match t with
					Float ->
						((match t1 with
						Int -> L.build_sitofp e1' (ltype_of_typ t) "tmp1" builder
						| _ -> e1'),
						(match t2 with
						Int -> L.build_sitofp e2' (ltype_of_typ t) "tmp2" builder
						| _ -> e2'))
					| _ -> (e1', e2')
				in b e1'' e2'' "tmp" builder
      | S.SUnop(op, e, t) ->
    	  let e' = expr builder e in
    	  (match op with
    	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | S.SAssign (s, e, _) -> let e' = expr builder e in
        (match s with
          SId(var, _) -> ignore (L.build_store e' (lookup var) builder); e'
      | SVector_access(vname, idx, ty) -> let gep = build_vect vname idx 1 in 
            ignore (L.build_store e' gep builder); e'
  	    | _ -> raise(Failure("should not reach here")))
      | S.SCall ("print_int", [e], _) | S.SCall ("printb", [e], _) ->
  	     L.build_call printf_func [| int_format_str ; (expr builder e) |]
  	       "printf" builder
      | S.SCall ("print_float", [e], _) -> 
         L.build_call printf_func [| float_format_str ; (expr builder e) |]
            "printf" builder
      | S.SCall ("print", [e], _) ->
          L.build_call printf_func [| (expr builder e) |] "printf" builder
      | S.SCall (f, act, _) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
  	    let actuals = List.rev (List.map (expr builder) (List.rev act)) in
  	    let result = (match fdecl.S.styp with A.Void -> ""
                                              | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list actuals) result builder
      | S.SVector_access (vname, idx, typ) -> 
            build_vect vname idx 0
    
      | S.SDimlist (v, dl) -> L.const_array i32_t (Array.of_list (List.map (fun i -> L.const_int i32_t i) dl)))
      in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	     S.SBlock sl -> List.fold_left stmt builder sl
      | S.SExpr e -> ignore (expr builder e); builder
      | S.SReturn e -> ignore (match fdecl.S.styp with
	       A.Void -> L.build_ret_void builder
	       | _ -> L.build_ret (expr builder e) builder); builder
      | S.SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
      	 let merge_bb = L.append_block context "merge" the_function in

      	 let then_bb = L.append_block context "then" the_function in
      	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
      	   (L.build_br merge_bb);

      	 let else_bb = L.append_block context "else" the_function in
      	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
      	   (L.build_br merge_bb);

      	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
      	 L.builder_at_end context merge_bb

      | S.SWhile (predicate, body) ->
    	  let pred_bb = L.append_block context "while" the_function in
    	  ignore (L.build_br pred_bb builder);

    	  let body_bb = L.append_block context "while_body" the_function in
    	  add_terminal (stmt (L.builder_at_end context body_bb) body)
    	    (L.build_br pred_bb);

    	  let pred_builder = L.builder_at_end context pred_bb in
    	  let bool_val = expr pred_builder predicate in

    	  let merge_bb = L.append_block context "merge" the_function in
    	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    	  L.builder_at_end context merge_bb
      | S.SFor (e1, e2, e3, body) -> stmt builder
	    ( S.SBlock [S.SExpr e1 ; S.SWhile (e2, S.SBlock [body ; S.SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (S.SBlock fdecl.S.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.S.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (lconst_of_typ t))  (* (L.const_int (ltype_of_typ t) 0)) *)
  in

  List.iter build_function_body functions;
  the_module
