open Ast
open Ast_utils
open Builtins
open Io
open Values
open Env
;;

let return = Lwt.return
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let _trace =
  let trace_out = Io.default_out "trace" in
  fun txt -> (
    if Driver.get_tracing () then Io.glog true trace_out txt
    else Format.ifprintf Format.std_formatter txt
  )
;;

(* Function environment: maps functions to their definitions *)
let functions = Hashtbl.create 7 ;;

exception UndefinedOperation ;;

(** Translate algebraic constructor into a corresponding
 *  floating-point operation
 *)
let float_op = function
  | Mult -> (fun x y -> x *. y)
  | Plus -> (+.)
  | Minus -> (-.)
  | Div -> (/.)
  | EDiv | Mod -> raise UndefinedOperation
;;

  (*
let get_cell env vname idxs =
  let rec aux r idxs =
    match idxs with
    | [] -> r
    | [i] -> r.(i)
    | i :: is -> aux r.(i) is
  in aux (Env.get vname env) idxs
;;
   *)


  
let rec eval_expr env e =
  match e.e_desc with
  | Int i -> Lwt.return (mk_int i)

  | Real f -> return (mk_float f)

  | String s -> return (mk_string s)

  | Bool b -> return (mk_bool b)

  | ArrayExpr (vname, es) ->
     begin
       let l =
           Lwt_list.map_s
             (fun e -> eval_expr env e >>= fun v -> return (as_int v))
             es
         in
         l >>= fun l ->
           let start, a = as_array !(Env.get env vname) in
           let rec get start a = function
             | [] -> assert false
             | [i] -> !(a.(zero_based_idx start i))
             | i :: is ->
                match !(a.(zero_based_idx start i)) with
                | VArray (s, a) -> get s a is
                | _ -> assert false
           in
           return (get start a l)
     end

  | Var v ->
     let vval =
       try !(Env.get env v) with Not_found -> Builtins.find_constant v in
     return vval

  | UnExpr (uop, e) ->
     begin
       ( match uop.uop_desc with
         | ULog op -> eval_ulog env uop.uop_loc op e
         | UArith op -> eval_uarith env uop.uop_loc op e )
       >>= fun v -> return v
     end

  | BinExpr(bop, e1, e2) ->
       ( match bop.bop_desc with
         | Arith op -> eval_arith env bop.bop_loc op e1 e2
         | Log op -> eval_log env bop.bop_loc op e1 e2
         | Rel op -> eval_rel env bop.bop_loc op e1 e2 )
       >>= fun v -> return v

  | Assigns (lval, e) ->
     eval_expr env e >>=
       fun v ->
       get_cell env lval >>=
       fun cell ->
       cell := v;
       return (mk_unit ())

  | IfThenElse (cond, then_exprs, else_exprs) ->
       eval_expr env cond >>= fun v ->
       let exprs = if as_bool v then then_exprs else else_exprs in
       eval_exprs env exprs

  | While (econd, exprs) ->
     eval_expr env econd >>= fun v ->
     if as_bool v
     then eval_exprs env exprs >>= fun _ -> eval_expr env e
     else return (mk_unit ())

  | Repeat (econd, exprs) ->
     eval_exprs env exprs >>=
       fun _ ->
       eval_expr env econd >>= fun b ->
       if not (as_bool b) then eval_expr env e
       else return (mk_unit ())

  | For (id, e1, e2, step, exprs) ->
     eval_expr env e1 >>= fun init_e ->
     (Env.get env id) := init_e;
     let loc = e.e_loc in
     let mke =  mk_expr loc in
     let id_e = mke (Var id) in
     let step_e = mke (Int step) in
     let bop = mk_bop loc (Arith Plus) in
     let step_e = mke (BinExpr(bop, id_e, step_e)) in
     let last_expr = mk_expr loc (Assigns (Id id, step_e)) in
     let exprs' = exprs @ [last_expr] in
     let ltop = mk_bop loc (Rel Lte) in
     let halt_e = mke (BinExpr (ltop, id_e, e2)) in
     let while_expr = mk_expr loc (While(halt_e, exprs')) in
     eval_expr env while_expr

  | Call (fname, eargs) ->
     Io.debug "Calling %a %d@." Pp.pp_expr e (List.length eargs);
     eval_call e.e_loc fname env eargs

  | Ast.Return e ->
     eval_expr env e

  | Switch (ec, cases) ->
     eval_expr env { e_loc = e.e_loc; e_desc = Ast_utils.switch_as_if ec cases; }

and eval_log env _loc op e1 e2 =
  let do_v1 = fun () -> eval_expr env e1 >>= fun v -> return v
  and do_v2 = fun () -> eval_expr env e2 >>= fun v -> return v in
  let bval =
    match op with
    | Band ->
       do_v1 () >>= fun v1 ->
       if as_bool v1 then do_v2 () >>= fun v2 -> return (as_bool v2)
       else Lwt.return_false
    | Bor ->
       do_v1 () >>= fun v1 ->
       let v = as_bool v1 in
       if v then Lwt.return_true
       else do_v2 () >>= fun v2 -> return (as_bool v2)
    | Bxor ->
       do_v1 () >>=
         fun v1 -> do_v2 () >>= fun v2 ->
       let ev = (fun x y -> (x && not y) || (not x && y)) in
       return (ev (as_bool v1) (as_bool v2))
  in bval >|= mk_bool

and get_cell env lval =
  match lval with
  | Id vname -> return (Env.get env vname)
  | ArrayId (vname, es) ->
     Lwt_list.map_s
       (fun e -> eval_expr env e >>= fun v -> return (as_int v))
       es
     >>= fun es ->
     let start, a = as_array (!(Env.get env vname )) in
     let rec loop a s is =
       match is with
       | [] -> assert false
       | [i] -> return a.(zero_based_idx s i)
       | i :: is ->
          let s, a = as_array !(a.(zero_based_idx s i)) in
          loop a s is
     in loop a start es

and expr_as_cell env e =
  match e.e_desc with
  | Var vname -> get_cell env (Id vname)
  | ArrayExpr (vname, es) -> get_cell env (ArrayId (vname, es))
  | _ -> assert false

and eval_rel env loc op e1 e2 =
  debug "Eval %a and %a in %a"
        Pp.pp_expr e1 Pp.pp_expr e2 Env.pp_env env;
  eval_expr env e1 >>= fun v1 ->
  eval_expr env e2 >>= fun v2 ->
  let ev_op () =
    match op with
    | NotEq -> (<>)
    | Eq -> (=)
    | Gt -> (>)
    | Gte -> (>=)
    | Lt -> (<)
    | Lte -> (<=)
  in

  match v1, v2 with
  | VBool b1, VBool b2 ->
     return (mk_bool ((ev_op ()) b1 b2))
  | VString s1, VString s2 ->
     return (mk_bool (ev_op() s1 s2))
  | VInt i1, VInt i2 ->
     return (mk_bool ((ev_op ()) i1 i2))
  | VFloat f1, VFloat f2 ->
     return (mk_bool ((ev_op ()) f1 f2))
  (* Integers promoted to float if possible *)
  | VFloat f1, VInt i2 ->
     return (mk_bool ((ev_op ()) f1 (float i2)))
  | VInt i1, VFloat f2 ->
     return (mk_bool ((ev_op ()) (float i1) f2))
  | _, _ ->
     Io.error "Cannot apply %s: bad types on line %a: %a, %a"
              (Ast_utils.string_of_rel_op op)
              Location.pp_lines loc
              Values.pp_value v1
              Values.pp_value v2
    ;
     exit 3;


and eval_ulog env _loc op e =
  match op with
  | Bnot ->
       eval_expr env e >>= fun b -> return (mk_bool (not (as_bool b)))

and eval_uarith env loc op e =
  match op with
  | UMinus ->
     begin
       eval_expr env e >>= fun v ->
       match v with
       | VInt i -> return (mk_int (- i))
       | VFloat f -> return (mk_float (-. f))
       | _ -> Io.fail loc "Cannot apply unary arithmetic operator"
     end

and eval_arith env loc op e1 e2 =
     eval_expr env e1 >>= fun v1 ->
     eval_expr env e2 >>= fun v2 ->
     begin
       match v1, v2 with
       | VInt i1, VInt i2 ->
          begin
            return (
                match op with
                | Mult -> mk_int (i1 * i2)
                | Plus  -> mk_int (i1 + i2)
                | Minus -> mk_int (i1 - i2)
                | EDiv -> mk_int (i1 / i2)
                | Mod -> mk_int (i1 mod i2)
                | Div -> mk_float ((float i1) /. (float i2))
              )
          end
       | VFloat f, VInt i ->
          begin
            try
              let g = float_op op in
              return (mk_float (g f (float i)))
            with UndefinedOperation ->
                 Io.fail loc "Cannot apply this operator"
          end
       | VInt i, VFloat f  ->
          begin
            try
              let g = float_op op in
              return (mk_float (g (float i) f))
            with UndefinedOperation ->
                  Io.fail loc "Cannot apply this operator"
          end
       | VFloat f1, VFloat f2 ->
          begin
            try
              let g = float_op op in
              return (mk_float (g f1 f2))
            with UndefinedOperation ->
                 Io.fail loc "Cannot apply this operator"
          end
       | _, _ ->
          (* Any other value combination is interpreted as string concatenation *)
          return
            (mk_string ((to_string v1) ^ (to_string v2)))
     end

and eval_call loc fname env eargs =
  (* trace "%s -> %s" env.current_f fname;*)
  if Builtins.is_builtin fname then (
    let def = Builtins.find_fundef fname in
    let args =
      match def.p_args with
      | SRep SVal ->
         Lwt_list.map_s
           (fun a -> eval_expr env a >>= fun v -> return (ref v))
           eargs

      | SRep SName ->
         (* Everything is passed by reference: this is the case of leia
          *)
         Lwt_list.map_s
           (fun e ->
            assert(Ast_utils.is_lval_compatible e);
            expr_as_cell env e
           ) eargs

      | SVal ->
         assert (List.length eargs >= 1);
         (*List.iter
           (fun a -> Format.printf "%a; " pp_val (snd (eval_expr env a)))
           eargs;*)
         Lwt_list.map_s
           (fun a -> eval_expr env a >>= fun v -> return (ref v))
           eargs

      | _ -> assert false
    in
    let res = def.p_eval args in
    (* trace "%s <- %s" env.current_f fname;*)
    res
  )
  else
      let fdef =
        try Hashtbl.find functions fname
        with Not_found -> Io.fail loc (Format.sprintf "Unknown function name %s"
                                                      fname);
      in
      debug "At %s: %a@." fname Env.pp_env env ;
      (* Adds binding from values to formals *)
      Lwt_list.fold_left_s
        (fun fenv (formal, earg) ->
         let id = Ast_utils.get_formal_name formal in
         match formal with
         | ByRef _ ->
            expr_as_cell env earg
            >>= fun c -> return (add_local fenv id c)
         | ByValue _ ->
            eval_expr env earg >>= fun v ->
            debug "Binding %s to %a@." id pp_value v;
            return (add_local fenv id (ref v))
        ) Env.empty (Utils.zip fdef.fun_formals eargs)
      >>= fun fenv ->
      debug "Inserting locals ...@.";
      Lwt_list.fold_left_s
        (fun fenv v ->
         return (add_local fenv v.var_id (ref (Values.allocate v.var_type))))
        fenv
        fdef.fun_locals
      >>= fun fenv ->
      debug "Executing function %s@." fname;
      eval_exprs { fenv with globals = env.globals } fdef.fun_body


and eval_exprs env exprs =
  let einit = Lwt.return (mk_unit ()) in
  List.fold_left
    (fun ev expr -> ev >>= fun _ -> eval_expr env expr) einit exprs
;;

let mk_declarations vardecls =
  List.fold_left
    (fun env v ->  add_global env v.var_id (ref (allocate v.var_type)))
    Env.empty
    vardecls
;;

let init_functions fundefs =
  List.iter
    (fun fdef ->
     debug "Register function %s@." fdef.fun_id;
     Hashtbl.add functions fdef.fun_id fdef)
    fundefs
;;


(* Evaluate the algorithm *)
let eval algo =
  debug "Starting interpretation of %s@." algo.a_id;
  init_functions algo.a_functions;
  ignore(eval_exprs (mk_declarations algo.a_variables) algo.a_body);
  (* Flush and add newline before retuning *)
  Format.printf "@.";
;;
