open Ast
open Ast_utils
open Base
open Base.ValEnv
open Builtins
module TM = Base.TypedMem
open TM
open Io
;;

let return = Lwt.return
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let trace =
  let trace_out = Io.default_out "trace" in
  fun txt -> (
    if Driver.get_tracing () then
      Io.glog true trace_out txt
    else Format.ifprintf Format.std_formatter txt
  )
;;

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

let expr_to_lval e =
  match e.e_desc with
  | Var v -> Id v
  | ArrayExpr (name, es) -> ArrayId(name, es)
  | _ -> assert false
;;

let rec eval_expr env e =
  match e.e_desc with
  | Int i -> Lwt.return (env, mk_int i)
  | Real f -> return (env, mk_float f)
  | String s -> return (env, mk_string s)
  | Bool b -> return (env, mk_bool b)
  | ArrayExpr (vname, es) ->
     begin
       try
         let l =
           Lwt_list.map_s
             (fun e -> eval_expr env e >>= fun (_, v) -> return v)
             es
         in
         l >>=
           fun l ->
           let as_int = function
             | Immediate (VInt (Some i)) -> i
             | _ -> assert false
           in
           let p = List.map as_int l in
           return (env, ValEnv.find env vname p)
       with Not_found ->
         let msg = Format.sprintf "Unbound variable: %s" vname in
         Io.fail e.e_loc msg
     end
  | Var v ->
     begin
       try
         let vval = try ValEnv.find env v []
                    with Not_found -> Builtins.find_constant v in
         return (env, vval)
       with
       | Not_found ->
          let msg = Format.sprintf "Unbound variable: %s" v in
          Io.fail e.e_loc msg
     end
  | UnExpr (uop, e) ->
     begin
       ( match uop.uop_desc with
         | ULog op -> eval_ulog env uop.uop_loc op e
         | UArith op -> eval_uarith env uop.uop_loc op e )
       >>= fun v -> return (env, v)
     end
  | BinExpr(bop, e1, e2) ->
     begin
       ( match bop.bop_desc with
         | Arith op -> eval_arith env bop.bop_loc op e1 e2
         | Log op -> eval_log env bop.bop_loc op e1 e2
         | Rel op -> eval_rel env bop.bop_loc op e1 e2 )
       >>=
         fun v -> return (env, v)
     end
  | Assigns (lval, e) ->
     eval_expr env e >>=
       fun (env, v) ->
       eval_lval env lval >>=
       fun (basename, offsets) ->
       debug "Assigns new value for %s: %a from %a@."
           basename pp_mvalue v
           Ast_utils.Pp.pp_expr e
       ;
       return (ValEnv.add env basename offsets v, mk_unit ())

  | IfThenElse (cond, then_exprs, else_exprs) ->
     begin
      eval_expr env cond >>= fun (env, v) ->
      match v with
      | Immediate (VBool (Some b)) ->
         let exprs = if b then then_exprs else else_exprs in
         eval_exprs env exprs
      | _ ->
         let msg = "This expression should be boolean." in
         Error.errloc cond.e_loc msg;
     end

  | While (econd, exprs) ->
     eval_expr env econd >>= fun (env, v) ->
     begin
        match  v with
       | Immediate (VBool (Some b)) ->
          if b then
            eval_exprs env exprs >>= fun (env, _) -> eval_expr env e
          else return (env, mk_unit ())
       | _ ->
          let msg = "This expression should be boolean." in
          Error.errloc e.e_loc msg;
     end

  | Repeat (econd, exprs) ->
     begin
       eval_exprs env exprs >>=
         fun (env', _v) ->
         eval_expr env' econd >>= fun (env, v) ->
         match v with
         | Immediate (VBool (Some b)) ->
            if not b then eval_expr env e
            else return (env', mk_unit ())
         | _ ->
            let msg = "This expression should be boolean." in
            Error.errloc e.e_loc msg;
     end

  | For (id, e1, e2, step, exprs) ->
     eval_expr env e1 >>= fun (_, init_e) ->
     let env' = ValEnv.add_immediate env id init_e in
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
     eval_expr env' while_expr

  | Call (fname, eargs) ->
     Io.debug "Calling %a %d@." Pp.pp_expr e (List.length eargs);
     eval_call e.e_loc fname env eargs

  | Ast.Return e ->
     eval_expr env e

  | Switch (ec, cases) ->
     eval_expr env { e_loc = e.e_loc; e_desc = Ast_utils.switch_as_if ec cases; }

(* This expression is either a variable or a pointer *)
and eval_lval env lval =
  match lval with
  | Id v -> return (v, [])
  | ArrayId (id, es) ->
     let l =
       Lwt_list.map_s
         (fun e -> eval_expr env e >>= fun (_, v) -> return v)
         es
     in
     l >>=
       fun l ->
       let as_int = function
         | Immediate (VInt (Some i)) -> i
         | _ -> assert false
       in
       let p = List.map as_int l in
       return (id, p)

and eval_log env loc op e1 e2 =
  let do_v1 = fun () -> eval_expr env e1 >>= fun (_e, v) -> return v
  and do_v2 = fun () -> eval_expr env e2 >>= fun (_e, v) -> return v in
  let strip_bool = function
    | Immediate (VBool (Some b)) -> b
    | _ ->
       error "Cannot apply %s (line %a): bad types or unitialized value"
             (string_of_log_op op)
             Location.pp_lines loc;
       assert false
  in
  let bval =
    match op with
    | Band ->
       do_v1 () >>= fun v1 ->
       let v = strip_bool v1 in
       if v then do_v2 () >>= fun v2 -> return (strip_bool v2)
       else Lwt.return_false
    | Bor ->
       do_v1 () >>= fun v1 ->
       let v = strip_bool v1 in
       if v then Lwt.return_true
       else do_v2 () >>= fun v2 -> return (strip_bool v2)
    | Bxor ->
       do_v1 () >>=
         fun v1 -> do_v2 () >>= fun v2 ->
       let ev = (fun x y -> (x && not y) || (not x && y)) in
       return (ev (strip_bool v1) (strip_bool v2))
  in bval >|= mk_bool


and eval_rel env loc op e1 e2 =
  debug "Eval %a and %a in %a"
        Pp.pp_expr e1 Pp.pp_expr e2 Base.ValEnv.pp env;
  eval_expr env e1 >>= fun (env, v1) ->
  eval_expr env e2 >>= fun (_, v2) ->
  let ev_op () =
    match op with
    | NotEq -> (<>)
    | Eq -> (=)
    | Gt -> (>)
    | Gte -> (>=)
    | Lt -> (<)
    | Lte -> (<=)
  in

  match TM.strip_immediate v1, TM.strip_immediate v2 with
  | VBool (Some b1), VBool (Some b2) ->
     return (mk_bool ((ev_op ()) b1 b2))
  | VString (Some s1), VString (Some s2) ->
     return (mk_bool (ev_op() s1 s2))
  | VInt (Some i1), VInt (Some i2) ->
     return (mk_bool ((ev_op ()) i1 i2))
  | VFloat (Some f1), VFloat (Some f2) ->
     return (mk_bool ((ev_op ()) f1 f2))
  (* Integers promoted to float if possible *)
  | VFloat (Some f1), VInt (Some i2) ->
     return (mk_bool ((ev_op ()) f1 (float i2)))
  | VInt (Some i1), VFloat (Some f2) ->
     return (mk_bool ((ev_op ()) (float i1) f2))
  | _, _ ->
     Io.error "Cannot apply %s: bad types on line %a: %a, %a"
              (Ast_utils.string_of_rel_op op)
              Location.pp_lines loc
              TM.pp_mvalue v1
              TM.pp_mvalue v2
    ;
     exit 3;


and eval_ulog env loc op e =
  match op with
  | Bnot ->
     begin
       eval_expr env e >>= fun (_, v) ->
       match strip_immediate v with
       | VBool (Some b) -> return (mk_bool (not b))
       | _ -> Io.fail loc "Cannot apply unary boolean operator"
     end

and eval_uarith env loc op e =
  match op with
  | UMinus ->
     begin
       eval_expr env e >>= fun (_, v) ->
       match strip_immediate v with
       | VInt (Some i) -> return (mk_int (- i))
       | VFloat (Some f) -> return (mk_float (-. f))
       | _ -> Io.fail loc "Cannot apply unary arithmetic operator"
     end

and eval_arith env loc op e1 e2 =
     eval_expr env e1 >>= fun (_, v1) ->
     eval_expr env e2 >>= fun (_, v2) ->
     begin
       match strip_immediate v1, strip_immediate v2 with
       | VInt (Some i1), VInt (Some i2) ->
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
       | VFloat (Some f), VInt (Some i) ->
          begin
            try
              let g = float_op op in
              return (mk_float (g f (float i)))
            with UndefinedOperation ->
                 Io.fail loc "Cannot apply this operator"
          end
       | VInt (Some i), VFloat (Some f)  ->
          begin
            try
              let g = float_op op in
              return (mk_float (g (float i) f))
            with UndefinedOperation ->
                 Io.fail loc "Cannot apply this operator"
          end
       | VFloat (Some f1), VFloat (Some f2) ->
          begin
            try
              let g = float_op op in
              return (mk_float (g f1 f2))
            with UndefinedOperation ->
                 Io.fail loc "Cannot apply this operator"
          end
       | VString (Some s1), VString (Some s2) ->
          begin
            return
              (match op with
               | Plus -> mk_string (String.concat "" [s1; s2])
               | _ -> assert false (* Typing must prevent this *)
              )
          end
       | _, _ ->
          begin
             debug
              "Binary operator applied to %a:%a and %a:%a@."
              pp_mvalue v1 pp_ty v1 pp_mvalue v2 pp_ty v2;
              assert false; (* Typer must prevent this *)
          end
     end


and eval_call loc fname env eargs =
  trace "%s -> %s" env.current_f fname;
  if Builtins.is_builtin fname then (
    let def = Builtins.find_fundef fname in
    let args =
      match def.p_args with
      | SRep SVal ->
         Lwt_list.map_s
           (fun a -> eval_expr env a >>= fun (_, v) -> return (AVal v))
           eargs

      | SRep SName ->
         let lvals = List.map expr_to_lval eargs in
         Lwt_list.map_s
           (fun lval ->
            eval_lval env lval >>=
              fun (basename, offsets) ->
              let v = ValEnv.find env basename offsets in
              debug "%s[%a] -> %a@."
                    basename
                    (Utils.pp_list ~sep:"," Format.pp_print_int) offsets
                    pp_mvalue v;
              return (ARef (basename, offsets, v)))
           lvals

      | SVal ->
         assert (List.length eargs >= 1);
         (*List.iter
           (fun a -> Format.printf "%a; " pp_val (snd (eval_expr env a)))
           eargs;*)
         Lwt_list.map_s
           (fun a -> eval_expr env a >>= fun (_, v) -> return (AVal v))
           eargs

      | _ -> assert false
    in
    let res = def.p_eval env args in
    trace "%s <- %s" env.current_f fname;
    res
  )
  else try
      let fdef = Hashtbl.find functions fname in
      let formals = fdef.fun_formals in
      let byrefs = by_refs formals in
      debug "C %a@." ValEnv.pp env ;
      (* Adds binding from values to formals *)
      let f_param_env =
        Lwt_list.fold_left_s
          (fun mem (argname, argexpr) ->
           eval_expr env argexpr >>=
             fun (_, v) ->
             debug "Binding %s to %a" argname TM.pp_mvalue v;
             return (TM.update_named argname [] v mem)
          ) TM.empty (Utils.zip (List.map get_formal_name formals) eargs)
      in
      f_param_env >>= fun f_param_env ->
      let f_local_env =
          Lwt_list.fold_left_s
            (fun mem v -> return (TM.inject v.var_id v.var_type mem))
            f_param_env
            fdef.fun_locals
      in
      f_local_env >>= fun f_local_env ->
        eval_exprs
          { env with locals = f_local_env; current_f = fname;}
          fdef.fun_body
      >>= fun (fenv, fvalue) ->
        let locals =
          List.fold_left
            (fun lenv name ->
             TM.update_named name [] (ValEnv.find fenv name []) lenv)
            env.locals byrefs
        in
        let renv =
          { current_f = env.current_f; globals = fenv.globals; locals ; }
        in
        trace "%s <- %s" env.current_f fname;
        return (renv, fvalue)
    with Not_found -> Io.fail loc "Unknown function name"

and eval_exprs env exprs =
  let einit = Lwt.return (env, mk_unit ()) in
  List.fold_left
    (fun ev expr -> ev >>= fun (env, _v) -> eval_expr env expr) einit exprs
;;

let mk_declarations vardecls =
  let globals =
    List.fold_left
      (fun mem v -> TM.inject v.var_id v.var_type mem)
      TM.empty
      vardecls
  in { current_f = "main"; globals; locals = TM.empty; }
;;

let init_functions fundefs =
  List.iter
    (fun fdef -> Hashtbl.add functions fdef.fun_id fdef)
    fundefs
;;

let eval algo =
  debug "Starting interpretation of %s@." algo.a_id;
  init_functions algo.a_functions;
  ignore(eval_exprs (mk_declarations algo.a_variables) algo.a_body);
  (* Flush and add newline before retuning *)
  Format.printf "@.";
;;
