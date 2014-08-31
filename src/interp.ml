open Ast
open Ast_utils
open Base
open Base.Types
open Base.Values
open Base.Values.ValEnv
open Builtins

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
 (* exception UnitializedVariable ;;*)

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

let rec eval_expr env e =
  match e.e_desc with
  | Int i ->
     Lwt.return (env, mk_int i)
  | Real f -> return (env, mk_float f)
  | String s -> return (env, mk_string s)
  | Bool b -> return (env, mk_bool b)
  | ArrayExpr (vname, es) ->
     begin
     try (
       let rec eval_array aval es =
         match aval, es  with
         | VArray (fidx, lidx, a), [e] ->
            eval_expr env e >>=
              fun (env, v) ->
              let idx =
                match v with
                | VInt (Some i) -> i
                | _ -> assert false
              in
              debug "Accessing array(%d, %d) at %d" fidx lidx idx;
              assert(idx >= fidx && idx <= lidx);
              return (env, a.(idx))
         | VArray (fidx, lidx, varr), e :: es ->
            eval_expr env e >>=
                fun (_env, v) ->
              let idx =
                match v with
                | VInt (Some i) -> i
                | _ -> assert false
              in
              debug "Accessing array(%d, %d) at %d" fidx lidx idx;
              assert(idx >= fidx && idx <= lidx);
              eval_array (varr.(idx)) es
         | _ -> assert false
       in eval_array (ValEnv.find env vname) es
     )
     with Not_found ->
       let msg = Format.sprintf "Unbound variable: %s" vname in
       Io.fail e.e_loc msg
     end
  | Var v ->
     begin
       try
         let vval = try ValEnv.find env v with Not_found -> find_constant v in
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
  | Assigns (Id vname, e) ->
     eval_expr env e >>=
       fun (env, v) ->
       debug "Assigns new value for %s: %a from %a@."
           vname pp_val v
           Ast_utils.pp_expr e
       ;
       return (ValEnv.add env vname v, VUnit)
  | Assigns (ArrayId(vname, eidxs), e) ->
     begin
       let rec set_array aval eidxs =
         match eidxs, aval with
         | [eidx], VArray(idx1, idx2, a) ->
            begin
              eval_expr env eidx >>=
                fun (env, idx) ->
              begin
                match idx with
                | VInt (Some i) ->
                   if (idx1 <= i) && (i <= idx2) then (
                     eval_expr env e >>=
                       fun (env, v) ->
                       a.(i) <- v;
                       return (env, VUnit)
                   )
                   else (
                     let msg =
                       Format.sprintf
                         "Out of bounds access %d on array %s[%d..%d]"
                         i vname idx1 idx2
                     in
                     Error.errloc eidx.e_loc msg;
                   )
                | _ ->
                   let msg = Format.sprintf "This expression should be an integer."
                   in Error.errloc eidx.e_loc msg;
              end
            end
         | eidx :: eidxs,  VArray(idx1, idx2, a) ->
            begin
            eval_expr env eidx >>=
              fun (_env, idx) ->
              begin
                match idx with
                | VInt (Some i) ->
                   if (idx1 <= i) && (i <= idx2) then set_array (a.(i)) eidxs
                   else (
                     let msg =
                       Format.sprintf
                         "Out of bounds access %d on array %s[%d..%d]"
                         i vname idx1 idx2
                     in
                     Error.errloc eidx.e_loc msg;
                   )
                | _ ->
                   let msg = Format.sprintf "This expression should be an integer."
                   in Error.errloc eidx.e_loc msg;
              end
            end
         | _, _ -> assert false
       in set_array  (ValEnv.find env vname) eidxs
     end

  | IfThenElse (cond, then_exprs, else_exprs) ->
     begin
      eval_expr env cond >>= fun (env, v) ->
      match v with
      | VBool (Some b) ->
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
       | VBool (Some b) ->
          if b then
            eval_exprs env exprs >>= fun (env, _) -> eval_expr env e
          else return (env, VUnit)
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
         | VBool (Some b) ->
            if not b then eval_expr env e
            else return (env', VUnit)
         | _ ->
            let msg = "This expression should be boolean." in
            Error.errloc e.e_loc msg;
     end

  | For (id, e1, e2, step, exprs) ->
     eval_expr env e1 >>= fun (_, init_e) ->
     let env' = ValEnv.add env id init_e in
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
     eval_call e.e_loc fname env eargs

  | Ast.Return e ->
     eval_expr env e

  | Switch (ec, cases) ->
     eval_expr env { e_loc = e.e_loc; e_desc = Ast_utils.switch_as_if ec cases; }

and eval_log env loc op e1 e2 =
  let do_v1 = fun () -> eval_expr env e1 >>= fun (_e, v) -> return v
  and do_v2 = fun () -> eval_expr env e2 >>= fun (_e, v) -> return v in
  let strip_bool = function
    | VBool (Some b) -> b
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
        pp_expr e1 pp_expr e2 Base.Values.ValEnv.pp env;
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
  match v1, v2 with
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
              Base.Values.pp_val v1
              Base.Values.pp_val v2
    ;
     exit 3;


and eval_ulog env loc op e =
  match op with
  | Bnot ->
     begin
       eval_expr env e >>= fun (_, v) ->
       match  v with
       | VBool (Some b) -> return (mk_bool (not b))
       | _ -> Io.fail loc "Cannot apply unary boolean operator"
     end

and eval_uarith env loc op e =
  match op with
  | UMinus ->
     begin
       eval_expr env e >>= fun (_, v) ->
       match v with
       | VInt (Some i) -> return (mk_int (- i))
       | VFloat (Some f) -> return (mk_float (-. f))
       | _ -> Io.fail loc "Cannot apply unary arithmetic operator"
     end

and eval_arith env loc op e1 e2 =
     eval_expr env e1 >>= fun (_, v1) ->
     eval_expr env e2 >>= fun (_, v2) ->
     begin
       match v1, v2 with
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
       | _, _ ->
          begin
            debug
              "Binary operator applied to %a:%s and %a:%s@."
              Base.Values.pp_val v1
              (Base.Values.to_string v1)
              Base.Values.pp_val v2
              (Base.Values.to_string v2);
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
         Lwt_list.map_s
           (fun a -> eval_expr env a >>=
                       fun (_, v) -> return (ARef (var_name a.e_desc, v)))
           eargs

      | SVal ->
         assert (List.length eargs >= 1);
         (*List.iter
           (fun a -> Format.printf "%a; " pp_val (snd (eval_expr env a)))
           eargs;*)
         eval_expr env (List.hd eargs) >>=
           fun (_, v) -> return ([AVal v])
      | _ -> assert false
    in
    trace "%s <- %s" env.current_f fname;
    def.p_eval env args
  )
  else try
      let fdef = Hashtbl.find functions fname in
      let formals = fdef.fun_formals in
      let byrefs = by_refs formals in
      debug "%a@." Base.Values.ValEnv.pp env ;
      let f_local_env =
        Lwt_list.fold_left_s
          (fun lenv (argname, argexpr) ->
           eval_expr env argexpr >>=
             fun (_, v) ->
             debug "Binding %s to %a" argname Base.Values.pp_val v;
             return (Env.add argname v lenv)
          ) Env.empty (Utils.zip (List.map get_formal_name formals) eargs)
      in
      f_local_env >>= fun f_local_env ->
        eval_exprs
          { env with locals = f_local_env; current_f = fname;}
          fdef.fun_body
      >>= fun (fenv, fvalue) ->
      let locals =
        List.fold_left
          (fun lenv name -> Env.add name (ValEnv.find fenv name) lenv)
          env.locals byrefs
      in
      let renv = { current_f = env.current_f; globals = fenv.globals; locals; }
      in
      trace "%s <- %s" env.current_f fname;
      return (renv, fvalue)
  with Not_found -> Io.fail loc "Unknown function name"

and eval_exprs env exprs =
  let einit = Lwt.return (env, VUnit) in
  List.fold_left
    (fun ev expr -> ev >>= fun (env, _v) -> eval_expr env expr) einit exprs
;;

let mk_default_val_from_type v =
  let rec aux = function
    | TyInt -> VInt None
    | TyReal -> VFloat None
    | TyString -> VString None
    | TyBool -> VBool None
    | TyArray (idx1, idx2, ty) ->
       if (idx1 < 0) || (idx2 < idx1) then
         Io.fail v.var_loc "Index ranges should be positive and in order."
       else VArray(idx1, idx2, Array.init (idx2 + 1) (fun _ -> aux ty))
    | _ -> assert false
  in aux v.var_type

let mk_declarations vardecls =
  let globals =
    List.fold_left
      (fun env v -> Env.add v.var_id (mk_default_val_from_type v) env)
      Env.empty
      vardecls
  in { current_f = "principal"; globals; locals = Env.empty; }
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
