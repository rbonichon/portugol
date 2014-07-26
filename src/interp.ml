open Ast
open Ast_utils
open Base
open Base.Types
open Base.Values
open Base.Values.ValEnv
open Builtins
open Io
;;

let trace =
  let trace_out = default_out "trace" in
  fun txt -> (
    if Driver.get_tracing () then
      Io.glog trace_out txt
    else Format.ifprintf Format.std_formatter txt
  )
;;


let functions = Hashtbl.create 7 ;;

exception UndefinedOperation ;;
 (* exception UnitializedVariable ;;*)

let float_op = function
  | Mult -> (fun x y -> x *. y)
  | Plus -> (+.)
  | Minus -> (-.)
  | Div -> (/.)
  | EDiv | Mod -> raise UndefinedOperation
;;

let rec eval_expr env e =
  match e.e_desc with
  | Int i -> env, mk_int i
  | Real f -> env, mk_float f
  | String s -> env, mk_string s
  | Bool b -> env, mk_bool b
  | ArrayExpr (vname, e) ->
     begin
     try (
       match ValEnv.find env vname with
       | VArray (fidx, lidx, a) ->
              begin
                let env, v =  eval_expr env e in
                let idx =
                  match v with
                  | VInt (Some i) -> i
                  | _ -> assert false
                in
                debug "Accessing array(%d, %d) at %d" fidx lidx idx;
                assert(idx >= fidx && idx <= lidx);
                env, a.(idx)
          end
       | _ -> assert false
     )
     with Not_found ->
       let msg = Format.sprintf "Unbound variable: %s" vname in
       fail e.e_loc msg
     end
  | Var v ->
     begin
       try
         let vval = try ValEnv.find env v with Not_found -> find_constant v in
         env, vval
       with
       | Not_found ->
          let msg = Format.sprintf "Unbound variable: %s" v in
          fail e.e_loc msg

     end
  | UnExpr (uop, e) ->
     begin
       env,
       match uop.uop_desc with
       | ULog op -> eval_ulog env uop.uop_loc op e
       | UArith op -> eval_uarith env uop.uop_loc op e
     end
  | BinExpr(bop, e1, e2) ->
     begin
       env,
       match bop.bop_desc with
       | Arith op -> eval_arith env bop.bop_loc op e1 e2
       | Log op -> eval_log env bop.bop_loc op e1 e2
       | Rel op -> eval_rel env bop.bop_loc op e1 e2
     end
  | Assigns (Id vname, e) ->
     let env, v = eval_expr env e in
     debug "Assigns new value for %s: %a from %a"
           vname pp_val v
           Ast_utils.pp_expr e
     ;
     ValEnv.add env vname v, VUnit
  | Assigns (ArrayId(vname, eidx), e) ->
     begin
     let env, idx = eval_expr env eidx in
     let varr = ValEnv.find env vname in
     match idx, varr with
     | VInt (Some i), VArray(idx1, idx2, a) ->
        if (idx1 <= i) && (i <= idx2) then (
          let env, v = eval_expr env e in
          a.(i) <- v;
          env, VUnit
        )
        else (
          let msg =
            Format.sprintf
              "Out of bounds access %d on array %s[%d..%d]"
                           i vname idx1 idx2
          in
          Error.errloc eidx.e_loc msg;
        )
     | _, _ ->
          let msg =
            Format.sprintf "This expression should be an integer."
          in Error.errloc eidx.e_loc msg;
     end

  | IfThenElse (cond, then_exprs, else_exprs) ->
     begin
       let env, v = eval_expr env cond in
     match v with
      | VBool (Some b) ->
        let exprs = if b then then_exprs else else_exprs in
        eval_exprs env exprs
       | _ ->
        let msg = "This expression should be boolean." in
        Error.errloc cond.e_loc msg;
     end

  | While (econd, exprs) ->
     let env, v = eval_expr env econd in
     begin
        match  v with
       | VBool (Some b) ->
          if b then
            let env, _ = eval_exprs env exprs in
            eval_expr env e
          else env, VUnit
       | _ ->
          let msg = "This expression should be boolean." in
          Error.errloc e.e_loc msg;
     end

  | Repeat (econd, exprs) ->
     begin
       let env', _v = eval_exprs env exprs in
       let env, v = eval_expr env' econd in
       match v with
       | VBool (Some b) ->
          if not b then eval_expr env e
          else env', VUnit
       | _ ->
          let msg = "This expression should be boolean." in
          Error.errloc e.e_loc msg;
     end

  | For (id, e1, e2, step, exprs) ->
     let _, init_e = eval_expr env e1 in
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

  | Return e ->
     eval_expr env e

  | Switch (ec, cases) ->
     eval_expr env { e_loc = e.e_loc; e_desc = Ast_utils.switch_as_if ec cases; }

and eval_log env loc op e1 e2 =
  let do_v1 = fun () -> snd (eval_expr env e1)
  and do_v2 = fun () -> snd (eval_expr env e2) in
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
    | Band -> (strip_bool (do_v1 ())) && (strip_bool (do_v2 ()))
    | Bor -> (strip_bool (do_v1 ())) || (strip_bool (do_v2 ()))
    | Bxor ->
       let ev = (fun x y -> (x && not y) || (not x && y)) in
       ev (strip_bool (do_v1 ())) (strip_bool (do_v2 ()))
  in mk_bool bval


and eval_rel env loc op e1 e2 =
  debug "Eval %a and %a in %a"
        pp_expr e1 pp_expr e2 Base.Values.ValEnv.pp env;
  let _, v1 = eval_expr env e1
  and _, v2 = eval_expr env e2 in
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
  | VBool (Some b1), VBool (Some b2) -> mk_bool ((ev_op ()) b1 b2)
  | VInt (Some i1), VInt (Some i2) -> mk_bool ((ev_op ()) i1 i2)
  | VFloat (Some f1), VFloat (Some f2) -> mk_bool ((ev_op ()) f1 f2)
  (* Integers promoted to float if possible *)
  | VFloat (Some f1), VInt (Some i2) -> mk_bool ((ev_op ()) f1 (float i2))
  | VInt (Some i1), VFloat (Some f2) -> mk_bool ((ev_op ()) (float i1) f2)
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
       let _, v = eval_expr env e in
       match  v with
       | VBool (Some b) -> mk_bool (not b)
       | _ -> fail loc "Cannot apply unary boolean operator"
     end

and eval_uarith env loc op e =
  match op with
  | UMinus ->
     begin
       let _, v = eval_expr env e in
       match v with
       | VInt (Some i) -> mk_int (- i)
       | VFloat (Some f) -> mk_float (-. f)
       | _ -> fail loc "Cannot apply unary arithmetic operator"
     end

and eval_arith env loc op e1 e2 =
     let _, v1 = eval_expr env e1 and _, v2 = eval_expr env e2 in
     begin
       match v1, v2 with
       | VInt (Some i1), VInt (Some i2) ->
          begin
            match op with
            | Mult -> mk_int (i1 * i2)
            | Plus  -> mk_int (i1 + i2)
            | Minus -> mk_int (i1 - i2)
            | EDiv -> mk_int (i1 / i2)
            | Mod -> mk_int (i1 mod i2)
            | Div -> mk_float ((float i1) /. (float i2))
          end
       | VFloat (Some f), VInt (Some i) ->
          begin
            try
              let g = float_op op in
              mk_float (g f (float i))
            with UndefinedOperation ->
                 fail loc "Cannot apply this operator"
          end
       | VInt (Some i), VFloat (Some f)  ->
          begin
            try
              let g = float_op op in
              mk_float (g (float i) f)
            with UndefinedOperation ->
                 fail loc "Cannot apply this operator"
          end
       | VFloat (Some f1), VFloat (Some f2) ->
          begin
            try
              let g = float_op op in
              mk_float (g f1 f2)
            with UndefinedOperation ->
                 fail loc "Cannot apply this operator"
          end
       | _, _ ->
          begin
            debug
              "Binary operator applied to %a:%s and %a:%s"
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
         List.map (fun a -> AVal (snd (eval_expr env a))) eargs

      | SRep SName ->
         List.map
           (fun a -> ARef (var_name a.e_desc, snd (eval_expr env a))) eargs

      | SVal ->
         assert (List.length eargs >= 1);
         (*List.iter
           (fun a -> Format.printf "%a; " pp_val (snd (eval_expr env a)))
           eargs;*)
         [AVal (snd (eval_expr env (List.hd eargs)))]
      | _ -> assert false
    in
    trace "%s <- %s" env.current_f fname;
    def.p_eval env args
  )
  else try
      let fdef = Hashtbl.find functions fname in
      let formals = fdef.fun_formals in
      let byrefs = by_refs formals in
      let f_local_env =
        List.fold_left2
          (fun lenv argname argexpr ->
           let v = snd (eval_expr env argexpr) in
           debug "Binding %s to %a" argname Base.Values.pp_val v;
           Env.add argname v lenv
          ) Env.empty (List.map get_formal_name formals) eargs
      in
      let fenv, fvalue =
        eval_exprs
          { env with locals = f_local_env; current_f = fname;}
          fdef.fun_body in
      let locals =
        List.fold_left
          (fun lenv name -> Env.add name (ValEnv.find fenv name) lenv)
          env.locals byrefs
      in
      let renv = { current_f = env.current_f; globals = fenv.globals; locals; }
      in
      trace "%s <- %s" env.current_f fname;
      renv, fvalue
  with Not_found -> fail loc "Unknown function name"

and eval_exprs env exprs =
  List.fold_left (fun (env, _v) expr -> eval_expr env expr) (env, VUnit) exprs
;;

let mk_default_val_from_type v =
  let rec aux = function
    | TyInt -> VInt None
    | TyReal -> VFloat None
    | TyString -> VString None
    | TyBool -> VBool None
    | TyArray (idx1, idx2, ty) ->
       if (idx1 < 0) || (idx2 < idx1) then
         fail v.var_loc "Index ranges should be positive and in order."
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
  debug "Starting interpretation of %s" algo.a_id;
  init_functions algo.a_functions;
  ignore(eval_exprs (mk_declarations algo.a_variables) algo.a_body);
  (* Flush and add newline before retuning *)
  Format.printf "@.";
;;
