open Ast ;;
open Base ;;
open Base.Types ;;
open Builtins ;;
open Io ;;

let rec unify loc lty rty =
  match lty, rty with
  | TyInt, TyInt -> TyInt
  (* Integer types are promoted to floating points *)
  | TyReal, TyReal
  | TyInt, TyReal
  | TyReal, TyInt -> TyReal
  | TyBool, TyBool -> TyBool
  | TyString, TyString -> TyString
  | TyArrow (tyargs1, tyret1), TyArrow (tyargs2, tyret2) ->
     TyArrow (List.map2 (unify loc) tyargs1 tyargs2, unify loc tyret1 tyret2)
  | TyUnit, TyUnit -> TyUnit
  | TyAny, _ -> rty
  | _, TyAny -> lty
  | TyArray (_, _, t1), TyArray (_, _, t2) ->
     TyArray (0, 0, unify loc t1 t2)
  | _, _ ->
     Io.error "Types do not match. Cannot unify %a and %a on line %a@."
                  Types.pp lty Types.pp rty
                  Location.pp_lines loc;
     exit 1;
;;


let rec get_base_type n ty =
  if n = 0 then ty
  else
    match ty with
    | TyArray (_, _, t) -> get_base_type (pred n) t
    | _ -> assert false
;;

let pass_num_only loc ty =
  match ty with
  | TyReal -> TyReal
  | TyInt -> TyInt
  | _ ->
     let msg = "This expression should be numerical." in
     fail loc msg
;;

let rec eval_expr env e =
  match e.e_desc with
  | Int _ -> env, TyInt
  | Real _ -> env, TyReal
  | String _ -> env, TyString
  | Bool _ -> env, TyBool
  | ArrayExpr (vname, eidxs) ->
     begin
       List.iter
         (fun eidx  ->
          assert ((unify eidx.e_loc (snd (eval_expr env eidx)) TyInt) = TyInt))
         eidxs;
     env, get_base_type (List.length eidxs) (Env.find vname env);
     end
  | Var vname -> env, Env.find vname env
  | UnExpr (uop, e) ->
     let env, ety = eval_expr env e in
     begin
       env,
       match uop.uop_desc with
       | ULog _ -> unify e.e_loc ety TyBool
       | UArith _ -> pass_num_only e.e_loc ety
     end
  | BinExpr (bop, e1, e2) ->
     begin
       let env, ety1 = eval_expr env e1 in
       let env, ety2 = eval_expr env e2 in
       env,
       match bop.bop_desc with
       | Arith Plus ->
          begin
            match unify e.e_loc ety1 ety2 with
            | (TyString | TyReal | TyInt) as t -> t
            | _ -> fail e.e_loc
                        "Only strings, integers and reals can be used with +."
          end
       | Arith _ -> pass_num_only e.e_loc (unify e.e_loc ety1 ety2)
       | Log _ ->
          unify e.e_loc
                (unify e1.e_loc ety1 TyBool) (unify e2.e_loc ety2 TyBool)
       | Rel _ -> (ignore(unify e.e_loc ety1 ety2); TyBool)
     end

  | Assigns (Id vname, e) ->
     let env, ety = eval_expr env e in
     let vty = Env.find vname env in
     ignore (unify e.e_loc vty ety);
     env, TyUnit

  | Assigns (ArrayId(vname, eidxs), rval) ->
     List.iter
       (fun eidx  ->
        assert ((unify eidx.e_loc (snd (eval_expr env eidx)) TyInt) = TyInt))
       eidxs;
     let bty = get_base_type (List.length eidxs) (Env.find vname env) in
     ignore (unify e.e_loc bty (snd (eval_expr env rval)));
     env, TyUnit

  | IfThenElse (econd, then_e, else_e) ->
     ignore (unify econd.e_loc (snd (eval_expr env econd)) TyBool);
     let env', ty = eval_exprs env then_e in
     if is_unit ty then eval_exprs env' else_e
     else assert false

  | While (econd, exprs)
  | Repeat (econd, exprs) ->
     ignore (unify econd.e_loc (snd (eval_expr env econd)) TyBool);
     eval_exprs env exprs

  | For (id, e1, e2, _, exprs) ->
     (* for loop is only specified for integer values *)
     let env, ety1 = eval_expr env e1 in
     let env, ety2 = eval_expr env e2 in
     let vty = Env.find id env in
     ignore (unify e1.e_loc ety1 TyInt);
     ignore (unify e2.e_loc ety2 TyInt);
     ignore (unify e.e_loc vty TyInt);
     eval_exprs env exprs

  | Call (fname, eargs) ->
     let tyret = eval_call e.e_loc fname env eargs in
     env, tyret

  | Return e -> eval_expr env e

  | Switch (ec, cases) ->
     eval_expr env { e_loc = e.e_loc; e_desc = Ast_utils.switch_as_if ec cases; }

and eval_call loc fname env args =
  let ftype =
    try
      if Env.mem fname env
      then Env.find fname env
      else (Builtins.find_fundef fname).p_type
    with Not_found ->
      let msg = Format.sprintf "Unknown function name: %s" fname in
      fail loc msg
  in match ftype with
     | TyArrow ([TyAny], tyret) -> tyret

     | TyArrow (tyargs, tyret) ->
        let mytyargs = List.map (fun e -> snd (eval_expr env e)) args in
        ignore (List.map2 (unify loc) tyargs mytyargs);
        tyret

     | _ ->
        let msg = Format.sprintf "%s is not a function" fname in
        fail loc msg

and eval_exprs env exprs =
  let env, _ =
    List.fold_left (fun (env, _ty) expr -> eval_expr env expr)
                   (env, TyUnit) exprs
  in env, TyUnit

;;


let funtype fdef =
  TyArrow ((List.map Ast_utils.get_formal_type fdef.fun_formals),
           fdef.fun_return_type)
;;


let type_function env fdef =
  let fenv =
    List.fold_left
      (fun e formal -> Env.add (Ast_utils.get_formal_name formal)
                               (Ast_utils.get_formal_type formal)
                               e
      ) env fdef.fun_formals
  in
  let fenv =
    List.fold_left
      (fun e vdecl -> Env.add vdecl.var_id vdecl.var_type e
      ) fenv fdef.fun_locals
  in
  let _, _ty = eval_exprs fenv fdef.fun_body in
(*  ignore (unify fdef.fun_loc ty fdef.fun_return_type); *)
  env, fdef.fun_return_type
;;

let fill_types program =
  let venv =
    List.fold_left (fun e v -> Env.add v.var_id v.var_type e)
                   Env.empty
                   program.a_variables
  in
  let env =
    List.fold_left
      (fun e fdef -> Env.add fdef.fun_id (funtype fdef) e)
      venv program.a_functions
  in List.iter (fun x -> ignore (type_function env x)) program.a_functions;
     env
;;

let eval algorithm =
  let initial_env = fill_types algorithm in
  ignore (eval_exprs initial_env algorithm.a_body)
;;
