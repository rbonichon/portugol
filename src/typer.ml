open Ast ;;
open Types ;;
open Builtins ;;
open Io ;;

module E = Utils.SMap ;;

exception UnificationError of Location.t * Types.t * Types.t ;;

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
  | _, _ -> raise (UnificationError(loc, lty, rty))
;;

let print_and_exit_on_error loc lty rty =
  Io.error "Line %a : types do not match ! Cannot unify %a and %a@."
            Location.pp_lines loc Types.pp lty Types.pp rty ;
  exit 1;
;;

let handle_unify on_error loc lty rty =
  try unify loc lty rty
  with UnificationError (loc, lty, rty) -> on_error loc lty rty
;;

let array_assignment loc =
  Io.error "Disallowed array assignment on %a@." Location.pp_lines loc;
  exit 1;
;;

let unify_fail = handle_unify print_and_exit_on_error ;;

let rec get_base_type n ty =
  if n = 0 then ty
  else
    match ty with
    | TyArray (_, _, t) -> get_base_type (pred n) t
    | _ ->
       Io.error
         "Types do not match. Expected array, has %a@."
         Types.pp ty;
       exit 2;
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
          assert ((unify_fail eidx.e_loc (snd (eval_expr env eidx)) TyInt) = TyInt))
         eidxs;
     env, get_base_type (List.length eidxs) (E.find vname env);
     end
  | Var vname -> env, E.find vname env
  | UnExpr (uop, e) ->
     let env, ety = eval_expr env e in
     begin
       env,
       match uop.uop_desc with
       | ULog _ -> unify_fail e.e_loc ety TyBool
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
            match unify_fail e.e_loc ety1 ety2 with
            | (TyString | TyReal | TyInt) as t -> t
            | _ -> fail e.e_loc
                        "Only strings, integers and reals can be used with +."
          end
       | Arith (EDiv | Mod) ->
          let t = unify_fail e.e_loc ety1 ety2 in
          begin
            match t with
            | TyInt -> t
            | _ ->
               debug "Here@.";
               fail e.e_loc "Parameters for \\ or % must be of type inteiro"
          end
       | Arith Div ->
          let _t = unify_fail e.e_loc ety1 ety2 in
          TyReal
       | Arith (Minus | Mult) ->
          pass_num_only e.e_loc (unify_fail e.e_loc ety1 ety2)
       | Log _ ->
          unify_fail e.e_loc
                (unify_fail e1.e_loc ety1 TyBool) (unify_fail e2.e_loc ety2 TyBool)
       | Rel _ -> (ignore(unify_fail e.e_loc ety1 ety2); TyBool)
     end

  | Assigns (Id vname, e) ->
     let env, ety = eval_expr env e in
     let vty = E.find vname env in
     if is_array (unify_fail e.e_loc vty ety) then array_assignment e.e_loc
     else env, TyUnit

  | Assigns (ArrayId(vname, eidxs), rval) ->
     List.iter
       (fun eidx  ->
        assert ((unify_fail eidx.e_loc (snd (eval_expr env eidx)) TyInt) = TyInt))
       eidxs;
     let rty = snd (eval_expr env rval) in
     if is_array rty then array_assignment rval.e_loc
     else
       let expected_ty = Types.pseudo_array_type (List.length eidxs) rty in
       ignore (unify_fail e.e_loc (E.find vname env) expected_ty);
       env, TyUnit

  | IfThenElse (econd, then_e, else_e) ->
     ignore (unify_fail econd.e_loc (snd (eval_expr env econd)) TyBool);
     let env', ty = eval_exprs env then_e in
     if is_unit ty then eval_exprs env' else_e
     else assert false

  | While (econd, exprs)
  | Repeat (econd, exprs) ->
     ignore (unify_fail econd.e_loc (snd (eval_expr env econd)) TyBool);
     eval_exprs env exprs

  | For (id, e1, e2, _, exprs) ->
     (* for loop is only specified for integer values *)
     let env, ety1 = eval_expr env e1 in
     let env, ety2 = eval_expr env e2 in
     let vty = E.find id env in
     ignore (unify_fail e1.e_loc ety1 TyInt);
     ignore (unify_fail e2.e_loc ety2 TyInt);
     ignore (unify_fail e.e_loc vty TyInt);
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
      if E.mem fname env
      then E.find fname env
      else (Builtins.find_fundef fname).p_type
    with Not_found ->
      let msg = Format.sprintf "Unknown function name: %s" fname in
      fail loc msg
  in match ftype with
     | TyArrow ([TyAny], tyret) -> tyret

     | TyArrow (tyargs, tyret) ->
        let mytyargs = List.map (fun e -> snd (eval_expr env e)) args in
        ignore (List.map2 (unify_fail loc) tyargs mytyargs);
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
      (fun e formal -> E.add (Ast_utils.get_formal_name formal)
                               (Ast_utils.get_formal_type formal)
                               e
      ) env fdef.fun_formals
  in
  let fenv =
    List.fold_left
      (fun e vdecl -> E.add vdecl.var_id vdecl.var_type e
      ) fenv fdef.fun_locals
  in
  let _, _ty = eval_exprs fenv fdef.fun_body in
(*  ignore (unify_fail fdef.fun_loc ty fdef.fun_return_type); *)
  env, fdef.fun_return_type
;;

let fill_types program =
  let venv =
    List.fold_left (fun e v -> E.add v.var_id v.var_type e)
                   E.empty
                   program.a_variables
  in
  let env =
    List.fold_left
      (fun e fdef -> E.add fdef.fun_id (funtype fdef) e)
      venv program.a_functions
  in List.iter (fun x -> ignore (type_function env x)) program.a_functions;
     env
;;

let eval algorithm =
  let initial_env = fill_types algorithm in
  ignore (eval_exprs initial_env algorithm.a_body)
;;
