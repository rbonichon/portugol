open Ast ;;
open Utils ;;
open Ast_utils ;;

(** Checks if there is any *)
module Unused = struct
  let rec eval_expr vset e =
    match e.e_desc with
    | Int _ | Bool _ | String _ | Real _ -> vset
    | Var v -> VSet.remove v vset
    | BinExpr (_, el, er) -> eval_expr (eval_expr vset el) er
    | UnExpr (_, e) -> eval_expr vset e
    | ArrayExpr (vname, eidxs) ->
       eval_exprs (VSet.remove vname vset) eidxs
    | Call (fname, eargs) ->
       List.fold_left eval_expr (VSet.remove fname vset) eargs
    | Assigns (Id _, e) -> eval_expr vset e
    | Assigns (ArrayId(_, eidxs), e) -> eval_expr (eval_exprs vset eidxs) e
    | IfThenElse (econd, then_exprs, else_exprs) ->
       eval_exprs (eval_exprs (eval_expr vset econd) then_exprs) else_exprs
    | While (econd, exprs)
    | Repeat (econd, exprs) ->
       eval_exprs (eval_expr vset econd) exprs
    | For (vname, e1, e2, _, exprs) ->
       let vset = VSet.remove vname vset in
       eval_exprs (eval_expr (eval_expr vset e1) e2) exprs
    | Return e ->
       eval_expr vset e
    | Switch (e, cases) ->
       let vset' = eval_expr vset e in
       let rec do_cases vset = function
         | [] -> vset
         | (evals, cmds) :: cases ->
            do_cases (eval_exprs (eval_exprs vset evals) cmds) cases
       in do_cases vset' cases

  and eval_exprs vset exprs =
    List.fold_left
      (fun vset expr ->
       eval_expr vset expr) vset exprs
  ;;

  let run program =
    let vset = eval_exprs
                 (declared_variables program)
                 program.a_body in
    if not (VSet.is_empty vset) then (
      Io.warning "Unused variables: %a" VSet.pp vset;
    )
  ;;

end


module Undeclared = struct
  let undeclared = Hashtbl.create 7 ;;

  let check_name loc name vset =
    begin
      if not (VSet.mem name vset) && not (Builtins.is_builtin name)
      then
        try
          let locs = Hashtbl.find undeclared name in
          Hashtbl.replace undeclared name (loc :: locs)
        with
        | Not_found -> Hashtbl.add undeclared name [loc];
    end;
    vset
  ;;

  let rec eval_expr vset e =
    match e.e_desc with
    | Int _ | Bool _ | String _ | Real _ -> vset
    | Var v -> check_name e.e_loc v vset
    | BinExpr (_, el, er) -> eval_expr (eval_expr vset el) er
    | UnExpr (_, e) -> eval_expr vset e
    | ArrayExpr (vname, eidxs) ->
       eval_exprs (check_name e.e_loc vname vset) eidxs
    | Assigns (Id name, e) ->
       eval_expr (check_name e.e_loc name vset) e
    | Assigns (ArrayId(aname, eidxs), e) ->
       eval_expr (eval_exprs (check_name e.e_loc aname vset) eidxs) e
    | IfThenElse (econd, then_exprs, else_exprs) ->
       eval_exprs (eval_exprs (eval_expr vset econd) then_exprs) else_exprs
    | While (econd, exprs)
    | Repeat (econd, exprs) ->
       eval_exprs (eval_expr vset econd) exprs
    | For (idname, e1, e2, _, exprs) ->
       eval_exprs (eval_expr
                    (eval_expr (check_name e.e_loc idname vset) e1) e2) exprs
    | Call (fname, eargs) ->
       eval_call e.e_loc vset fname eargs
    | Return e ->
       eval_expr vset e
    | Switch (e, cases) ->
       let vset = eval_expr vset e in
       eval_cases vset cases

and eval_cases vset cases =
  List.fold_left
    (fun vset (conds, cmds) ->
     eval_exprs (eval_exprs vset conds) cmds
    ) vset cases

 and eval_call loc vset fname eargs =
    List.fold_left eval_expr (check_name loc fname vset) eargs

  and eval_exprs vset exprs =
    List.fold_left
      (fun vset expr ->
       eval_expr vset expr) vset exprs
  ;;

  let eval_function env fundef =
    let fenv =
      VSet.union
        env
        (VSet.union
           (VSet.of_list (List.map Ast_utils.get_var_id fundef.fun_locals))
           (VSet.of_list
              (List.map get_formal_name fundef.fun_formals))) in
    ignore (eval_exprs fenv fundef.fun_body)


  let run program =
    Hashtbl.clear undeclared ;
    let env =
      VSet.union (declared_variables program ) (declared_functions program) in
    List.iter (eval_function env) program.a_functions;
    ignore (eval_exprs env program.a_body);


    if not (Hashtbl.length undeclared = 0) then (
      Io.error "@[<v 4>Undeclared variable(s):@ %a@]@."
      (fun ppf htbl ->
       Hashtbl.iter
         (fun vname locs ->
          let vhdr = Format.sprintf "%s on line(s) " vname in
          let rec pp_locs ppf = function
            | [loc] -> Format.fprintf ppf "%a@ " Location.pp_lines loc
            | loc :: locs ->
               Format.fprintf ppf "%a,@ %a" Location.pp_lines loc pp_locs locs
            | [] -> assert false
          in
          Format.fprintf ppf "@[<hov %d>%s %a@]@ "
                         (String.length vhdr) vhdr pp_locs (List.rev locs);
         ) htbl)
        undeclared;
      exit 3;
    )
  ;;

end
