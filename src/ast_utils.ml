open Utils ;;
open Ast ;;
exception AlreadyDeclared of string ;;

let as_string_set to_string elements =
  List.fold_left
    (fun vset v ->
     let vname = to_string v in
     if VSet.mem vname vset then raise (AlreadyDeclared vname)
     else VSet.add vname vset)
    VSet.empty elements
;;

let declared_variables program =
  as_string_set (fun v -> v.var_id) program.a_variables
;;

let declared_functions program =
  as_string_set (fun f -> f.fun_id) program.a_functions
;;

let local_variables fundef =
  as_string_set (fun v -> v.var_id) fundef.fun_locals
;;

let get_fundef program fname =
  List.find (fun fdef -> fname = fdef.fun_id) program.a_functions
;;

let get_var_id v = v.var_id ;;

let var_name e =
  match e with
  | Var v -> v
  | _ -> assert false
;;

let extract_formal_var varg =
    match varg with
    | ByRef x -> x
    | ByValue x -> x
;;

let get_formal_name varg = get_var_id (extract_formal_var varg) ;;

let get_formal_type varg = (extract_formal_var varg).var_type ;;

let is_by_ref = function
  | ByValue _ -> false
  | ByRef _ -> true
;;

let by_refs vargs =
  List.map get_formal_name (List.filter is_by_ref vargs) ;;

let mk_expr loc e = { e_desc = e; e_loc = loc;} ;;
let mk_bop loc b = { bop_desc = b; bop_loc = loc; } ;;


let sub_blocks = function
  | Int _ | Real _ | String _ | Var _ | Bool _
  | Assigns _
  | Call _
  | BinExpr _
  | UnExpr _
  | ArrayExpr _
  | Return _ -> []
  | IfThenElse (_, then_exprs, else_exprs) ->
     [ then_exprs; else_exprs ]
  | Repeat (_, b)
  | While (_, b)
  | For (_, _, _, _, b) -> [ b ]
  | Switch (_, cases) ->
     List.map snd cases
;;

let has_sub_subblocks e = sub_blocks e <> [] ;;

open Format ;;

let string_of_rel_op = function
  | Gt -> ">"
  | Gte -> ">="
  | Lt -> "<"
  | Lte -> "<="
  | Eq -> "="
  | NotEq -> "<>"
;;

let string_of_arith_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Div -> "/"
  | Mult -> "*"
  | EDiv -> "\\\\"
  | Mod -> "%"
;;

let string_of_log_op = function
  | Band -> "e"
  | Bor -> "ou"
  | Bxor -> "xou"
;;

let string_of_bop  = function
  | Rel rop -> string_of_rel_op rop
  | Log lop -> string_of_log_op lop
  | Arith aop -> string_of_arith_op aop
;;

let pp_bop fmt bop = fprintf fmt "%s" (string_of_bop bop.bop_desc)
;;

let pp_uop fmt uop =
  match uop.uop_desc with
  | ULog Bnot -> fprintf fmt "nao"
  | UArith UMinus -> fprintf fmt "-"
;;

let rec pp_lval fmt = function
  | Id name -> Format.fprintf fmt "%s" name
  | ArrayId (aname, es) ->
     fprintf fmt "%s[%a]" aname (pp_exprs ",") es

and pp_expr fmt e =
  match e.e_desc with
  | Int i -> fprintf fmt "%d" i
  | Real r -> fprintf fmt "%.5f" r
  | String s -> fprintf fmt "''%s''" s
  (* Two simple ' are used instead of "" because of a graphviz/dot related
     problem. It doesn't like "". Weird ... *)
  | Var s  -> fprintf fmt "%s" s
  | Bool b ->
     fprintf fmt "%s" (if b then "verdadeiro" else "falso")
  | Call (fname, eargs) ->
     fprintf fmt "%s (%a)" fname (pp_exprs ", ") eargs
  | Assigns (lval, e) ->
     Format.fprintf fmt "@[<hov 1>%a <-@ %a@]" pp_lval lval pp_expr e
  | While (e, es) ->
     fprintf fmt "@[<v 0>@[<v 4>@[<hov 2>enquanto@ %a@ faca@]@ %a@]\
                  @ fimenquanto@]"
             pp_expr e (pp_exprs "@ ") es
  | Repeat (e, es) ->
     fprintf fmt "@[<v 0>@[<v 4>repita@ %a@]@ ate %a@]"
             pp_expr e (pp_exprs "@ ") es
  | For (ename, einit, estop, 1, exprs) ->
     fprintf fmt "para %s de %a ate %a faca %a fimpara"
             ename pp_expr einit pp_expr estop (pp_exprs "@ ") exprs
  | For (ename, einit, estop, d, exprs) ->
     fprintf fmt "para %s de %a ate %a passo %d faca %a fimpara"
             ename pp_expr einit pp_expr estop d (pp_exprs "@ ") exprs

  | Return e -> fprintf fmt "retorne %a" pp_expr e
  | IfThenElse (econd, ethens, []) ->
     fprintf fmt
             "@[<v 0>se %a@ @[<v 4>entao@ %a@]@ fimse@]"
             pp_expr econd (pp_exprs "@ ") ethens
  | IfThenElse (econd, ethens, eelses) ->
     fprintf fmt
             "@[<v 0>se %a@ @[<v 4>entao@ %a@]@ \
              @[<v 4>senao@ %a@]@ \
              fimse@]"
             pp_expr econd (pp_exprs "@ ") ethens (pp_exprs "@ ") eelses
  | ArrayExpr (aname, es) ->
     fprintf fmt "%s[%a]" aname (pp_exprs ",") es
  | BinExpr (bop, e1, e2) ->
     fprintf fmt "@[<hov 2>%a@ %a@ %a@]"
             pp_expr e1 pp_bop bop pp_expr e2
  | UnExpr (uop, e) ->
     fprintf fmt "%a %a" pp_uop uop pp_expr e
  | Switch _ -> assert false

and pp_exprs sep fmt exprs =
  let rec pp_aux fmt = function
    | [] -> ()
    | [e] -> fprintf fmt "%a" pp_expr e
    | e :: es -> fprintf fmt "%a%s%a" pp_expr e sep pp_aux es
  in fprintf fmt "%a" pp_aux exprs
;;

let pp_vardecls fmt vardecls =
  Format.fprintf fmt "@[<v 0>";
  List.iter
    (fun v -> fprintf fmt "%s : %a@ " v.var_id Base.Types.pp v.var_type)
    vardecls;
  Format.fprintf fmt "@]";
;;

let pp_program fmt program =
  fprintf fmt "@[<v 0>\
               algoritmo \"%s\"@ \
               var @[<v 0>%a@]\
                   @[<v 4>@ %a@]@ \
               fimalgoritmo@]@."
          program.a_id
          pp_vardecls program.a_variables
          (pp_exprs "@ ") program.a_body
;;


(* Get a set of function calls made by this expression *)
let get_fcalls e =
  let rec aux vset e =
    match e.e_desc with
        | Int _
        | Real _
        | String _
        | Bool _
        | Var _ -> vset
        | Return e
        | Assigns (_, e)
        | UnExpr (_, e) -> aux vset e
        | BinExpr (_, e1, e2) -> aux (aux vset e1) e2
        | ArrayExpr (_, es)  -> aux_list vset es
        | IfThenElse (e1, e2s, e3s) ->
           aux (aux_list (aux_list vset e2s) e3s) e1
        | While (e, es)
        | Repeat (e, es) -> aux (aux_list vset es) e
        | For (_, einit, eend, _, es) ->
           aux (aux (aux_list vset es) eend) einit
        | Call (fname, es) ->
           aux_list (VSet.add fname vset) es
        | Switch (e, scases) ->
           let vset' =
             List.fold_left
               (fun vset (e1s, e2s) -> aux_list (aux_list vset e1s) e2s)
               vset scases
           in aux vset' e

  and aux_list vset es = List.fold_left aux vset es in
  aux VSet.empty e
;;

let mk_for_cond loc vname e1 e2 =
  let mk_bexp loc bexp e1_desc e2_desc =
    { e_loc = loc ;
      e_desc = BinExpr( bexp,
                       { e_loc = loc; e_desc = e1_desc; },
                       { e_loc = loc; e_desc = e2_desc; }
                      )
    } in
  let sup = { bop_loc = loc; bop_desc = Rel Gte; }
  and inf = { bop_loc = loc; bop_desc = Rel Lte; }
  and band = { bop_loc = loc; bop_desc = Log Band; }
  and v = Var vname
  in
  { e_loc = loc ;
    e_desc = BinExpr(band,
                     mk_bexp e1.e_loc sup v e1.e_desc,
                     mk_bexp e2.e_loc inf v e2.e_desc
                    );
  }
;;


let switch_as_if e cases =
  let mk_eq expr =
    let loc = expr.e_loc in
    { e_loc = loc;
      e_desc = BinExpr({ bop_loc = loc; bop_desc = Rel Eq; }, e, expr); }
  in
  let rec mk_cond = function
    | [] -> assert false
    | [expr] -> mk_eq expr
    | expr :: exprs ->
       let loc = expr.e_loc in
       let eq1 = mk_eq expr in
       { e_loc = loc;
         e_desc = BinExpr({ bop_loc = loc; bop_desc = Log Bor},
                          eq1, mk_cond exprs
                         ) }
  in
  let rec mk_switch else_opt = function
    | [] -> (get_opt else_opt)
    | (econds, es) :: cases ->
       begin match else_opt with
             | None -> mk_switch (Some es) cases
             | Some else_es ->
                let eqcond = mk_cond econds in
                mk_switch
                  (Some [
                       { e_loc = eqcond.e_loc;
                         e_desc = IfThenElse (eqcond, es, else_es); }]
                  )
                  cases
       end
  in
  match mk_switch None (List.rev cases) with
  | [e] -> e.e_desc
  | _ -> assert false
;;


let for_as_while e =
  match e.e_desc with
  | For (vname, einit, eend, step, body) ->
     let loc = e.e_loc in
     let cinit =
       { e_loc = Location.dummy_loc;
         e_desc = Assigns (Id vname, einit);
       }
     in
     let var_e = { e_loc = loc; e_desc = Var vname; } in
     let comp_e =
       { e_loc = loc;
         e_desc =
           BinExpr( { bop_loc = loc; bop_desc = Rel Lte; }, var_e, eend);
       }
     in
     let incr_e =
       { e_loc = loc;
         e_desc = BinExpr(
                      { bop_loc = loc; bop_desc = Arith Plus; },
                      var_e,
                      { e_loc = loc; e_desc = Int step });
       }
     in
     let incr_cmd =
       { e_loc = loc;
         e_desc = Assigns (Id vname, incr_e);
       }
     in
     let while_cmd =
       { e_loc = loc;
         e_desc = While (comp_e, body @ [incr_cmd]);
       }
     in cinit :: [while_cmd]
  | _ -> assert false
;;
