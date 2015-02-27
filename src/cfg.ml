(** Computes the CFG of a given [Ast.program] element
 *  using the OCamlGraph library.
 *  Outputs the dot file of the CFG.
 *)
open Ast ;;
open Utils ;;
open Ast_utils ;;

(* The type of information contained by the vertices of the CFG *)
type g_expr =
  | GInst of Ast.expr
  | GCall of Ast.expr
  | GReturn of Ast.expr
  | GChoice of Ast.expr
  | GOut of Location.t * string
  | GIn of Location.t * string
;;

type g_node = {
  g_expr : g_expr;
  g_module : string;
}

let pp fmt = function
  | GInst e
  | GReturn e
  | GChoice e
  | GCall e -> Format.fprintf fmt "%a" Ast_utils.Pp.pp_expr e
  | GIn (_, s) -> Format.fprintf fmt "in_%s" s
  | GOut (_, s) -> Format.fprintf fmt "out_%s" s
;;

let cname = function
  | GInst _ -> "ginst"
  | GCall _ -> "gcall"
  | GReturn _ -> "greturn"
  | GChoice _ -> "gchoice"
  | GOut _ -> "gout"
  | GIn _ -> "gin"
;;

let get_location = function
  | GCall e
  | GInst e
  | GReturn e
  | GChoice e -> e.e_loc
  | GIn (l, _) -> l
  | GOut (l, _) -> l
;;

let node_expr gnode =
  match gnode.g_expr with
  | GCall e
  | GInst e
  | GReturn e
  | GChoice e -> e
  | GIn _
  | GOut _ -> assert false
;;

module ComparableExpr = struct
  type t = g_node ;;

  let compare = Pervasives.compare ;;

  let hash = Hashtbl.hash ;;

  let equal x y =  compare x y = 0 ;;
end

module GLabel = struct
  type t =
    | LDefault
    | LCall
    | LOut
    | LChoice of bool
  ;;

  let compare = Pervasives.compare ;;
  let default = LDefault ;;
end

module DA = Graph.Graphviz.DotAttributes ;;
module NodeSet = Set.Make(ComparableExpr) ;;

open Lexing ;;
let column (pos:Lexing.position) = pos.pos_cnum - pos.pos_bol
;;

exception Found ;;

let fun_search p e =
  let fcalls = get_fcalls e in
  try
    VSet.iter (fun fname -> if p fname then raise Found) fcalls;
    false;
  with Found -> true
;;

let has_inputs = fun_search Builtins.is_input_builtin
let has_outputs = fun_search Builtins.is_output_builtin
;;

module G = struct
  include Graph.Persistent.Digraph.ConcreteLabeled(ComparableExpr)(GLabel);;
  open Location ;;
  let graph_attributes _ = [] ;;

  (* Default colors used in the dot output *)
  let out_color = `Color 0x0000ee
  and in_color = `Color 0xff0000
  and choice_color = `Color 0x009874
  ;;

  let vertex_name v =
    let l = get_location v.g_expr in
    let lstart = l.loc_start.pos_lnum
    and lend = l.loc_end.pos_lnum
    and cstart = column l.loc_start
    and cend = column l.loc_end in
    let s = cname v.g_expr in
    Format.sprintf "%s_%d%d%d%d" s lstart cstart lend cend
  ;;

  let default_vertex_attributes _ = [] ;;
  let vertex_attributes v =
    let shape _e =
(*         if has_inputs e then `Shape `House
         else if has_outputs e then `Shape `Invhouse
         else *) `Shape `Box
    in

    let label = Utils.sfprintf "%a" pp v.g_expr in
    match v.g_expr with
    | GChoice _ -> [`Shape `Diamond; `Label label; choice_color;]
    | GCall e ->
       [shape e; `Label label; in_color; ]
    | GInst _ -> [`Shape `Box; `Label label;]
    | GReturn e ->
       [shape e; `Label label; out_color;]
    | GIn _ -> [`Label label; `Shape `Ellipse; ]
    | GOut _ -> [`Label label; `Shape `Box; ]
  ;;


  let get_subgraph v =
    let id = v.g_module in Some
    { DA.sg_name = id;
      DA.sg_attributes = [`Label id; `Style `Solid;];
      DA.sg_parent = None;
    }
  ;;

  (* Define yes and no cases for conditionals *)
  let yes = "sim" and no = "nao" ;;
  let default_edge_attributes _ = [] ;;
  let edge_attributes e =
    match E.label e with
    | GLabel.LDefault -> []
    | GLabel.LCall -> [`Style `Dashed; in_color;]
    | GLabel.LOut -> [`Style `Dotted; out_color;]
    | GLabel.LChoice b ->
       let l = if b then yes else no in
       [`Label l; ]

  ;;

end
;;

module GDot = Graph.Graphviz.Dot(G) ;;

type fun_cfg = {
  f_id : string;
  f_out : g_node; (* One out node *)
  f_in: g_node;
}
;;

let fun_tbl = Hashtbl.create 7 ;;

(*
let update_out fname = function
  | None -> ()
  | Some outnode ->
     let fcfg = Hashtbl.find fun_tbl fname in
     Hashtbl.replace fun_tbl fname { fcfg with f_out = outnode :: fcfg.f_outs; }
;;*)

(* fun_calls: fun_name -> node set *)
let fun_calls = Hashtbl.create 37 ;;

let update_calls node call_set =
  VSet.iter
    (fun fname ->
     try let s = Hashtbl.find fun_calls fname in
         Hashtbl.replace fun_calls fname (NodeSet.add node s)
     with Not_found -> Hashtbl.add fun_calls fname (NodeSet.singleton node)
    ) call_set
;;

let add_opt_edge g label np = function
  | None -> g
  | Some n ->
     let edge = G.E.create np label n in
     G.add_edge_e g edge
;;

let add_opts_edge g n1 l n2 =
  match n1, n2 with
  | None, _
  | _, None -> g
  | Some n1', Some n2' ->
     let edge = G.E.create n1' l n2' in
     G.add_edge_e g edge
;;

open GLabel ;;

let rec eval_expr g succ_n fcfg e =
  let mk_node g g_expr succs =
    let n = { g_expr; g_module = fcfg.f_id; } in
    update_calls n (get_fcalls (node_expr n));
    List.fold_left (fun g s -> add_opt_edge g LDefault n s) g succs,
    Some n
  in
  match e.e_desc with
  | Int _
  | Real _
  | String _
  | Var _
  | Bool _
  | UnExpr _
  | BinExpr _
  | ArrayExpr _ -> assert false
  | Assigns _ -> mk_node g (GInst e) [succ_n;]
  | IfThenElse (econd, ethens, eelses) ->
     let g1, succ1 = eval_exprs g succ_n fcfg ethens in
     let g2, succ2 = eval_exprs g1 succ_n fcfg eelses in
     let g3, n = mk_node g2 (GChoice econd) [] in
     let g4 = add_opts_edge g3 n (LChoice true) succ1 in
     add_opts_edge g4 n (LChoice false) succ2, n

  | While (econd, es) ->
     let g', n = mk_node g (GChoice econd) [] in
     let g, succ = eval_exprs g' n fcfg es in
     add_opts_edge
       (add_opts_edge g n (LChoice true) succ)
       n (LChoice false) succ_n
     ,n

  | Repeat (econd, es) ->
     let g1, cnode = mk_node g (GChoice econd) [] in
     let g2, succ = eval_exprs g1 cnode fcfg es in

     add_opts_edge
       (add_opts_edge g2 cnode (LChoice false) succ)
       cnode (LChoice true) succ_n
     , succ

  | For _ ->
     eval_exprs g succ_n fcfg (Ast_utils.for_as_while e)
     (* let fcond = Ast_utils.mk_for_cond e.e_loc vname e1 e2 in
      * let g1, cnode = mk_node g (GChoice fcond) [] in
      * let g2, succ = eval_exprs g1 cnode fcfg es in
      * add_opts_edge
      *   (add_opts_edge g2 cnode (LChoice true) succ)
      *   cnode (LChoice false) succ_n
      * , cnode *)

  | Return e ->
     let g, n = mk_node g (GReturn e) [] in
     let n' = get_opt n in
     let edge = G.E.create n' LDefault fcfg.f_out in
     G.add_edge_e g edge, n

  | Call (fname, _es) ->
     let ins, opt_out =
       if Builtins.is_builtin fname then [succ_n], None
       else
         let f_cl = Hashtbl.find fun_tbl fname in
         [succ_n; Some f_cl.f_in;], Some f_cl.f_out
     in
     let g', n = mk_node g (GCall e) ins in
     add_opts_edge g' opt_out LOut n,
     n

  | Switch (e, cases) ->
     let e' = { e with e_desc = switch_as_if e cases; } in
     eval_expr g succ_n fcfg e'

and eval_exprs g succ_n m_id exprs =
  let re = List.rev exprs in
  let rec aux g succ_n = function
    | [] -> g, succ_n
    | [x] -> eval_expr g succ_n m_id x
    | x :: xs ->
       let g', n = eval_expr g succ_n m_id x in
       aux g' n xs
  in aux g succ_n re
;;

let mk_fcfg g fname floc =
  let g_module = fname in
  let f_out = { g_expr = GOut (floc, g_module); g_module; }
  and f_in = { g_expr = GIn (floc, g_module); g_module; } in
  let g = G.add_vertex g f_out in
  g, {f_id = g_module; f_out; f_in; }
;;

let eval_fundef g fundef =
  let g_module = fundef.fun_id in
  (* add_out_node *)
  let g, fcfg = mk_fcfg g g_module fundef.fun_loc in
  Hashtbl.add fun_tbl g_module fcfg;
  let g, n = eval_exprs g (Some fcfg.f_out) fcfg fundef.fun_body in
  (* Adds the edge from entry to first command *)
  let e = G.E.create fcfg.f_in LDefault (get_opt n) in
  G.add_edge_e g e
;;

let add_pending_fcalls g =
  Hashtbl.fold
    (fun fname callnodes g ->
     if not (Builtins.is_builtin fname) then (
       try
         let fun_cfg = Hashtbl.find fun_tbl fname in
         NodeSet.fold
           (fun cn g ->
            (* Adding return links *)
            let g' =
              let edge = G.E.create fun_cfg.f_out GLabel.LOut cn in
              G.add_edge_e g edge
            in add_opt_edge g' GLabel.LCall cn (Some fun_cfg.f_in)
           ) callnodes g
       with
       | Not_found ->
          (Io.warning "Cannot find function definition for CFG: %s@." fname);
          g
     ) else g
    ) fun_calls g
;;

let dotofile = !Utils.mktemp "cfg_" ".dot" ;;

let output g =
  let oc =
    if Driver.get_cfg_view () then open_out_bin dotofile
    else Pervasives.stdout
  in
  GDot.output_graph oc g;
  close_out oc;
;;

let compile_and_show () =
  let png_file = Driver.get_cfg_file () in
  ignore (Sys.command (Format.sprintf "dot -Tpng -o %s %s" png_file dotofile
                      ));
  if Driver.get_cfg_view () then
    ignore (Sys.command (Format.sprintf "%s %s" (browser ()) png_file));
;;

let build program =
  Io.debug "Building CFG in %s ...@." dotofile;
  let g = List.fold_left eval_fundef G.empty program.a_functions in
  let g, main = mk_fcfg g "algoritmo" program.a_loc in
  let g, n = eval_exprs g (Some main.f_out) main program.a_body in
  let g = G.add_edge_e g (G.E.create main.f_in LDefault (get_opt n)) in
  (* Treat pending function calls *)
  let g = add_pending_fcalls g in
  output g;
  compile_and_show ();
;;
