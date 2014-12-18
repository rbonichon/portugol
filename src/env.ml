(* An environment maps an identifier (a name for a variable) to a
   modifiable value (a reference cell that holds a value). There is
   one environment for global variables and one for local
   variables. *)
open Utils;;
type environment = Values.t ref SMap.t ;;
type t = {
    globals : environment;
    locals : environment;
  }
;;

(* Interpreting variable accesses. The reference cell that holds the
   variable is found by looking up the global and local
   environments. The lookup cannot fail if the variable is properly
   bound, a condition that the typechecker enforces. If a global and a
   local have the same name, the latter takes precedence. *)
let get (env:t) (name:string) =
  Io.debug "Looking for %s@." name;
  try SMap.find name env.locals
  with Not_found ->
    SMap.find name env.globals
;;

let add (env:environment) (name:string) (v:Values.t ref) =
  SMap.add name v env
;;

let add_local (env:t) name v = { env with locals = add env.locals name v };;
let add_global (env:t) name v = { env with globals = add env.globals name v };;

let empty = { globals = SMap.empty; locals = SMap.empty } ;;

open Format ;;
let pp_environment fmt (env:environment) =
  fprintf fmt "@[<hov 2>";
  SMap.iter
    (fun name v -> fprintf fmt "%s -> %a@ ::@ " name Values.pp_value !v) env;
  fprintf fmt "@]";
;;

let pp_env fmt (env:t) =
  fprintf fmt "@[<v 0>";
  Format.fprintf fmt "@[<v 0>Globals@ %a@]" pp_environment env.globals;
  Format.fprintf fmt "@[<v 0>Locals@ %a@]" pp_environment env.locals;
  fprintf fmt "@]";
;;
