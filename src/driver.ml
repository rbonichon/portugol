let genr_bool_switch () =
  let file = ref false in
  (fun s ->  file := s),
  (fun () -> !file)
;;

let set_file, get_file =
  let file = ref "" in
  (fun s ->  file := s),
  (fun () -> !file)
;;

let set_print, get_print =
  let p = ref false in
  (fun s -> p := s),
  (fun () -> !p)
;;

let set_debug, get_debug =
  let p = ref false in
  (fun s -> p := s),
  (fun () -> !p)
;;

let set_sbs, get_sbs = genr_bool_switch () ;;

let set_pp, get_pp = genr_bool_switch () ;;

let set_noexec, get_no_exec = genr_bool_switch () ;;

(* CFG options *)
let set_cfg, get_cfg = genr_bool_switch () ;;

let set_cfg_file, get_cfg_file, is_default_cfg_file =
  let default = !Utils.mktemp "cfg_" ".png" in
  let file = ref default in
  (fun s -> file := s),
  (fun () -> !file),
  (fun () -> !file = default)
;;

let set_cfg_view, get_cfg_view = genr_bool_switch () ;;

let set_tracing, get_tracing = genr_bool_switch () ;;

let set_lib, is_lib = genr_bool_switch () ;;

let add_include_directory, get_include_directories =
  let dirs = ref (Utils.StringSet.singleton (".")) in
  (fun dir -> dirs := Utils.StringSet.add dir !dirs),
  (fun () -> !dirs)
;;
