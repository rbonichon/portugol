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

let set_cfg, get_cfg = genr_bool_switch () ;;

let set_tracing, get_tracing = genr_bool_switch () ;;
