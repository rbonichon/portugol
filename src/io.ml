open Lexing ;;
open Location ;;
open Format ;;

let newline fmt =
  pp_force_newline fmt ();
;;

type output = {
  name : string;
  mutable fmt : Format.formatter;
}

let default_out name = { name; fmt = Format.std_formatter; } ;;

let debug_output = default_out "debug"
and log_output = default_out "log"
and warning_output = default_out "warning"
and res_output = default_out "result"
and error_output = { name = "error"; fmt = Format.err_formatter; }
;;

let set_formatter output fmt = output.fmt <- fmt ;;

let ouputs = [ debug_output; log_output; warning_output; error_output; ] ;;

let glog ?tag:(tag=true) (output: output) txt  =
  let fmt = output.fmt in
  (if tag then Format.fprintf fmt "[%s] " output.name
   else Format.ifprintf fmt "");
  Format.kfprintf
    (fun fmt ->
     Format.fprintf fmt "@?";
     Format.pp_print_flush fmt ();
    )
    fmt txt
;;

(** Various types of outputs *)
let debug txt =
   if Driver.get_debug () then glog debug_output txt
   else Format.ifprintf Format.std_formatter txt
;;

let warning txt = glog warning_output txt;;

let error txt = glog error_output txt;;

let result txt = glog ~tag:false res_output txt ;;

let log txt = glog log_output txt ;;

let warn loc msg =  warning "%a %s" Location.pp_lines loc msg ;;



module Error = struct
exception Lex_error of string;;

let char_num pos = pos.pos_cnum - pos.pos_bol ;;
let errpos pos msg =
  let s = Format.sprintf "File \"%s\", line %d, character %d:"
    pos.Lexing.pos_fname pos.Lexing.pos_lnum (char_num pos)
  in error "@[%s : %s@]@." s msg;
  exit 2;
;;

let errloc loc msg =
  let lstart = loc.loc_start and lend = loc.loc_end in
  let s = Format.sprintf
    "@[<hov 1>File \"%s\",@ from line %d character %d@ to line %d character %d@]"
    lstart.pos_fname lstart.pos_lnum
    (char_num lstart)
    lend.pos_lnum
    (char_num lend)
  in error "@[%s : %s@]@." s msg;
  exit 2;
;;

end ;;

let fail loc msg = Error.errloc loc msg ;;

let not_yet_implemented loc =
  fail loc "Not yet implemented"
;;
