open Lexing ;;

type t = {
  loc_start: position;
  loc_end: position;
}
;;

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
   { loc_start = loc;
     loc_end = loc;
   }
;;

let none = in_file "_none_";;

let dummy_loc = { loc_start = dummy_pos; loc_end = dummy_pos; }
;;

let pp_lines fmt l =
  let lstart = l.loc_start.pos_lnum
  and lend = l.loc_end.pos_lnum in
  if lstart = lend then Format.fprintf fmt "%d" lstart
  else Format.fprintf fmt "%d-%d" lstart lend
;;
