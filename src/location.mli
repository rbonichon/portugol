type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
}
;;

val in_file: string -> t ;;

val none : t ;;

val pp_lines: Format.formatter -> t -> unit ;;

val dummy_loc : t ;;
