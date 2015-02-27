type specargs =
  | SVal (* A by-value parameter *)
  | SName (* A by-reference parameter *)
  | SRep of specargs (* A repetition *)
;;

type t = {
    p_name: string;  (** Name of builtin *)
    p_args: specargs; (** Expected parameters *)
    p_type: Types.t; (** Return type *)
    mutable p_eval: (Values.t ref list) Lwt.t -> Values.t Lwt.t;
    (** Function representing the evaluation of the builtin *)
  }
;;

val is_builtin : string -> bool ;;
(** [is_builtin funname] checks out if [funname] designates a reserverd function
 ** name *)

val set_eval_function :
  t -> ((Values.t ref list) Lwt.t -> Values.t Lwt.t) -> unit ;;

val find_fundef : string -> t ;;
  (** raise Not_found *)

val find_constant : string -> Values.t ;;
(** raise Not_found *)

val is_input_builtin : string -> bool ;;
val is_output_builtin : string -> bool ;;

val read_impl : (unit -> string Lwt.t) -> (Values.t ref list) Lwt.t -> Values.t Lwt.t ;;
val print_def : t ;;
val println_def : t ;;
val read_def : t ;;
