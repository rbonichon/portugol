type t = private
       | VInt of int
       | VFloat of float
       | VString of string
       | VBool of bool
       | VArray of int * (t ref array)
       | VUnit
;;

(** Constructors *)
val mk_unit : unit -> t ;;
val mk_int : int -> t ;;
val mk_float : float -> t ;;
val mk_string : string -> t ;;
val mk_bool : bool -> t ;;

val as_string : t -> string ;;
val as_float : t -> float ;;
val as_int : t -> int ;;
val as_bool : t -> bool ;;
val as_array : t -> int * t ref array ;;

val to_string : t -> string ;;
(** [to_string v] generates the string representation of [v] *)

val zero_based_idx : int -> int -> int ;;

val pp_ty : Format.formatter -> t -> unit ;;
val pp_value : Format.formatter -> t -> unit ;;

val allocate : Types.t -> t ;;
  (** Allocate default value w.r.t to the declared type *)
