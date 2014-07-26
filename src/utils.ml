exception NoOptionalValue ;;
let get_opt = function
  | Some x -> x
  | None -> raise NoOptionalValue
;;


let rec array_for_all p i1 i2 a  =
  i1 > i2 ||
p a.(i1) && array_for_all p (succ i1) i2 a
;;


module VSet = struct
  include Set.Make(String) ;;

  let pp fmt vset =
    Format.fprintf fmt "@[<hov 2>";
    iter (fun v -> Format.fprintf fmt "%s;@ " v) vset;
    Format.fprintf fmt "@]";
  ;;

  let of_list strings =
    List.fold_left (fun set s -> add s set) empty strings
  ;;
end


let sfprintf text =
  let b = Buffer.create 256 in
  let return fmt = Format.pp_print_flush fmt (); Buffer.contents b in
  Format.kfprintf return (Format.formatter_of_buffer b) text
;;
