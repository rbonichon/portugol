exception NoOptionalValue ;;
let get_opt = function
  | Some x -> x
  | None -> raise NoOptionalValue
;;


let rec array_for_all p i1 i2 a  =
  i1 > i2 ||
p a.(i1) && array_for_all p (succ i1) i2 a
;;

let mktemp = ref Filename.temp_file
;;

let browser () =
  try Sys.getenv "BROWSER"
  with Not_found -> "firefox"

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
;;

module SMap = struct
  include Map.Make(
              struct
                type t = string ;;
                let compare = Pervasives.compare ;;
              end
            );;
end




let sfprintf text =
  let b = Buffer.create 256 in
  let return fmt = Format.pp_print_flush fmt (); Buffer.contents b in
  Format.kfprintf return (Format.formatter_of_buffer b) text
;;

(** There seems to be a problem with
   Str.split for js_of_ocaml.

   This helper function as a substitute
   for the lone use of of Str.split in [Builtins.read_impl].
*)
let split_on_spaces s =
  let l = ref [] in
  let acc = Buffer.create 128 in
  String.iter
    (fun c ->
     match c with
     | ' '
     | '\t' ->
        if Buffer.length acc <> 0 then
          begin
            l := (Buffer.contents acc) :: !l;
            Buffer.clear acc;
          end;
     | _ -> Buffer.add_char acc c
    ) s;
  List.rev ((Buffer.contents acc) :: !l)
;;


exception ZipSizeException ;;
let zip l1 l2 =
  let rec zaux z l1 l2 =
  match l1, l2 with
  | [], [] -> List.rev z
  | x :: xs, y :: ys -> zaux ((x, y) :: z) xs ys
  | _, _ -> raise ZipSizeException
  in zaux [] l1 l2
;;
