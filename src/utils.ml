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

let rec range i1 i2 =
  if i1 > i2 then []
  else if i1 = i2 then [i1]
  else i1 :: range (i1 + 1) i2
;;

let browser () =
  try Sys.getenv "BROWSER"
  with Not_found -> "firefox"
;;

module StringSet =   Set.Make(String) ;;

module VSet = struct
    include StringSet

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

let lex_file (file: string) : Lexing.lexbuf * (unit -> unit) =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = file;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
  (lexbuf, fun () -> close_in chan)
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

 let pp_list ?(sep=format_of_string "@, ") f fmt l =
  let rec aux fmt = function
    | [] -> ()
    | [e] -> Format.fprintf fmt "%a" f e
    | e :: es -> Format.fprintf fmt "%a%(%)%a" f e sep aux es
  in Format.fprintf fmt "%a" aux l
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
