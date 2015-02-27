open Format ;;

(* Values are tagged with their types *)
type t =
   | VInt of int
   | VFloat of float
   | VString of string
   | VBool of bool
   | VArray of int * (t ref array)
   | VUnit
;;

(** Construction functions *)
let mk_int i = VInt i
and mk_float f = VFloat f
and mk_string s = VString s
and mk_bool b = VBool b
and mk_unit () = VUnit
;;

let rec allocate (ty:Types.t) : t =
  match ty with
    | Types.TyInt -> VInt 0
    | Types.TyReal -> VFloat 0.
    | Types.TyString -> VString ""
    | Types.TyBool -> VBool true
    | Types.TyArray (idx1, idx2, ty) ->
       begin
         let v = allocate ty in
         let len = idx2 - idx1 + 1 in
         match v with
         | VArray (n, v) ->
            VArray(idx1, Array.init len (fun _ -> ref (VArray(n, Array.copy v))))
         | _ -> VArray(idx1, Array.init len (fun _ -> ref v))
       end
    | _ -> assert false
;;

let as_float (v:t): float =
  match v with
  | VInt i -> float i
  | VFloat f -> f
  | _ -> assert false
;;

let as_int (v:t) : int =
  match v with
  | VInt i -> i
  | _ -> assert false
;;

let as_bool (v:t) : bool =
  match v with
  | VBool b -> b
  | _ -> assert false
;;

let as_string (v:t) : string =
  match v with
  | VString s -> s
  | _ -> assert false
;;

let as_array (v:t) : int * t ref array =
  match v with
  | VArray (n, a) -> n, a
  | _ -> assert false
;;

let zero_based_idx (start:int) (idx:int) = idx - start ;;

let rec pp_value fmt = function
  | VBool b ->
     let text = if b then "verdadeiro" else "falso" in
     fprintf fmt "%s" text
  | VInt i -> fprintf fmt "%d" i
  | VFloat f -> fprintf fmt "%.6f" f
  | VString s -> fprintf fmt "%s" s
  | VUnit -> fprintf fmt "()"
  | VArray (_, a) ->
     fprintf fmt "@[<hov 2>{";
     Array.iter (fun v -> fprintf fmt "%a;@ " pp_value !v) a;
     fprintf fmt "@]}";
;;

let to_string (v : t) : string =
  Utils.sfprintf "%a" pp_value v
;;

let pp_ty fmt = function
  | VBool _ -> fprintf fmt "bool"
  | VInt _ -> fprintf fmt "int"
  | VFloat _ -> fprintf fmt "float"
  | VString _ -> fprintf fmt "string"
  | VUnit -> fprintf fmt "unit"
  | VArray _ -> fprintf fmt "array"
;;
