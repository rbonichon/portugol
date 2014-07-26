module Env = struct
  include Map.Make(String)
end
;;

module Values = struct
  type t =
  | VInt of (int option)
  | VFloat of (float option)
  | VString of (string option)
  | VBool of (bool option)
  | VArray of int * int * t array
  | VUnit
  ;;

  let rec unitialized = function
    | VInt None
    | VFloat None
    | VString None
    | VBool None -> true
    | VArray (i1, i2, a) -> Utils.array_for_all unitialized i1 i2 a
    | _ -> false
  ;;

  (** Construction functions *)
  let mk_int i = VInt (Some i)
  and mk_float f = VFloat (Some f)
  and mk_string s = VString (Some s)
  and mk_bool b = VBool (Some b)
  and mk_unit () = VUnit
  ;;

  let apply_opt f = function
    | Some v -> f v
    | None -> "??"

  let rec pp_val fmt = function
    | VString s -> Format.fprintf fmt "%s" (apply_opt (fun x -> x) s)
    | VFloat f -> Format.fprintf fmt "%s"  (apply_opt string_of_float f)
    | VInt i -> Format.fprintf fmt "%s" (apply_opt string_of_int i)
    | VBool b -> Format.fprintf fmt "%s" (apply_opt string_of_bool b)
    | VArray (_fidx, _lidx, a)  ->
       Format.fprintf
         fmt "[ %a ]"
         (fun fmt a ->
          Array.iter (fun e -> Format.fprintf fmt "%a; " pp_val e) a
         ) a
    | VUnit -> ()
  ;;

  let rec to_string = function
    | VString _ -> "string"
    | VFloat _ -> "real"
    | VInt _ -> "int"
    | VBool _ -> "bool"
    | VArray (i1, _, a) -> (to_string a.(i1))^" array"
    | VUnit -> "()"
  ;;

  module ValEnv = struct

    type venv = {
      current_f: string; (* Current function name *)
      globals: t Env.t;
      locals: t Env.t;
    }
    ;;

    let empty = { current_f = "";
                  globals = Env.empty; locals = Env.empty; }

    let pp_with_hdr title fmt venv =
      Format.fprintf fmt "%s@ " title;
      Env.iter
        (fun name value ->
         Format.fprintf fmt "%s : %a@ " name pp_val value
        ) venv
    ;;

    let pp fmt venv =
      Format.fprintf
        fmt "@[<v 0>@ %a@ %a@ "
        (pp_with_hdr "Globals") venv.globals
        (pp_with_hdr "Locals") venv.locals
      ;
      Format.fprintf fmt "@]";
    ;;

    let add env name value =
      { env with locals = Env.add name value env.locals }
    ;;

    let find env name =
      try
        Env.find name env.locals
      with
      | Not_found -> Env.find name env.globals
    ;;
  end
end



module Types = struct
  (** Types allowed in Portugol *)
  type t =
    | TyArrow of t list * t
    | TyInt
    | TyReal
    | TyString
    | TyBool
    | TyAny
    | TyArray of int * int * t
    | TyUnit (** the type of instructions *)
  ;;

  let is_unit = function
    | TyUnit -> true
    | _ -> false
  ;;

  let return_type = function
    | TyArrow (_, t) -> t
    | _ -> assert false
  ;;

  let rec pp fmt = function
    | TyInt -> Format.fprintf fmt "inteiro"
    | TyReal -> Format.fprintf fmt "real"
    | TyString -> Format.fprintf fmt "caractere"
    | TyAny -> Format.fprintf fmt "any"
    | TyBool -> Format.fprintf fmt "logico"
    | TyArray (_, _, t) -> Format.fprintf fmt "vetor[%a]" pp t
    | TyUnit -> Format.fprintf fmt "unit"
    | TyArrow (targs, t)->
       let rec pp_args fmt targs =
         match targs with
         | [] -> ()
         | [ta] -> Format.fprintf fmt "%a" pp ta
         | ta :: tas -> Format.fprintf fmt "%a x@ %a"
                                       pp ta pp_args tas
       in
       Format.fprintf fmt "@[<hov 1>%a -> %a@]" pp_args targs pp t
  ;;
end
