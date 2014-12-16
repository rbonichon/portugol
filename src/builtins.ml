open Base ;;
open Base.Types;;
module TM = Base.TypedMem ;;
open TM ;;
open Lwt ;;

type funarg =
  | AVal of TM.mvalue
  | ARef of string * TM.path * TM.mvalue
;;

let get_val = function
  | AVal v -> v
  | ARef (_, _, v) -> v
;;

type specargs = SVal | SName | SRep of specargs;;

type t = {
  p_name: string;
  p_args: specargs;
  p_type: Types.t;
  mutable p_eval:
            Base.ValEnv.venv -> (funarg list) Lwt.t ->
            (Base.ValEnv.venv * TM.mvalue) Lwt.t;
}
;;

let set_eval_function builtin eval = builtin.p_eval <- eval ;;

(* A hash-table module for strings *)
module H = Hashtbl.Make(
               struct
                 type t = String.t
                 let hash = Hashtbl.hash
                 let equal x y = String.compare x y = 0
               end
             ) ;;


let print_args_as_strings ?newline:(nl=false) args =
  return (
      List.iter
        (fun a -> Io.result "%a" TM.pp_mvalue a) args
    )
   >>=
  fun _ -> return (if nl then Io.result "@."  else Io.result "@?")

;;

let print_def = {
  p_name = "escreva";
  p_args = SRep SVal;
  p_type = TyArrow([TyAny], TyUnit);
  p_eval =
    (fun env args ->
     args >>= fun fargs ->
     return (print_args_as_strings (List.map get_val fargs))
     >>= fun _ -> return (env, TM.mk_unit ()));
    }
;;

let println_def = {
  print_def with
  p_name = "escreval";
  p_eval =
    (fun env args ->
     args >>= fun fargs ->
     return (print_args_as_strings (List.map get_val fargs))
     >>= fun _ -> return (env, TM.mk_unit ()));
 }

let string_length_def = {
    p_name = "compr";
    p_args = SVal;
    p_type = TyArrow([TyString], TyInt);
    p_eval =
      (fun env args ->
       args >>= fun fargs ->
       assert(List.length fargs = 1);
       return (
           env,
           match fargs with
           | (AVal (Immediate (VString (Some s)))) :: _ ->
              mk_int (String.length s)
           | _ -> assert false
         ))
  }
;;

(* Strings in Portugol are 1-indexed,
   Strings in OCaml are 0-indexed
 *)
let string_sub_def = {
    p_name = "copia";
    p_args = SVal;
    p_type = TyArrow([TyString; TyInt; TyInt], TyString);
    p_eval =
      (fun env args ->
       args >>= fun fargs ->
       assert(List.length fargs = 3);
       return (
           env,
           match fargs with
           | (AVal (Immediate (VString (Some s)))) ::
               (AVal (Immediate (VInt (Some sidx)))) ::
                 (AVal (Immediate (VInt (Some slen)))) :: _
             -> mk_string (String.sub s (sidx - 1) slen)
           | _ -> assert false
         ))
  }
;;


let ascii_code_def = {
    p_name = "asc";
    p_args = SVal;
    p_type = TyArrow([TyString;], TyInt);
    p_eval =
      (fun env args ->
       args >>= fun fargs ->
       assert(List.length fargs = 1);
       return (
           env,
           match fargs with
           | (AVal (Immediate (VString (Some s)))) :: _
             -> mk_int (Char.code s.[0])
           | _ -> assert false
         ))
  }
;;


let chr_def = {
    p_name = "carac";
    p_args = SVal;
    p_type = TyArrow([TyInt;], TyString);
    p_eval =
      (fun env args ->
       args >>= fun fargs ->
       assert(List.length fargs = 1);
       return (
           env,
           match fargs with
           | (AVal (Immediate (VInt (Some i)))) :: _
             (* TODO: Return a proper error if i < 0 || o >255 *)
             -> mk_string (String.make 1 (Char.chr i))
           | _ -> assert false
         ))
  }
;;


(** Read an entry: *)


let read_impl read_entry env args =
  try
    read_entry () >>=
      fun line ->
      args >>= fun args ->
      let words = Utils.split_on_spaces line in
      let env =
        List.fold_left2
          (fun e a w ->
           match a with
           | ARef (name, path, v) ->
              let v' =
                match v with
                | Immediate VInt _ -> mk_int (int_of_string w)
                | Immediate VFloat _ -> mk_float (float_of_string w)
                | Immediate VString _ -> mk_string w
                | Immediate VBool _ -> mk_bool (bool_of_string w)
                | _ -> assert false
              in ValEnv.add e name path v'
           | AVal _ -> assert false
          )
          env args words
      in

      Io.debug "Read unit %s" line;
      return (env, mk_unit ())
  with _ -> Io.error "Bad argument entered. Check the type\n"; exit 1;
;;

(* Definition for primitive leia *)
let read_def = {
  p_name = "leia";
  p_args = SRep SName;
  p_type = TyArrow([TyAny], TyAny);
  p_eval = read_impl (fun () -> return (read_line ()));
}

let rand_int = {
  p_name = "randi";
  p_args = SVal;
  p_type = TyArrow([TyInt], TyInt);
  p_eval = (fun env args ->
(*            print_args_as_strings (List.map get_val args);*)
            args >>= fun args ->
            return (
                env,
                match args with
                | (AVal (Immediate (VInt (Some x)))) :: _ -> mk_int (Random.int x)
                | _ -> assert false
              )
           (* Should have been checked by a prior
            * analysis *)
           );
}

let rand_float = {
  p_name = "rand";
  p_args = SRep SVal;
  p_type = TyArrow([], TyReal);
  p_eval = (fun env _ -> return (env, mk_float (Random.float (1.0))));
}
;;

(* Polymorphic testing *)
let eq_test = {
    p_name = "_eqtest";
    p_type = TyArrow([TyAny; TyAny], TyBool);
    p_args = SVal ;
    p_eval =
      (fun env args ->
       args >>= fun args ->
       return (
           env,
           let rec all_equal = function
             | [] -> assert false
             | [_] -> true
             | x :: ((y :: _) as l) ->
                (x = y) && all_equal l
           in mk_bool (all_equal args)
         )
      )
}
;;

let float2int = {
  p_name = "int";
  p_args = SVal;
  p_type = TyArrow([TyReal], TyInt);
  p_eval =
    (fun env args ->
     args >>= fun args ->
     return (
         env,
         match args with
         | AVal (Immediate (VFloat (Some f))) :: _ -> mk_int (truncate f)
         | _ -> assert false
       )
    );
}

let unary_real f = fun env args ->
  args >>= fun args ->
  assert (List.length args = 1);
  let vf =
    match List.hd args with
    | AVal av -> num_as_caml_float av
    | _ -> assert false
  in return (env, mk_float (f vf))
;;

let unary_math_funs =
  [ "abs", abs_float;
    "arccos", acos;
    "arcsen", asin;
    "arctan", atan;
    "cos", cos;
    "sen", sin;
    "tan", tan;
    "log", log;
    "raizq", sqrt;
    "quad", (fun x -> x *. x);
  ]
;;

let binary_real f = fun env args ->
  args >>= fun args ->
  Io.debug "Num args: %d@." (List.length args);
  assert (List.length args = 2);
  let vf1, vf2 =
    match args with
    | AVal av1 :: AVal av2 :: [] -> num_as_caml_float av1, num_as_caml_float av2
    | _ -> assert false
  in return (env, mk_float (f vf1 vf2))
;;


let binary_math_funs = [
  "logn", (fun x n -> log(x) /. log(n));
  "exp", (fun x e -> x ** e);
];;

let defs = [
  print_def;
  println_def;
  read_def;
  rand_int;
  rand_float;
  float2int;
  string_length_def;
  string_sub_def;
  ascii_code_def;
  chr_def;
] @
             List.map
               (fun (name, ml_math_fun) ->
                { p_name = name;
                  p_args = SVal;
                  p_type = TyArrow([TyReal], TyReal);
                  p_eval = unary_real (ml_math_fun);
                }
               ) unary_math_funs
             @
               List.map
                 (fun (name, ml_math_fun) ->
                  { p_name = name;
                    p_args = SVal;
                    p_type = TyArrow([TyReal; TyReal], TyReal);
                    p_eval = binary_real (ml_math_fun);
                  }
                 ) binary_math_funs

;;

let h = H.create (List.length defs) ;;

let output_builtins = [ "escreva"; "escreval"; ];;
let is_output_builtin name = List.mem name output_builtins ;;


let input_builtins = ["leia"; ] ;;
let is_input_builtin name =  List.mem name input_builtins ;;

let find_fundef name = H.find h name ;;

let is_fundef name = H.mem h name ;;

(** Constants **)

let pi = 4.0 *. atan 1.0 ;;

let constants = [
  ("pi", TM.mk_float pi);
]
;;

let constants_tbl = H.create (List.length constants) ;;

let find_constant name = H.find constants_tbl name ;;

let is_constant name = H.mem constants_tbl name ;;

let is_builtin name = is_constant name || is_fundef name ;;

(* Initialize everything *)
let _ =
  Random.self_init (); (* For random functions *)
  (* Function builtins *)
  List.iter (fun def -> H.add h def.p_name def) defs;
  (* Constants *)
  List.iter
    (fun (cname, cval) -> H.add constants_tbl cname cval)
    constants;
;;
