open Types;;
open Values
open Lwt ;;

type specargs = SVal | SName | SRep of specargs;;

type t = {
  p_name: string;
  p_args: specargs;
  p_type: Types.t;
  mutable p_eval: (Values.t ref list) Lwt.t -> Values.t Lwt.t;
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
  return (List.iter (fun v -> Io.result "%a" Values.pp_value !v) args )
  >>= fun _ -> return (if nl then Io.result "@."  else Io.result "@?")
;;

let print_def : t= {
  p_name = "escreva";
  p_args = SRep SVal;
  p_type = TyArrow([TyAny], TyUnit);
  p_eval =
    (fun args -> args >>= fun fargs ->
     return (print_args_as_strings fargs)
     >>= fun _ -> return (Values.mk_unit ()))
    }
;;

let println_def = {
  print_def with
  p_name = "escreval";
  p_eval =
    (fun args ->
     args >>= fun fargs ->
     return (print_args_as_strings ~newline:true fargs)
     >>= fun _ -> return (Values.mk_unit ()));
 }

let string_length_def = {
    p_name = "compr";
    p_args = SVal;
    p_type = TyArrow([TyString], TyInt);
    p_eval =
      (fun args ->
       args >>= fun fargs ->
       assert(List.length fargs = 1);
       return (mk_int (String.length (as_string !(List.hd fargs)))))
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
      (fun args ->
       args >>= fun fargs ->
       assert(List.length fargs = 3);
       return (
           match fargs with
           | s :: sidx :: slen :: _ ->
              mk_string (String.sub (as_string !s) ((as_int !sidx) - 1) (as_int !slen))
           | _ -> assert false
         ))
  }
;;


let ascii_code_def = {
    p_name = "asc";
    p_args = SVal;
    p_type = TyArrow([TyString;], TyInt);
    p_eval =
      (fun args ->
       args >>= fun fargs ->
       assert(List.length fargs = 1);
       return (
           let s = as_string !(List.hd fargs) in
           mk_int (Char.code s.[0] )))
  }
;;


let chr_def = {
    p_name = "carac";
    p_args = SVal;
    p_type = TyArrow([TyInt;], TyString);
    p_eval =
      (fun args ->
       args >>= fun fargs ->
       assert(List.length fargs = 1);
       return (
           let i = as_int !(List.hd fargs) in
           (* TODO: Return a proper error if i < 0 || o >255 *)
           mk_string (String.make 1 (Char.chr i))
         ))
  }
;;

(** Read an entry: *)
let read_impl read_entry args =
  try
    read_entry () >>=
      fun line ->
      args >>= fun args ->
      let words = Utils.split_on_spaces line in
      List.iter2
        (fun r w ->
         let v =
             match !r with
             | VInt _ -> mk_int (int_of_string w)
             | VFloat _ -> mk_float (float_of_string w)
             | VString _ -> mk_string w
             | VBool _ ->
                begin
                  match (String.lowercase w) with
                  | "falso" | "f" | "0" -> mk_bool false
                  | "verdadeiro" | "v" | "t" | "1" -> mk_bool true
                  | _ -> assert false
                end
             | _ -> assert false
           in r := v
        )
        args words;
      Io.debug "Read unit %s" line;
      return (mk_unit ())
  with e -> Io.error "Bad argument entered. Check the type\n"; raise e;
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
  p_eval = (fun args ->
            args >>= fun args ->
            return ( mk_int (as_int !(List.hd args)) )
           (* Should have been checked by a prior
            * analysis *)
           );
}

let rand_float = {
  p_name = "rand";
  p_args = SRep SVal;
  p_type = TyArrow([], TyReal);
  p_eval = (fun _ -> return (mk_float (Random.float (1.0))));
}
;;

(* Polymorphic testing *)
let _eq_test = {
    p_name = "_eqtest";
    p_type = TyArrow([TyAny; TyAny], TyBool);
    p_args = SVal ;
    p_eval =
      (fun args ->
       args >>= fun args ->
       return (
           let rec all_equal = function
             | [] -> assert false
             | [_] -> true
             | x :: ((y :: _) as l) ->
                (!x = !y) && all_equal l
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
    (fun args ->
     args >>= fun args ->
     return (mk_int (truncate (as_float !(List.hd args))))
    );
}

let unary_real f = fun args ->
  args >>= fun args ->
  assert (List.length args = 1);
  return (mk_float (f (as_float !(List.hd args))))
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

let binary_real f = fun args ->
  args >>= fun args ->
  Io.debug "Num args: %d@." (List.length args);
  assert (List.length args = 2);
  let vf1, vf2 =
    match args with
    | av1 :: av2 :: [] -> as_float !av1, as_float !av2
    | _ -> assert false
  in return (mk_float (f vf1 vf2))
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
   (fun (p_name, ml_math_fun) ->
    { p_name;
      p_args = SVal;
      p_type = TyArrow([TyReal], TyReal);
      p_eval = unary_real (ml_math_fun);
    }
   ) unary_math_funs
 @
   List.map
     (fun (p_name, ml_math_fun) ->
      { p_name;
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
  ("pi", mk_float pi);
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
