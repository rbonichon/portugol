1open Base ;;
open Base.Values ;;
open Base.Types;;

type funarg =
  | AVal of Values.t
  | ARef of string * Values.t
;;

let get_val = function
  | AVal v -> v
  | ARef (_, v) -> v
;;

let get_name = function
  | AVal _ -> assert false
  | ARef (s, _v)  -> s
;;

type specargs = SVal | SName | SRep of specargs;;

type t = {
  p_name: string;
  p_args: specargs;
  p_type: Types.t;
  mutable p_eval:
            Values.ValEnv.venv -> funarg list -> Values.ValEnv.venv * Values.t;
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
  List.iter (fun a -> Io.result "%a" pp_val a) args;
  (if nl then Io.result "@."  else Io.result "@?");
;;

let print_def = {
  p_name = "escreva";
  p_args = SRep SVal;
  p_type = TyArrow([TyAny], TyUnit);
  p_eval = (fun env args ->
            let args = List.map get_val args in
            print_args_as_strings args; env, VUnit);
}
;;

let println_def = {
  print_def with
  p_name = "escreval";
  p_eval = (fun env args ->
            let args = List.map get_val args in
            print_args_as_strings ~newline:true args;
            env, VUnit)
}

(** Read an entry: *)


let read_impl read_entry env args =
  try
    let line = read_entry () in
    let words = Utils.split_on_spaces line in
    let env =
      List.fold_left2
        (fun e a w ->
         match a with
         | ARef (name, v) ->
            let v' =
              match v with
              | VInt _ -> mk_int (int_of_string w)
              | VFloat _ -> mk_float (float_of_string w)
              | VString _ -> mk_string w
              | VBool _ -> mk_bool (bool_of_string w)
              | _ -> assert false
            in ValEnv.add e name v'
         | AVal _ -> assert false
        )
        env args words
    in

    Io.log "Read unit %s" line;
    env, VUnit
  with _ -> Io.error "Bad argument entered. Check the type\n"; exit 1;
;;

(* Definition for primitive leia *)
let read_def = {
  p_name = "leia";
  p_args = SRep SName;
  p_type = TyArrow([TyAny], TyAny);
  p_eval = read_impl read_line;
}

let rand_int = {
  p_name = "randi";
  p_args = SVal;
  p_type = TyArrow([TyInt], TyInt);
  p_eval = (fun env args ->
(*            print_args_as_strings (List.map get_val args);*)
            env,
            match args with
            | (AVal (VInt (Some x))) :: _ -> mk_int (Random.int x)
            | _ -> assert false
           (* Should have been checked by a prior
            * analysis *)
           );
}

let rand_float = {
  p_name = "rand";
  p_args = SRep SVal;
  p_type = TyArrow([], TyReal);
  p_eval = (fun env _ -> env, mk_float (Random.float (1.0)));
}
;;

let float2int = {
  p_name = "int";
  p_args = SVal;
  p_type = TyArrow([TyReal], TyInt);
  p_eval =
    (fun env args ->
     env,
     match args with
     | AVal (VFloat (Some f)) :: _ -> mk_int (truncate f)
     | _ -> assert false
    );
}

let unary_real f = fun env args ->
  assert (List.length args = 1);
  let vf =
    match List.hd args with
    | AVal (VFloat (Some vf)) -> vf
    | _ -> assert false
  in env, mk_float (f vf)
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
  assert (List.length args = 2);
  let vf1, vf2 =
    match args with
    | AVal (VFloat (Some vf1)) :: AVal (VFloat (Some vf2)) :: [] -> vf1, vf2
    | _ -> assert false
  in env, mk_float (f vf1 vf2)
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
  ("pi", VFloat (Some pi));
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
