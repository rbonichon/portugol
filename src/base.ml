module Env = struct
  include Map.Make(String)
end
;;



module TypedMem = struct
    open Format
    open Types

    module M =
      Map.Make(struct
                  type t = int
                  let compare = Pervasives.compare
                end)
    ;;
    module S = Utils.SMap ;;

    type offset = int ;;
    type path = offset list ;;
    type basic =
      | VInt of (int option)
      | VFloat of (float option)
      | VString of (string option)
      | VBool of (bool option)
      | VUnit
    ;;

    type mmap = mvalue M.t
    and mvalue =
      | Immediate of basic
      | Indirect of int * int * mmap
    ;;

    let rec unitialized = function
      | Immediate (VInt None)
      | Immediate (VFloat None)
      | Immediate (VString None)
      | Immediate (VBool None) -> true
      | Indirect (_, _, mmap) -> M.for_all (fun _ v -> unitialized v) mmap
      | _ -> false
    ;;

    (** Construction functions *)
    let mk_int i = Immediate (VInt (Some i))
    and mk_float f = Immediate (VFloat (Some f))
    and mk_string s = Immediate (VString (Some s))
    and mk_bool b = Immediate (VBool (Some b))
    and mk_unit () = Immediate VUnit
    ;;

    let apply_opt f = function
      | Some v -> f v
      | None -> "??"

    let pp_basic fmt = function
      | VString s -> Format.fprintf fmt "%s" (apply_opt (fun x -> x) s)
      | VFloat f -> Format.fprintf fmt "%s"  (apply_opt string_of_float f)
      | VInt i -> Format.fprintf fmt "%s" (apply_opt string_of_int i)
      | VBool b -> Format.fprintf fmt "%s" (apply_opt string_of_bool b)
      | VUnit -> ()
    ;;

    (* Represents a numerical value (VInt or VFloat) as a float *)
    let num_as_caml_float = function
      | Immediate (VFloat (Some vf)) -> vf
      | Immediate (VInt (Some vi)) -> float vi
      | _ -> assert false
    ;;

    type namemap = int S.t ;;
    type t = {
        n : int;
        index : namemap; (* Associate a name to its integer index *)
        values : mmap ;
      }
    ;;


    let empty = { n = 0; index = S.empty; values = M.empty; }

    (* Built a default unitialized value from the declared type of a variable *)
    let mk_default_val_from_type ty =
      let rec aux = function
        | TyInt -> Immediate (VInt None)
        | TyReal -> Immediate (VFloat None)
        | TyString -> Immediate (VString None)
        | TyBool -> Immediate (VBool None)
        | TyArray (idx1, idx2, ty) ->
           let mvalue = aux ty in
           let mmap =
             List.fold_left
               (fun mmap i -> M.add i mvalue mmap) M.empty (Utils.range idx1 idx2)
           in Indirect (idx1, idx1, mmap)
        | _ -> assert false
      in aux ty
    ;;

    let get_idx (name: string) mem = S.find name mem.index ;;
    let as_indirect mmap = Indirect (0, max_int, mmap) ;;
    let is_immediate = function
      | Immediate _ -> true
      | Indirect _ -> false
    ;;

    let strip_immediate mval =
      assert(is_immediate mval);
      match mval with
      | Immediate v -> v
      | _ -> assert false
    ;;

    let get_mmap = function
      | Indirect(_ , _, mmap) -> mmap
      | Immediate _ -> assert false
    ;;

    let inject (name:string) (ty:Types.t) (mem:t) =
      let idx = mem.n in
      let n = mem.n + 1 in
      let index = S.add name idx mem.index in
      let values = M.add idx (mk_default_val_from_type ty) mem.values in
      { n; index; values; }
    ;;

    let get (p:path) (mem:t) =
      List.fold_left
        (fun mval idx ->
         match mval with
         | Indirect (_min, _max, mmap) ->
            (* assert(min <= idx && idx <= max);*)
            M.find idx mmap
         | _ -> assert false
        ) (as_indirect mem.values) p
    ;;

    let get_named (name: string) (p:path) (mem:t) =
      get ((get_idx name mem) :: p) mem
    ;;

    let pp_path fmt p =
      fprintf fmt "(%a)" (Utils.pp_list ~sep:";" pp_print_int) p
    ;;

    let rec pp_mvalue fmt = function
      | Immediate v -> fprintf fmt "%a" pp_basic v
      | Indirect (_, _, mmap) ->
         fprintf fmt "@[<hov 2>{ ";
         M.iter (fun i mv -> fprintf fmt "%d: %a;@ " i pp_mvalue mv) mmap;
         fprintf fmt "}@]";
    ;;

    let pp_ty fmt = function
      | Immediate VBool _ -> fprintf fmt "bool"
      | Immediate VInt _ -> fprintf fmt "int"
      | Immediate VFloat _ -> fprintf fmt "float"
      | Immediate VString _ -> fprintf fmt "string"
      | Immediate VUnit -> fprintf fmt "unit"
      | Indirect (_, _, _) -> fprintf fmt "array"
    ;;

    let pp_store fmt (store: t) =
      fprintf fmt "@[<v 0>";
      S.iter
        (fun name idx ->
         fprintf fmt "%d: %s -> %a@ " idx name pp_mvalue (get [idx] store))
        store.index;
      fprintf fmt "@]";
    ;;

    let update (p:path) (v:mvalue) (mem:t) =
      assert(p <> []);
      Io.debug "%a@.%a@." pp_path p pp_store mem;
      let aux mmap  = function
        | [] -> assert false
        | [idx] -> M.add idx v mmap
        | idx :: idxs ->
           let rec aux' mvalue idxs =
             match mvalue, idxs with
             | Indirect (min, max, mmap), [ i ] ->
                (* assert(min <= i && i <= max);*)
                Indirect(min, max, M.add i v mmap)
             | Indirect (min, max, mmap),  i :: is ->
                assert(min <= i && i <= max);
                Io.debug "Find %d@." i;
                let mval = aux' (M.find i mmap) is in
                Indirect (min, max, M.add i mval mmap)
             | _, _ -> assert false
           in M.add idx (aux' (M.find idx mmap) idxs) mmap
      in { mem with values = aux mem.values p }
    ;;

    let update_named (name:string) (p:path) (v:mvalue) (mem:t) =
      Io.debug "Update %s@." name;
      update ((get_idx name mem) :: p) v mem
    ;;

    let mem (name:string) (mem:t) = S.mem name mem.index ;;



end

module ValEnv = struct
    module TM = TypedMem ;;
    type venv = {
      current_f: string; (* Current function name *)
      globals: TypedMem.t;
      locals: TypedMem.t;
      formals: TypedMem.t;
    }
    ;;

    let empty = { current_f = "UNSET";
                  globals = TM.empty;
                  locals = TM.empty;
                  formals = TM.empty;
                }

    let pp_with_hdr title fmt (mem:TM.t) =
      Format.fprintf fmt "%s@ %a" title TM.pp_store mem;
    ;;

    let pp fmt venv =
      Format.fprintf
        fmt "@[<v 0>@ %a@ %a@ "
        (pp_with_hdr "Globals") venv.globals
        (pp_with_hdr "Locals") venv.locals
      ;
      Format.fprintf fmt "@]";
    ;;

    let add env name offsets value =
      if TM.mem name env.locals then
        { env with locals = TM.update_named name offsets value env.locals }
      else if TM.mem name env.formals then
        { env with formals = TM.update_named name offsets value env.formals }
      else if TM.mem name env.globals then
        { env with globals = TM.update_named name offsets value env.globals }
      else assert false;
      (* Pre-ran code analyses should forbid the binding of a value to an
       * unbound variables *)
    ;;

    let add_immediate env name value =
      assert(TM.is_immediate value);
      add env name [] value ;;

    let find env name offsets =
      try TM.get_named name offsets env.locals
      with
      | Not_found ->
         begin
           try TM.get_named name offsets env.formals
           with Not_found -> TM.get_named name offsets env.globals
         end
    ;;

    let find_immediate env name =
      let mval = find env name [] in
      assert(TM.is_immediate mval);
      mval
    ;;

  end
