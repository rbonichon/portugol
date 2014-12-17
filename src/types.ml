(** Types allowed in Portugol *)
  type t =
    | TyArrow of t list * t    (** Functions *)
    | TyInt                    (** Integers *)
    | TyReal                   (** Floats *)
    | TyString                 (** Strings *)
    | TyBool                   (** Booleans *)
    | TyAny                    (** Polymorphic hole *)
    | TyArray of int * int * t (** Array type with first and last index
                                   authorized *)
    | TyUnit                   (** the type of instructions *)
  ;;

  let is_unit = function
    | TyUnit -> true
    | _ -> false
  ;;

  let return_type = function
    | TyArrow (_, t) -> t
    | _ -> assert false
  ;;

  (* Returns a list of dimensions and the base types of elements of a vector *)
  let get_dim_btype vty =
    let rec aux dims = function
    | (TyInt | TyReal | TyString | TyAny | TyBool | TyUnit | TyArrow _) as ty ->
       List.rev dims, ty
    | TyArray (i1, i2, t) -> aux ((i1, i2) :: dims) t
    in aux [] vty
  ;;
  let rec pp fmt = function
    | TyInt -> Format.fprintf fmt "inteiro"
    | TyReal -> Format.fprintf fmt "real"
    | TyString -> Format.fprintf fmt "caractere"
    | TyAny -> Format.fprintf fmt "any"
    | TyBool -> Format.fprintf fmt "logico"
    | TyArray _ as ty ->
       let d, bty = get_dim_btype ty in
       let pp_dim fmt (i1, i2) = Format.fprintf fmt "%d..%d" i1 i2 in
       Format.fprintf
         fmt "@[vetor [%a] de %a@]" (Utils.pp_list ~sep:",@ " pp_dim) d pp bty
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
