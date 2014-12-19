(** Allowed types *)

type ty = Types.t ;;

type var = {
  var_id: string;
  var_type: ty;
  var_loc: Location.t;
}
;;

type log_op = Band |  Bor | Bxor ;;

type ulog_op = Bnot ;;

type rel_op = Gt | Gte | Lt | Lte | Eq | NotEq ;;

type arith_op = Plus | Minus | Div | Mult | EDiv | Mod ;;

type uarith_op = UMinus ;;

type bop_desc =
  | Rel of rel_op
  | Log of log_op
  | Arith of arith_op

type binop = {
  bop_desc: bop_desc;
  bop_loc: Location.t;
}

type uop_desc =
  | ULog of ulog_op
  | UArith of uarith_op

type unop = {
  uop_desc: uop_desc;
  uop_loc: Location.t;
}

type varg =
  | ByValue of var
  | ByRef of var
;;


type expr = {
  e_desc: expr_desc;
  e_loc: Location.t;
}

and expr_desc =
  | Int of int
  | Real of float
  | String of string
  | Var of string
  | Bool of bool
  | Call of string * expr list
  | BinExpr of binop * expr * expr
  | UnExpr of unop * expr
  | ArrayExpr of string * expr list
  | Assigns of lval * expr
  | IfThenElse of expr * expr list * expr list
  | While of expr * expr list
  | Repeat of expr * expr list
  | For of string * expr * expr * int * expr list
  | Switch of expr * case list
  | Return of expr

and lval =
  | Id of string
  | ArrayId of string * expr list

and case = (expr list) * (expr list)
;;


type fundef = {
  fun_id: string;
  fun_formals: varg list;
  fun_locals: var list;
  fun_body: expr list;
  fun_return_type: ty;
  fun_loc: Location.t
}

type algorithm = {
  a_id: string;
  a_variables: var list;
  a_body: expr list;
  a_functions: fundef list;
  a_includes: string list; (* Name of files to include *)
  a_loc: Location.t;
}
;;

type library = {
    lib_id : string;
    lib_variables: var list;
    lib_functions: fundef list;
    lib_includes: string list;
    lib_loc: Location.t;
  }
;;
