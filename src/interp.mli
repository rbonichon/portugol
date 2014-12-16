val eval_expr: Base.ValEnv.venv -> Ast.expr ->
               (Base.ValEnv.venv * Base.TypedMem.mvalue) Lwt.t
;;

val eval: Ast.algorithm -> unit ;;
