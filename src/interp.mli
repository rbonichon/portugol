val eval_expr: Base.Values.ValEnv.venv -> Ast.expr ->
               (Base.Values.ValEnv.venv * Base.Values.t) Lwt.t
;;

val eval: Ast.algorithm -> unit ;;
