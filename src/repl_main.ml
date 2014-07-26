let read_input venv =
  let lexbuf = Lexing.from_string (read_line ()) in
  let e = Parser.toplevel Lexer.token lexbuf in
  let venv, v = Interp.eval_expr venv e in
  Format.printf "@.%a : %s@?" Base.Values.pp_val v (Base.Values.to_string v);
  venv
;;

let top_loop () =
  let rec aux venv =
    Format.printf "@.portugol >> @?";
    aux (read_input venv)
  in aux Base.Values.ValEnv.empty
;;

let main () =
  Format.printf "Welcome to toplevel";
  top_loop ();
;;

main () ;;

(*
 Local Variables:
 compile-command: make repl
 End:
*)
