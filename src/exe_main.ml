(* Default message to the user *)
let umsg = "Usage: portuml <file>";;

(*
 * Specification of the known command-line switches of this program.
 * See OCaml's Arg module.
*)
let rec argspec =
  [
  "--help", Arg.Unit print_usage ,
  " print this option list and exits";
  "-help", Arg.Unit print_usage ,
  " print this option list and exits";
  "-debug", Arg.Unit (fun () -> Driver.set_debug true),
  " output debug messages";
  "-sbs", Arg.Unit (fun () -> Driver.set_sbs true),
  " activate step by step execution";
  "-cfg", Arg.Unit (fun () -> Driver.set_cfg true),
  " output cfg";
  "-trace", Arg.Unit (fun () -> Driver.set_tracing true),
  " trace execution";

]

and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0;
;;

let report_error lbuf msg =
  let p = Lexing.lexeme_start_p lbuf in
  Io.Error.errpos p msg;
;;

let lex_file () =
  try
    Arg.parse argspec Driver.set_file umsg;
    let file = Driver.get_file () in
    Io.debug "Opening %s@." file;
    let chan = open_in file in
    let lexbuf = Lexing.from_channel chan in
    lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = file;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
    (lexbuf, fun () -> close_in chan)
  with
    | Not_found -> exit 2;
;;

let main () =
  let (lexbuf, _close) = lex_file () in
  try
    Io.debug "Parsing file %s" (Driver.get_file ());
    let pgram = Parser.entry Lexer.token lexbuf in
    Analyze_variables.Undeclared.run pgram;
    Analyze_variables.Unused.run pgram;
    Io.debug "Typing program";
    (* Type-check the program *)
    ignore (Typer.eval pgram);
    (* Evaluate it *)
    Io.debug "Eval program @. %a" Ast_utils.pp_program pgram;
    Interp.eval pgram;
    if Driver.get_cfg () then Cfg.build pgram ;
  with
  | Parsing.Parse_error -> report_error lexbuf "Syntax error"

;;

main ()
