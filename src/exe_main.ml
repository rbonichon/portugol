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
  " output debug messagesx";
  "-sbs", Arg.Unit (fun () -> Driver.set_sbs true),
  " activate step by step execution";
  "-cfg", Arg.Unit (fun () -> Driver.set_cfg true),
  " output cfg";
  "-ocfg", Arg.String (fun s -> Driver.set_cfg true; Driver.set_cfg_file s;),
  " output cfg";
  "-cfgview", Arg.Unit (fun () -> Driver.set_cfg_view true),
  " computes CFG and view in browser";
  "-trace", Arg.Unit (fun () -> Driver.set_tracing true),
  " trace execution";
  "-pp", Arg.Unit (fun () -> Driver.set_pp true),
  " prints the parsed program on stdout";
  "-noexec", Arg.Unit (fun () -> Driver.set_noexec true),
  " do not execute the program, just parse it";
  "-I", Arg.String (fun s -> Driver.add_include_directory s),
  " add directory to include search path";
  "-lib", Arg.Unit (fun () -> Driver.set_lib true),
  " treat as a library";
]

and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0;
;;

let lex_file () =
  try
    Arg.parse argspec Driver.set_file umsg;
    let file = Driver.get_file () in
    Io.debug "Opening %s@." file;
    Utils.lex_file file
  with
    | Not_found -> exit 2;
;;

let main () =
  let (lexbuf, _close) = lex_file () in
  try
    Io.debug "Parsing file %s" (Driver.get_file ());
    if Driver.is_lib () then
      let _ = Parser.library Lexer.token lexbuf in
      exit 0;
    else
    let pgram = Parser.entry Lexer.token lexbuf in
    let pgram = Preprocess.add_includes pgram in
    if Driver.get_pp () || Driver.get_debug () then
      Ast_utils.Pp.pp_program Format.std_formatter pgram;
    if not (Driver.get_no_exec ()) then
      begin
        Analyze_variables.Undeclared.run pgram;
        Analyze_variables.Unused.run pgram;
        Io.debug "Typing program";
        (* Type-check the program *)
        ignore (Typer.eval pgram);
        if Driver.get_cfg () then (Cfg.build pgram ; exit 0;);
        (* Evaluate it *)
        Interp.eval pgram;
      end
  with
  | Parser.Error -> Io.Error.report_error lexbuf "Syntax error"
;;

main ()
