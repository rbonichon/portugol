open Ast
open Utils
;;

(* Includes the set of files specified as import directives *)
exception FileFound of string ;;

let add_includes (program: Ast.algorithm) : Ast.algorithm =
  let include_directories = Driver.get_include_directories () in
  let to_include = program.a_includes in
  let included = StringSet.empty in
  let find_qualified_file basename =
    StringSet.iter
      (fun dir ->
       let fname = Filename.concat dir basename in
       if Sys.file_exists fname then raise (FileFound fname)
      ) include_directories;
    raise Not_found;
  in
  let rec do_include program (included: StringSet.t) (to_include: string list)
      : Ast.algorithm
    =
    match to_include with
    | [] -> program
    | incl :: incls ->
       begin
         try find_qualified_file incl
         with
         | Not_found ->
            Io.fail program.a_loc (Format.sprintf "Cannot find file %s@." incl)
         | FileFound f ->
            begin
              let lbuf, close = Utils.lex_file f in
              try
                let m = Parser.library Lexer.token lbuf in
                close ();
                (* TODO: Check variable and function names *)
                let included = StringSet.add f included in
                (* Adds also included file from the library file *)
                let to_include =
                  List.fold_left
                    (fun fs f ->
                     if StringSet.mem f included then fs else f :: fs)
                    incls m.lib_includes
                in
                (* Should make some verification here *)
                let p =
                  { program with
                    a_variables = program.a_variables @ m.lib_variables;
                    a_functions = program.a_functions @ m.lib_functions;
                  }
                in do_include p included to_include
              with
              | Parsing.Parse_error -> Io.Error.report_error lbuf "Syntax error"
            end
       end
  in do_include program included to_include
;;
