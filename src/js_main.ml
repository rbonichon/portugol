open Format ;;
open Builtins ;;

module Html = Dom_html;;

(* Redirect messages into buffers to be displayed through the gui *)
let error_buffer = Buffer.create 2048
and std_buffer = Buffer.create 2048
;;

let cleared_contents b =
  let s = Buffer.contents b in
  Buffer.clear b;
  s
;;

let std_cleared_contents () = cleared_contents std_buffer
and err_cleared_contents () = cleared_contents error_buffer
;;

let err_out = Format.formatter_of_buffer error_buffer
and std_out = Format.formatter_of_buffer std_buffer
and warn_out = Format.formatter_of_buffer
;;

let mk_html_newline fmt =
  let nl () = Format.fprintf fmt "<br/>" in
  let outfuns = pp_get_formatter_out_functions fmt () in
  pp_set_formatter_out_functions fmt { outfuns with out_newline = nl ;}
;;

let out_init () =
  Io.set_formatter Io.warning_output err_out;
  Io.set_formatter Io.res_output std_out;
  Io.set_formatter Io.error_output err_out;
;;

let parse_eval lexbuf =
  try
    (*    ignore(Parsing.set_trace true); *)
    let program = Parser.entry Lexer.token lexbuf in
    Interp.eval program;
  with
  | Parsing.Parse_error ->
     let _pos = Parsing.symbol_start_pos () in
     Io.error "Parse error@.";
  | _ -> Io.error "Unknown error"
;;

let (>>=) = Lwt.bind ;;

let create_div d name =
  let div = Html.createDiv d in
  div##style##border <- Js.string "1px black dashed";
  div##style##padding <- Js.string "5px";
  div##id <- Js.string name;
  div
;;

let set_content d content =
 d##innerHTML <- Js.string (""^content^"")
;;

let find_node_id id =
  let doc = Html.document in
  Js.Opt.get (doc##getElementById(Js.string id))
             (fun () -> assert false)
;;

let create_input_unit =
  let n = ref (-1) in
  fun () ->
  let doc = Html.document in
  let tarea = Html.createTextarea doc in
  tarea##rows <- 1;
  tarea##cols <- 35;
  tarea##id <- Js.string (incr n; "in_"^(string_of_int !n));
  let stdout = find_node_id "std_out" in
  Dom.appendChild stdout tarea;
;;

let read_function env args =
  Io.log "leia";
  create_input_unit ();
  Builtins.read_impl env args;
;;

let cm doc name =
  let node = find_node_id "code" in
  Js.Unsafe.fun_call
       (Js.Unsafe.variable "CodeMirror.fromTextArea")
       [|Js.Unsafe.inject node|]
;;

let on_load _ =
  let d = Html.document in
  let body = find_node_id "pbody" in
  let textbox = Html.createTextarea d in
  textbox##rows <- 20; textbox##cols <- 80;
  textbox##value <- Js.string "// Write here";
  textbox##id <- Js.string "code";
  let dsrc = create_div d "src"
  and dstd = create_div d "std"
  and dstd_hdr = Html.createH2 d
  and dstd_out = Html.createDiv d
  and derr = create_div d "err"
  and derr_hdr = Html.createH2 d
  and derr_out = Html.createDiv d  in
  dstd_out##id <- Js.string "std_out";
  derr_hdr##innerHTML <- Js.string "Error/Warning messages";
  dstd_hdr##innerHTML <- Js.string "Standard input/output";
  Dom.appendChild dsrc textbox;
  Dom.appendChild body dsrc;
  Dom.appendChild body dstd;
  Dom.appendChild dstd dstd_hdr;
  Dom.appendChild dstd dstd_out;
  Dom.appendChild body derr;
  Dom.appendChild derr derr_hdr;
  Dom.appendChild derr derr_out;
  let eval_button = Html.createButton ~name:(Js.string "Evaluate") d in
  eval_button##onclick <-
    Html.handler
      ( fun ev ->
        let text = Js.to_string (textbox##value) in
        parse_eval (Lexing.from_string text);
        set_content derr_out (err_cleared_contents ());
        set_content dstd_out (std_cleared_contents ());
        Html.stopPropagation ev; Js._true
      );

  Dom.appendChild dsrc eval_button;
(* (\*  ignore (cm d "code"); *\)
 *   let rec preview old_text n =
 *     let text = Js.to_string (textbox##value) in
 *     let n =
 *       if text <> old_text then begin
 *         begin
 *           try
 *         with _ -> () end;
 *         20
 *       end else
 *         max 0 (n - 1)
 *     in
 *     Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>=
 *       fun () -> preview text n
 *   in
 *   ignore (preview "" 0); *)
  Js._false
;;


let _ =
  out_init ();
  mk_html_newline err_out;
  mk_html_newline std_out;
  read_def.p_eval <- read_function;
  Html.window##onload <- Html.handler on_load;
;;

(*
 Local Variables:
 compile-command: "make js"
 End:
*)
