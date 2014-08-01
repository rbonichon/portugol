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
  let pp_out_string os s n1 n2 =
    let s' = Format.sprintf "<span>%s</span>" s in
    os s' n1 (String.length s')
  in
  let outfuns = pp_get_formatter_out_functions fmt () in
  pp_set_formatter_out_functions
    fmt
    { outfuns with
      out_newline = nl ;
      out_string = pp_out_string outfuns.out_string;
    }
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
  | _ -> ()
;;

(* let (>>=) = Lwt.bind ;; *)

let create_div d name =
  let div = Html.createDiv d in
  div##style##border <- Js.string "1px black dashed";
  div##style##padding <- Js.string "5px";
  div##id <- Js.string name;
  div
;;


let find_node_id id =
  let doc = Html.document in
  Js.Opt.get (doc##getElementById(Js.string id))
             (fun () -> assert false)
;;

let append_out_text d text =
  let doc = Html.document in
  let div = Html.createDiv doc in
  div##innerHTML <- Js.string text;
  Dom.appendChild d div;
  Io.log "Text appended";
;;

let entry_done = ref false ;;

let set_read_buffer, get_read_buffer =
  let b = Buffer.create 2048 in
  (fun s -> Buffer.add_string b s),
  (fun () -> cleared_contents b )
;;

let input_elt, output_elt =
  let n = ref (-1) in
  (fun () ->
    let doc = Html.document in
    let basename = incr n; "in_"^(string_of_int !n) in
    let tarea = Html.createTextarea doc
    and div = Html.createDiv doc
    and entry_but = Html.createButton ~name:(Js.string ("enter_"^basename)) doc
    and stdout = find_node_id "std_out" in
    tarea##rows <- 3;
    tarea##cols <- 35;
    tarea##id <- Js.string basename;
    tarea##value <- Js.string "here";
    entry_but##innerHTML <- Js.string "Entrar";
    entry_but##onclick <-
      Html.handler
        ( fun ev ->
          set_read_buffer (Js.to_string (tarea##value));
          Html.stopPropagation ev; Js._true
        );

    Dom.appendChild stdout div;
    Dom.appendChild div tarea;
    Dom.appendChild div entry_but;
    Io.log "Appended child";
  ),
  (fun text ->
   let doc = Html.document in
   let span = Html.createSpan doc
   and div = Html.createDiv doc in
   div##className <- Js.string "stdout_elt";
   span##innerHTML <- Js.string text;
   Dom.appendChild div span;
   let stdout = find_node_id "std_out" in
   Dom.appendChild stdout div;
  )
;;

let read_function env args =
  Io.log "leia";
  input_elt ();
  let read_entry () = "1"
  in
  Builtins.read_impl read_entry env args
;;

let print_function pfun env args =
  Io.log "print";
  let e, v = pfun env args in
  let t = std_cleared_contents () in
  output_elt t;
  e, v
;;

let initial_program =
  "algoritmo \"Test\"\n\
   var x : inteiro\n\
   inicio\n\
     escreva(\"Hello\")\n\
     escreva(\"Entre com um inteiro\")\n\
     leia(x)\n\
     escreva(\"Would it be: \", x, \"?\")\n\
  fimalgoritmo "
;;

let on_load _ =
  let d = Html.document in
  let mkContainer () =
    let container = Html.createDiv d in
    container##className <- Js.string "container";
    container
  in
  let body = find_node_id "pbody" in
  let header = Html.createDiv d in
  header##className <- Js.string "navbar navbar-static-top";
  Dom.appendChild body header;
  let c1 = mkContainer () in
  Dom.appendChild header c1;
  let navbar_hdr = Html.createDiv d in
  navbar_hdr##className <- Js.string "navbar-header";
  Dom.appendChild c1 navbar_hdr;
  let a = Html.createA d in
  a##className <- Js.string "navbar-brand";
  a##innerHTML <- Js.string "Portugol";
  Dom.appendChild navbar_hdr a;
  let container = mkContainer () in
  Dom.appendChild body container;
  let textbox = Html.createTextarea d in
  textbox##rows <- 20; textbox##cols <- 80;
  textbox##value <- Js.string initial_program;
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
  Dom.appendChild container dsrc;
  Dom.appendChild container dstd;
  Dom.appendChild dstd dstd_hdr;
  Dom.appendChild dstd dstd_out;
  Dom.appendChild container derr;
  Dom.appendChild derr derr_hdr;
  Dom.appendChild derr derr_out;
  let clean_outputs () =
    let es = Js.string "" in
    dstd_out##innerHTML <- es;
    derr_out##innerHTML <- es;
  in
  let eval_button =
    Html.createButton
      ~_type:(Js.string "button")
      ~name:(Js.string "eval") d
  in
  eval_button##innerHTML <- Js.string "Avaliar algoritmo";
  eval_button##className <- Js.string "btn btn-primary";
  eval_button##onclick <-
    Html.handler
      ( fun ev ->
        clean_outputs ();
        let text = Js.to_string (textbox##value) in
        parse_eval (Lexing.from_string text);
        Html.stopPropagation ev; Js._true
      );
  let clear_button =
    Html.createButton
      ~_type:(Js.string "button") ~name:(Js.string "clear") d
  in
  clear_button##innerHTML <- Js.string "Limpar";
  clear_button##className <- Js.string "btn btn-primary";
  clear_button##onclick <-
    Html.handler
      (fun ev ->
       textbox##value <- Js.string "" ;
       clean_outputs ();
       Html.stopPropagation ev; Js._true
      );
  Dom.appendChild dsrc eval_button;
  Dom.appendChild dsrc clear_button;
  Js._false
;;


let _ =
  out_init ();
  mk_html_newline err_out;
  mk_html_newline std_out;
  (* Redirect I/O *)
  print_def.p_eval <- print_function print_def.p_eval;
  println_def.p_eval <- print_function println_def.p_eval;
  read_def.p_eval <- read_function;
  Html.window##onload <- Html.handler on_load;
;;

(*
 Local Variables:
 compile-command: "make js"
 End:
*)
