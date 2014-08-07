open Format ;;
open Builtins ;;
open Lwt ;;

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
and warn_out = Format.formatter_of_buffer error_buffer
;;

let mk_html_newline fmt =
  let pp_out_string os s n1 n2 =
    let s' = Format.sprintf "%s" s in
    os s' n1 (String.length s')
  in
  let nl () = Format.fprintf fmt ".bab." in
  let outfuns = pp_get_formatter_out_functions fmt () in
  pp_set_formatter_out_functions
    fmt
    { outfuns with
      out_string = pp_out_string outfuns.out_string;
      out_newline = nl;
    }
;;


let out_init () =
  (* Redirects formatters to buffers *)
  Io.set_formatter Io.warning_output err_out;
  Io.set_formatter Io.res_output std_out;
  Io.set_formatter Io.error_output err_out;
  (* Deactivates output tags *)
  Io.set_tagging false ;
;;

let parse_eval lexbuf =
  try
    (*    ignore(Parsing.set_trace true); *)
    let program = Parser.entry Lexer.token lexbuf in
    Analyze_variables.Undeclared.run program;
    Analyze_variables.Unused.run program;
    Io.debug "Typing program";
    (* Type-check the program *)
    ignore (Typer.eval program);

    Interp.eval program;
  with
  | Parsing.Parse_error ->
     let _pos = Parsing.symbol_start_pos () in
     Io.error "Parse error@.";
  | _ -> ()
;;

let (>>=) = Lwt.bind ;;

let create_div d name =
  let div = Html.createDiv d in
  div##className <- Js.string "panel";
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

let set_read_buffer, get_read_buffer =
  let b = Buffer.create 2048 in
  (fun s -> Buffer.add_string b s),
  (fun () -> cleared_contents b )
;;



let read_function =
  let n = ref (-1) in
  fun env args ->
   args >>= fun fargs ->
   let c = Lwt_condition.create () in
   let doc = Html.document in
   let basename = incr n; "in_"^(string_of_int !n) in
   let tarea = Html.createTextarea doc
   and li = Html.createLi doc
   and entry_but = Html.createButton ~name:(Js.string ("enter_"^basename)) doc
   and stdout = find_node_id "std_out" in
   li##className <- Js.string "stdout_elt";
   let nargs = List.length fargs in
   let tmsg = Printf.sprintf "Entre %n valores" nargs in
   let msglen = String.length tmsg in
   tarea##rows <- 1;
   tarea##cols <- msglen + 5 * nargs;
   tarea##id <- Js.string basename;
   tarea##className <- Js.string "ic";
   tarea##placeholder <- Js.string tmsg ;
   entry_but##innerHTML <- Js.string "Entrar";
   Dom.appendChild stdout li;
   Dom.appendChild li tarea;
   Dom.appendChild li entry_but;
   entry_but##onclick <-
     Html.handler
       ( fun ev ->
         set_read_buffer (Js.to_string (tarea##value));
         tarea##readOnly <- Js._true;
         Lwt_condition.signal c true;
         Dom.removeChild li entry_but;
         Html.stopPropagation ev; Js._true
       );
   Io.log "Appended child";
   let rec read_entry () =
     Lwt_condition.wait c >>= fun _ -> Lwt.return (get_read_buffer ())
   in Builtins.read_impl read_entry env args
;;



let initial_program =
  "algoritmo \"Test\"\n\
   var x, y : inteiro\n\
   inicio\n\
     escreva(\"Hello\")\n\
     escreva(\"Entre com um inteiro\")\n\
     leia(x, y)\n\
     escreva(\"Would it be: \", x, \"?\", y)\n\
  fimalgoritmo "
;;

let initial_program =
 "algoritmo \"Aleatorio\"\n\
  var a, b, c, res : real\n\
  inicio\n\
        \tleia(a, b)\n\
        c <- rand()\n\
        res <- a + c * (b - a)\n\
        escreva(res)\n\
fimalgoritmo\
"



let stdOut text =
  let d = Html.document in
  let li = Html.createLi d in
  let ulout = find_node_id "std_out" in
  li##className <- Js.string "stdout_elt";
  li##innerHTML <- Js.string text;
  Dom.appendChild ulout li;
;;

let print_function pfun env args =
    Io.log "print";
    pfun env args >>= fun (e, v) ->
    let t = std_cleared_contents () in
    stdOut t;
    return (e, v)
;;

let document = Html.document ;;

let append_text e s =
  Dom.appendChild e (document##createTextNode (Js.string s))
;;

let on_load _ =
  let d = document in

  let mkContainer () =
    let container = Html.createDiv d in
    container##className <- Js.string "container";
    container
  in

  let mkPanel title =
    let panel = Html.createDiv d in
    let panel_title = Html.createDiv d in
    let panel_content = Html.createDiv d in
    let h2 = Html.createH4 d in
    h2##innerHTML <- Js.string title;
    panel##className <- Js.string "panel panel-default";
    panel_title##className <- Js.string "panel-heading";
    panel_content##className <- Js.string "panel-body";
    let panel_content = Html.createDiv d in
    Dom.appendChild panel_title h2;
    Dom.appendChild panel panel_title;
    Dom.appendChild panel panel_content;
    panel, panel_content
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

  let dsrc, dsrc_contents = mkPanel "Código"
  and dstd, dstd_out = mkPanel "Tela"
  and derr, derr_out = mkPanel "Avisos e erros" in

  Dom.appendChild container dsrc;
  Dom.appendChild container dstd;
  Dom.appendChild container derr;

  let ulout = Html.createUl d in
  Dom.appendChild dstd_out ulout;
  ulout##id <- Js.string "std_out";

  let editor  = Html.createTextarea d in
  editor##value <- Js.string initial_program;
  Dom.appendChild dsrc_contents editor;
  let mkCodeMirror (id: string) =
    Js.Unsafe.fun_call
      ((Js.Unsafe.variable "CodeMirror")##fromTextArea)
      [| Js.Unsafe.inject editor;
         Js.Unsafe.obj
           [| ("lineNumbers", Js.Unsafe.inject Js._true);
              ("mode", Js.Unsafe.inject (Js.string "text/x-portugol"));
              ("theme", Js.Unsafe.inject (Js.string "solarized light"));
             |]
        |]
  in

  let cm_editor =  mkCodeMirror "code" in

  let clean_outputs () =
    let es = Js.string "" in
    ulout##innerHTML <- es;
    derr_out##innerHTML <- es;
  in

  let errOut () =
    derr_out##innerHTML <- Js.string (err_cleared_contents ())
  in

  let eval_button =
    Html.createButton
      ~_type:(Js.string "button")
      ~name:(Js.string "eval") d
  in
  eval_button##innerHTML <- Js.string "Executar";
  eval_button##className <- Js.string "btn btn-primary";
  eval_button##onclick <-
    Html.handler
      ( fun ev ->
        clean_outputs ();
        let text = Js.to_string
                     (Js.Unsafe.fun_call
                        cm_editor##getValue
                        [| Js.Unsafe.inject (Js.string " ") |])
        in
        parse_eval (Lexing.from_string text);
        errOut ();
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
       ignore(Js.Unsafe.fun_call (Js.Unsafe.coerce cm_editor)##setValue [| Js.Unsafe.inject
        (Js.string "") |]) ;
       (* textbox##value <- Js.string "" ;*)
       clean_outputs ();
       Html.stopPropagation ev; Js._true
      );

  let save_button =
    Html.createButton
      ~_type:(Js.string "button") ~name:(Js.string "save") d
  in

  save_button##innerHTML <- Js.string "Salvar";
  save_button##className <- Js.string "btn btn-primary";
  save_button##onclick <-
    Html.handler
      (fun ev ->
       let content = Js.Unsafe.fun_call cm_editor##getValue [| |] in
       let uriContent =
         Js.string ("data:text/x-portugol," ^
                    (Js.to_string (Js.encodeURI content))) in
       let _ = Html.window##open_(uriContent, Js.string "foo.alg", Js.null) in
       Html.window##close ();
       Html.stopPropagation ev; Js._true
      );

  let actions, actions_contents = mkPanel "Ações" in
  let dbuttons = Html.createDiv d in
  dbuttons##className <- Js.string "btn-group";
  dbuttons##id <- Js.string "buttons";
  Dom.appendChild dsrc actions;
  Dom.appendChild actions_contents dbuttons;
  Dom.appendChild dbuttons eval_button;
  Dom.appendChild dbuttons clear_button;
  Dom.appendChild dbuttons save_button;

  let prefs, prefs_contents = mkPanel "Preferências" in
  let mode_selector = Html.createSelect d in
  mode_selector##className <- Js.string "form-control";
  let option = Html.createOption d in
  append_text option "Escolha um tema";
  Dom.appendChild mode_selector option;
  let modes = ["solarized light"; "solarized dark"; ] in
  List.iter
    (fun mode ->
     let option = Html.createOption d in
     append_text option mode;
     Dom.appendChild mode_selector option;
    ) modes;
  mode_selector##onchange <-
    Html.handler
      (fun _ ->
       let i = mode_selector##selectedIndex - 1 in
       if i >= 0 && i < List.length modes then
         ignore (Js.Unsafe.fun_call
                   cm_editor##setOption
                   [|  Js.Unsafe.inject (Js.string "theme");
                       Js.Unsafe.inject (Js.string (List.nth modes i));
                      |]);
       Js._false;
    );
  Dom.appendChild prefs_contents mode_selector;
  Dom.appendChild dsrc prefs;
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
