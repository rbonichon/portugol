{
  open Format
  open Lexing
  open Parser
  ;;

  let keywords = [
    ("algoritmo", ALGORITHM);
    ("fimalgoritmo", ENDALGORITHM);
    ("var", VAR);
    ("funcao", FUNCTION);
    ("fimfuncao", ENDFUNCTION);
    ("procedimento", PROCEDURE);
    ("fimprocedimento", ENDPROCEDURE);
    ("retorne", RETURN);
    ("inicio", START);
    ("se", IF);
    ("entao", THEN);
    ("senao", ELSE);
    ("fimse", ENDIF);
    ("enquanto", WHILE);
    ("faca", DO);
    ("fimenquanto", ENDWHILE);
    ("repita", REPEAT);
    ("escolha", SWITCH);
    ("fimescolha", ENDSWITCH);
    ("outrocaso", DEFAULTCASE);
    ("caso", CASE);
    ("nao", BNOT);
    ("e", BAND);
    ("ou", BOR);
    ("xou", BXOR);
    ("inteiro", TINT);
    ("real", TREAL);
    ("caractere", TSTRING);
    ("caracteres", TSTRING);
    ("texto", TSTRING);
    ("logico", TBOOL);
    ("vetor", TVETOR);
    ("de", OF);
    ("para", FOR);
    ("ate", TO);
    ("fimpara", ENDFOR);
    ("passo", STEP);
    ("incluia", IMPORT);
  ];;

  let keyword_table =
    let len = List.length keywords in
    let h = Hashtbl.create len in
    List.iter
      (fun (s, k) -> Hashtbl.add h s k ) keywords;
    h
  ;;

(* To buffer string literals *)

let string_buffer = Buffer.create 256

let reset_string_buffer () =
  Buffer.reset string_buffer

let store_string_char c = Buffer.add_char string_buffer c

let get_stored_string () =
  let s = Buffer.contents string_buffer in
  reset_string_buffer ();
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

}

(* Some know regular expressions *)
let newline = ('\010' | '\013' | "\013\010")
let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
  | space* newline   { Lexing.new_line lexbuf; token lexbuf }
  | space+    { token lexbuf }
  | "//"      { comment lexbuf; (* See the comment rule below  *)
                Lexing.new_line lexbuf;
                token lexbuf }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRAC }
  | ']'       { RBRAC }
  | ".."      { DOTDOT }
  | "verdadeiro"    { BOOL(true) }
  | "falso"   { BOOL(false) }
  | '-'       { MINUS }
  | '+'       { PLUS }
  | '^'       { POW }
  | '*'       { STAR }
  | '/'       { SLASH }
  | "\\"      { BACKSLASH }
  | '%'       { PERCENT }
  | '='       { EQUAL }
  | "<>"      { NEQUAL }
  | "<="      { LESS_EQUAL }
  | ">="      { GREATER_EQUAL }
  | '<'       { LESS }
  | '>'       { GREATER }
  | "<-"      { LESS_MINUS }
  | ','       { COMMA }
  | ':'       { COLON }
  | ';'       { SEMICOMMA }
  | digit+ ".." digit+
      { let lex_str = Lexing.lexeme lexbuf in
        let pt_idx = String.index lex_str '.'
        and len = String.length lex_str in
        let aidx1 = int_of_string (String.sub lex_str 0 pt_idx)
        and aidx2 =
          int_of_string (String.sub lex_str (pt_idx + 2) (len - pt_idx - 2)) in
        IDXRANGE(aidx1, aidx2)
      }
  | digit+    { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
              { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.none;
        string lexbuf;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string()) }

  | lower (digit|lower|upper| '_')*
  | '_'       {
        let s = Lexing.lexeme lexbuf in
        try
            Hashtbl.find keyword_table s
        with Not_found ->
            IDENT s
      }
  | eof       { EOF }
  | _
      { let msg = sprintf "@[Bad character %c@]" (Lexing.lexeme_char lexbuf 0) in
        raise (Io.Error.Lex_error msg)
      }

and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | eof
      { raise Not_found }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and comment = parse
| newline
    { () }
| eof
    { Format.eprintf "Warning: unterminated comment@." }
| _
    { comment lexbuf }
