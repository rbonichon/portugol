%{
  open Base.Types ;;
  open Ast;;
  open Location ;;
  (* Localizing a symbol*)
  let symbol_rloc () = {
    loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
  };;


  let mk_program id vars fundefs commands = {
    a_id = id;
    a_variables = vars;
    a_functions = fundefs;
    a_body = commands;
    a_loc = symbol_rloc ();
  }
  ;;

  let mk_function id formals ret_type locals body = {
    fun_id = id;
    fun_formals = formals;
    fun_return_type = ret_type;
    fun_locals = locals;
    fun_body = body;
    fun_loc = symbol_rloc ();
  }
  ;;

  let mk_expr e = {
    e_desc = e;
    e_loc = symbol_rloc ();
  };;

  let mk_decls names ty =
    List.map
      (fun (name, loc) -> { var_id = name; var_type = ty; var_loc = loc; })
      names
  ;;

  let mk_by_refs vargs =
    List.map (fun x -> ByRef x) vargs
  ;;

  let mk_by_values vargs =
    List.map (fun x -> ByValue x) vargs
  ;;

  let mk_vname v = v, symbol_rloc () ;;

  let mk_binop b = {
    bop_desc = b;
    bop_loc = symbol_rloc ();
  } ;;

  let mk_lop op = mk_binop (Log op) ;;

  let mk_rop op = mk_binop (Rel op) ;;

  let mk_aop op = mk_binop (Arith op) ;;

  let mk_uop uop = {
    uop_desc = uop;
    uop_loc = symbol_rloc ();
  }
  ;;

  let mk_ulop op = mk_uop (ULog op) ;;
  let mk_uaop op = mk_uop (UArith op) ;;

%}

/* Parser tokens */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <int * int> IDXRANGE
%token <string> STRING
%token EQUAL
%token NEQUAL

%token BOR
%token BXOR
%token BAND
%token BNOT

%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token MINUS
%token PLUS
%token STAR SLASH BACKSLASH PERCENT
%token POW
%token COLON
%token WHILE DO ENDWHILE REPEAT
%token IF
%token THEN
%token ELSE
%token ENDIF ALGORITHM VAR START ENDALGORITHM

%token <string> IDENT
%token COMMA SEMICOMMA
%token LESS_MINUS
%token LPAREN LBRAC
%token RPAREN RBRAC
%token FUNCTION ENDFUNCTION
%token PROCEDURE ENDPROCEDURE
%token RETURN
%token SWITCH ENDSWITCH CASE DEFAULTCASE
%token TINT TBOOL TREAL TSTRING TVETOR OF DOTDOT
%token FOR ENDFOR TO
%token STEP
%token EOF



/*  Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%left EQUAL LESS GREATER NEQUAL LESS_EQUAL GREATER_EQUAL
%left BOR
%left BXOR
%left BAND
%left PLUS MINUS
%left STAR SLASH
%left BACKSLASH PERCENT
%nonassoc prec_unary_minus

/* The type of a program and the start of the parser */

%start entry
%type <Ast.algorithm> entry

%start toplevel
%type <Ast.expr> toplevel

%%

entry:
    | main { $1 }
;

main:
  | ALGORITHM STRING vars fundefs START cmds ENDALGORITHM EOF
              { mk_program $2 $3 $4 $6 }
  | ALGORITHM STRING vars START cmds ENDALGORITHM EOF
              { mk_program $2 $3 [] $5 }
;

fundefs:
  | fundef { [$1] }
  | fundef fundefs { $1 :: $2 }
;

fundef:
  | FUNCTION IDENT LPAREN formals RPAREN COLON ty
             vars START cmds ENDFUNCTION
             { mk_function $2 $4 $7 $8 $10}

  | PROCEDURE IDENT LPAREN formals RPAREN
             vars START cmds ENDPROCEDURE
             { mk_function $2 $4 TyUnit $6 $8 }
;

formals:
  | /* empty */ { [] }
  | params   { $1 }
;

params:
  | param      { $1 }
  | param SEMICOMMA params { $1 @ $3 }
;

param:
  | VAR vardecl    { mk_by_refs $2 }
  | vardecl        { mk_by_values $1 }
;

vars:
  | VAR vardecls   { $2 }
  | /* empty */    { [] }
;

vardecls:
  | /* empty */                { [] }
  | vardecl vardecls         { $1 @ $2 }
;

vardecl:
  | IDENT comma_vnames COLON ty   { mk_decls (mk_vname $1 :: $2) $4 }
;

toplevel:
  | cmd    { $1 }
  | expr   { $1 }
  | EOF    { raise End_of_file }
;

ty:
  | TINT       { TyInt }
  | TREAL      { TyReal }
  | TBOOL      { TyBool }
  | TSTRING    { TyString }
  | TVETOR LBRAC range RBRAC OF ty   { let i1, i2 = $3 in TyArray (i1, i2, $6)}
  | TVETOR LBRAC range COMMA range RBRAC OF ty
           { let r1, r2 = $3 and c1, c2 = $5 in
             TyArray (r1, r2, TyArray(c1, c2, $8))
           }
;

range:
  | INT DOTDOT INT { $1, $3}
  | IDXRANGE       { $1 }

comma_vnames:
  | /* empty */              { [] }
  | COMMA IDENT comma_vnames { mk_vname $2 :: $3 }
;

cmds:
  | /* empty */        { [] }
  | cmd cmds           { $1 :: $2}
;

simple_cmd:
  | IF expr THEN cmds ELSE cmds ENDIF
          { mk_expr (IfThenElse($2, $4, $6)) }
  | IF expr THEN cmds ENDIF
          { mk_expr (IfThenElse($2, $4, [])) }
  | WHILE expr DO cmds ENDWHILE
          { mk_expr (While($2, $4)) }
  | REPEAT cmds TO expr
          { mk_expr (Repeat($4,$2)) }
  | lval LESS_MINUS expr
          { mk_expr (Assigns($1, $3) ) }
  | FOR IDENT OF expr TO expr step DO cmds ENDFOR
          { mk_expr (For($2, $4, $6, $7, $9) ) }
  | RETURN expr
          { mk_expr (Return ($2)) }
  | SWITCH expr cases default_case ENDSWITCH
           { mk_expr (Switch ($2, $3 @ [$4])) }
;

cases:
  | /* empty */   { [] }
  | CASE expr comma_exprs cmds cases { ($2 :: $3, $4) :: $5 }
;

default_case:
  | DEFAULTCASE cmds { [], $2 }
;


step:
  | /* empty */   { 1 (* Default step is one *) }
  | STEP INT      { $2 }
;

cmd:
  | fcall { $1 }
  | simple_cmd { $1 }
;

fcall:
  | IDENT LPAREN RPAREN
          { mk_expr (Call($1, [])) }
  | IDENT LPAREN expr comma_exprs RPAREN
          { mk_expr (Call($1, $3 :: $4)) }
;

lval:
  | IDENT { Id $1}
  | IDENT LBRAC expr RBRAC { ArrayId($1, [$3]) }
  | IDENT LBRAC expr COMMA expr RBRAC { ArrayId($1, [$3; $5]) }
;

comma_exprs:
  | /* empty */              { [] }
  | COMMA expr comma_exprs   { $2 :: $3 }

expr:
  | INT                  { mk_expr (Int $1) }
  | FLOAT                { mk_expr (Real $1) }
  | IDENT                { mk_expr (Var $1) }
  | BOOL                 { mk_expr (Bool $1) }
  | STRING               { mk_expr (String $1) }
  | fcall                { $1 }
  | LPAREN expr RPAREN   { $2 }
  | IDENT LBRAC expr comma_exprs RBRAC
          { mk_expr (ArrayExpr($1, $3 :: $4))}
  | MINUS expr %prec prec_unary_minus
         { mk_expr (UnExpr(mk_uaop UMinus, $2)) }
  | BNOT expr %prec prec_unary_minus
         { mk_expr (UnExpr(mk_ulop Bnot, $2)) }
  | expr PLUS expr { mk_expr (BinExpr(mk_aop Plus, $1, $3)) }
  | expr MINUS expr { mk_expr (BinExpr(mk_aop Minus, $1, $3)) }
  | expr SLASH expr { mk_expr (BinExpr(mk_aop Div, $1, $3)) }
  | expr BACKSLASH expr { mk_expr (BinExpr(mk_aop EDiv, $1, $3)) }
  | expr STAR expr { mk_expr (BinExpr(mk_aop Mult, $1, $3)) }
  | expr POW expr { mk_expr (Call("exp", [$1; $3;])) }
  | expr PERCENT expr { mk_expr (BinExpr(mk_aop Mod, $1, $3)) }
  | expr BAND  expr { mk_expr (BinExpr(mk_lop Band, $1, $3)) }
  | expr BXOR  expr { mk_expr (BinExpr(mk_lop Bxor, $1, $3)) }
  | expr BOR  expr { mk_expr (BinExpr(mk_lop Bor, $1, $3)) }
  | expr EQUAL expr { mk_expr (BinExpr(mk_rop Eq, $1, $3)) }
  | expr NEQUAL expr { mk_expr (BinExpr(mk_rop NotEq, $1, $3)) }
  | expr GREATER expr { mk_expr (BinExpr(mk_rop Gt, $1, $3)) }
  | expr LESS expr { mk_expr (BinExpr(mk_rop Lt, $1, $3)) }
  | expr GREATER_EQUAL expr { mk_expr (BinExpr(mk_rop Gte, $1, $3)) }
  | expr LESS_EQUAL expr { mk_expr (BinExpr(mk_rop Lte, $1, $3)) }
;
