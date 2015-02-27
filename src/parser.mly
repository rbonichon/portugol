%{
  open Types ;;
  open Ast;;
  open Location ;;
  open Lexing ;;
  (* Localizing a symbol*)
  let symbol_rloc () = {
    loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
  };;


  let mk_module (vars, includes, fundefs) =
    let lib_loc = symbol_rloc () in
    { lib_functions = fundefs;
      lib_includes = includes;
      lib_variables = vars;
      lib_loc;
      lib_id =  lib_loc.loc_start.pos_fname;
    }
  ;;

  let mk_program id vars includes fundefs commands = {
    a_id = id;
    a_variables = vars;
    a_functions = fundefs;
    a_body = commands;
    a_includes = includes;
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
%token IMPORT
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

%left BOR
%left BXOR
%left BAND
%left EQUAL LESS GREATER NEQUAL LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left STAR SLASH BACKSLASH PERCENT POW
%nonassoc BNOT
%nonassoc prec_unary_minus

/* The type of a program and the start of the parser */

%start entry
%type <Ast.algorithm> entry

%start toplevel
%type <Ast.expr> toplevel

%start library
%type <Ast.library> library

%%

entry:
 | main { $1 }
;

main:
 | ALGORITHM name=STRING; prelude=prelude; START cmds=cmd*; ENDALGORITHM EOF
 { let vars, incls, mods = prelude in
   mk_program name vars incls mods cmds }
;

library:
| pmodule=prelude; EOF { mk_module pmodule }
;

prelude:
| vars=loption(vars); includes=import*; fundefs=fundef*;
    { vars, includes, fundefs }
;

fundef:
| FUNCTION fname=IDENT;
  formals=delimited(LPAREN, separated_list(SEMICOMMA, param), RPAREN);
  COLON rtype=ty; vars=loption(vars); START cmds=cmd*; ENDFUNCTION
 { mk_function fname (List.flatten formals) rtype vars cmds}

| PROCEDURE fname=IDENT
  LPAREN formals=separated_list(SEMICOMMA, param); RPAREN
  vars=loption(vars); START cmds=cmd*; ENDPROCEDURE
 { mk_function fname (List.flatten formals) TyUnit vars cmds }
;

import:
| IMPORT filename=STRING { filename }
;

param:
  | VAR vardecl    { mk_by_refs $2 }
  | vardecl        { mk_by_values $1 }
;

vars:
  | vars=preceded(VAR, vardecl*); { List.flatten vars }
;

vardecl:
  | vnames=separated_nonempty_list(COMMA, IDENT); COLON vtype=ty;
   { mk_decls (List.map mk_vname vnames) vtype }
;

toplevel:
  | cmd EOF   { $1 }
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
;

simple_cmd:
  | IF e=expr; THEN cmds_if=cmd+; ELSE cmds_else=cmd+; ENDIF
          { mk_expr (IfThenElse(e, cmds_if, cmds_else)) }
  | IF e=expr; THEN cmds=cmd+; ENDIF
          { mk_expr (IfThenElse(e, cmds, [])) }
  | WHILE e=expr; DO cmds=cmd+; ENDWHILE
          { mk_expr (While(e, cmds)) }
  | REPEAT cmds=cmd+; TO e=expr;
          { mk_expr (Repeat(e, cmds)) }
  | name=lval; LESS_MINUS e=expr;
          { mk_expr (Assigns(name, e) ) }
  | FOR vname=IDENT; OF init=expr; TO limit=expr; step=option(step);
    DO cmds=cmd+; ENDFOR
          { let stepval = match step with None -> 1 | Some v -> v in
            mk_expr (For(vname, init, limit, stepval, cmds)) }
  | RETURN e=expr;
          { mk_expr (Return (e)) }
  | SWITCH e=expr; cases=case+; default=default_case; ENDSWITCH
           { mk_expr (Switch (e, cases @ [default])) }
;

case:
| CASE exprs=separated_nonempty_list(COMMA, expr); cmds=cmd+;
  { (exprs, cmds) }
;

default_case:
  | DEFAULTCASE cmds=cmd+; { [], cmds }
;

step:
  | STEP stepval=INT;      { stepval }
;

cmd:
  | fcall { $1 }
  | simple_cmd { $1 }
;

fcall:
| fname=IDENT; exprs=delimited(LPAREN, separated_list(COMMA,expr), RPAREN);
        { mk_expr (Call(fname, exprs)) }
;

lval:
  | IDENT { Id $1}
  | id=IDENT; idxs=delimited(LBRAC, separated_nonempty_list(COMMA, expr),RBRAC);
    { ArrayId(id, idxs) }
;

expr:
  | INT                  { mk_expr (Int $1) }
  | FLOAT                { mk_expr (Real $1) }
  | IDENT                { mk_expr (Var $1) }
  | BOOL                 { mk_expr (Bool $1) }
  | STRING               { mk_expr (String $1) }
  | fcall                { $1 }
  | LPAREN e=expr; RPAREN   { e }
  | id=IDENT; LBRAC exprs=separated_nonempty_list(COMMA,expr); RBRAC
          { mk_expr (ArrayExpr(id, exprs))}
  | MINUS expr %prec prec_unary_minus
         { mk_expr (UnExpr(mk_uaop UMinus, $2)) }
  | BNOT expr
         { mk_expr (UnExpr(mk_ulop Bnot, $2)) }
  | e1=expr; PLUS          e2=expr; { mk_expr (BinExpr(mk_aop Plus, e1, e2)) }
  | e1=expr; MINUS         e2=expr; { mk_expr (BinExpr(mk_aop Minus, e1, e2)) }
  | e1=expr; SLASH         e2=expr; { mk_expr (BinExpr(mk_aop Div, e1, e2)) }
  | e1=expr; BACKSLASH     e2=expr; { mk_expr (BinExpr(mk_aop EDiv, e1, e2)) }
  | e1=expr; STAR          e2=expr; { mk_expr (BinExpr(mk_aop Mult, e1, e2)) }
  | e1=expr; POW           e2=expr; { mk_expr (Call("exp", [e1; e2;])) }
  | e1=expr; PERCENT       e2=expr; { mk_expr (BinExpr(mk_aop Mod, e1, e2)) }
  | e1=expr; BAND          e2=expr; { mk_expr (BinExpr(mk_lop Band, e1, e2)) }
  | e1=expr; BXOR          e2=expr; { mk_expr (BinExpr(mk_lop Bxor, e1, e2)) }
  | e1=expr; BOR           e2=expr; { mk_expr (BinExpr(mk_lop Bor, e1, e2)) }
  | e1=expr; EQUAL         e2=expr; { mk_expr (BinExpr(mk_rop Eq, e1, e2)) }
  | e1=expr; NEQUAL        e2=expr; { mk_expr (BinExpr(mk_rop NotEq, e1, e2)) }
  | e1=expr; GREATER       e2=expr; { mk_expr (BinExpr(mk_rop Gt, e1, e2)) }
  | e1=expr; LESS          e2=expr; { mk_expr (BinExpr(mk_rop Lt, e1, e2)) }
  | e1=expr; GREATER_EQUAL e2=expr; { mk_expr (BinExpr(mk_rop Gte, e1, e2)) }
  | e1=expr; LESS_EQUAL    e2=expr; { mk_expr (BinExpr(mk_rop Lte, e1, e2)) }
;
