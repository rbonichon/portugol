%{
  open Types ;;
  open Ast;;
  open Location ;;
  open Lexing ;;

  let mk_loc loc_start loc_end = { loc_start; loc_end; } ;;

  let mk_module (vars, includes, fundefs) lib_loc =
    { lib_functions = fundefs;
      lib_includes = includes;
      lib_variables = vars;
      lib_loc;
      lib_id = lib_loc.loc_start.pos_fname;
    }
  ;;

  let mk_program id vars includes fundefs commands loc = {
    a_id = id;
    a_variables = vars;
    a_functions = fundefs;
    a_body = commands;
    a_includes = includes;
    a_loc = loc;
  }
  ;;

  let mk_function id formals ret_type locals body fun_loc = {
    fun_id = id;
    fun_formals = formals;
    fun_return_type = ret_type;
    fun_locals = locals;
    fun_body = body;
    fun_loc;
  }
  ;;

  let mk_expr e e_loc = {
    e_desc = e;
    e_loc;
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

  let mk_vname vloc v = v, vloc ;;

  let mk_binop b bop_loc = {
    bop_desc = b;
    bop_loc;
  } ;;

  let mk_lop op = mk_binop (Log op) ;;

  let mk_rop op = mk_binop (Rel op) ;;

  let mk_aop op = mk_binop (Arith op) ;;

  let mk_uop uop uop_loc = {
    uop_desc = uop;
    uop_loc;
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
    let loc = mk_loc $startpos $endpos in
    mk_program name vars incls mods cmds loc }
;

library:
| pmodule=prelude; EOF {
  let loc = mk_loc $startpos $endpos in  mk_module pmodule loc
  }
;

prelude:
| vars=loption(vars); includes=import*; fundefs=fundef*;
    { vars, includes, fundefs }
;

fundef:
| FUNCTION fname=IDENT;
  formals=delimited(LPAREN, separated_list(SEMICOMMA, param), RPAREN);
  COLON rtype=ty; vars=loption(vars); START cmds=cmd*; ENDFUNCTION
 { let loc = mk_loc $startpos $endpos in
   mk_function fname (List.flatten formals) rtype vars cmds loc }

| PROCEDURE fname=IDENT
  LPAREN formals=separated_list(SEMICOMMA, param); RPAREN
  vars=loption(vars); START cmds=cmd*; ENDPROCEDURE
 { let loc = mk_loc $startpos $endpos in
   mk_function fname (List.flatten formals) TyUnit vars cmds loc }
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
   { let loc = mk_loc $startpos $endpos
     in mk_decls (List.map (mk_vname loc) vnames) vtype }
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
    { let loc = mk_loc $startpos $endpos in
      mk_expr (IfThenElse(e, cmds_if, cmds_else)) loc }
  | IF e=expr; THEN cmds=cmd+; ENDIF
    { let loc = mk_loc $startpos $endpos in
      mk_expr (IfThenElse(e, cmds, [])) loc }
  | WHILE e=expr; DO cmds=cmd+; ENDWHILE
     { let loc = mk_loc $startpos $endpos in mk_expr (While(e, cmds)) loc }
  | REPEAT cmds=cmd+; TO e=expr;
     { let loc = mk_loc $startpos $endpos in mk_expr (Repeat(e, cmds)) loc }
  | name=lval; LESS_MINUS e=expr;
     { let loc = mk_loc $startpos $endpos in mk_expr (Assigns(name, e)) loc }
  | FOR vname=IDENT; OF init=expr; TO limit=expr; step=option(step);
    DO cmds=cmd+; ENDFOR
          { let loc = mk_loc $startpos $endpos in
            let stepval =
              match step with None -> mk_expr (Int 1) loc | Some v -> v in
            mk_expr (For(vname, init, limit, stepval, cmds)) loc }
  | RETURN e=expr;
          { let loc = mk_loc $startpos $endpos in mk_expr (Return (e)) loc }
  | SWITCH e=expr; cases=case+; default=default_case; ENDSWITCH
          { let loc = mk_loc $startpos $endpos in
           mk_expr (Switch (e, cases @ [default])) loc }
;

case:
| CASE exprs=separated_nonempty_list(COMMA, expr); cmds=cmd+;
  { (exprs, cmds) }
;

default_case:
  | DEFAULTCASE cmds=cmd+; { [], cmds }
;

step:
  | STEP stepval=expr;      { stepval }
;

cmd:
  | fcall { $1 }
  | simple_cmd { $1 }
;

fcall:
| fname=IDENT; exprs=delimited(LPAREN, separated_list(COMMA,expr), RPAREN);
 { let loc = mk_loc $startpos $endpos in mk_expr (Call(fname, exprs)) loc }
;

lval:
  | IDENT { Id $1}
  | id=IDENT; idxs=delimited(LBRAC, separated_nonempty_list(COMMA, expr),RBRAC);
    { ArrayId(id, idxs) }
;

expr:
  | INT                  { let loc = mk_loc $startpos $endpos in mk_expr (Int $1) loc }
  | FLOAT                { let loc = mk_loc $startpos $endpos in mk_expr (Real $1) loc }
  | IDENT                { let loc = mk_loc $startpos $endpos in mk_expr (Var $1) loc }
  | BOOL                 { let loc = mk_loc $startpos $endpos in mk_expr (Bool $1) loc }
  | STRING               { let loc = mk_loc $startpos $endpos in mk_expr (String $1) loc }
  | f=fcall;                { f }
  | LPAREN e=expr; RPAREN   { e }
  | id=IDENT; LBRAC exprs=separated_nonempty_list(COMMA,expr); RBRAC
          { let loc = mk_loc $startpos $endpos in  mk_expr (ArrayExpr(id, exprs)) loc}
  | MINUS e=expr; %prec prec_unary_minus
         { let loc = mk_loc $startpos $endpos in mk_expr (UnExpr(mk_uaop UMinus loc, e)) loc }
  | BNOT e=expr;
         { let loc = mk_loc $startpos $endpos in mk_expr (UnExpr(mk_ulop Bnot loc, e)) loc }
  | e1=expr; _op=PLUS;          e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_aop Plus oploc,  e1, e2)) eloc }
  | e1=expr; _op=MINUS;         e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_aop Minus oploc, e1, e2)) eloc }
  | e1=expr; _op=SLASH;         e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
     mk_expr (BinExpr(mk_aop Div oploc,   e1, e2)) eloc }
  | e1=expr; _op=BACKSLASH;     e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
     mk_expr (BinExpr(mk_aop EDiv oploc,  e1, e2)) eloc }
  | e1=expr; _op=STAR;          e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_aop Mult oploc,  e1, e2)) eloc }
  | e1=expr; _op=PERCENT;       e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_aop Mod oploc,   e1, e2)) eloc }
  | e1=expr; _op=BAND;          e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_lop Band oploc,  e1, e2)) eloc }
  | e1=expr; _op=BXOR;          e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_lop Bxor oploc,  e1, e2)) eloc }
  | e1=expr; _op=BOR;           e2=expr;
    { let eloc = mk_loc $startpos $endpos in
      let oploc = mk_loc $startpos(_op) $endpos(_op) in
      mk_expr (BinExpr(mk_lop Bor oploc,   e1, e2)) eloc }
  | e1=expr; _op=EQUAL;         e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_rop Eq oploc,    e1, e2)) eloc }
  | e1=expr; _op=NEQUAL;        e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
     mk_expr (BinExpr(mk_rop NotEq oploc, e1, e2)) eloc }
  | e1=expr; _op=GREATER;       e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_rop Gt oploc,    e1, e2)) eloc }
  | e1=expr; _op=LESS;          e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_rop Lt oploc,    e1, e2)) eloc }
  | e1=expr; _op=GREATER_EQUAL; e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_rop Gte oploc,   e1, e2)) eloc }
  | e1=expr; _op=LESS_EQUAL;    e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    let oploc = mk_loc $startpos(_op) $endpos(_op) in
    mk_expr (BinExpr(mk_rop Lte oploc,   e1, e2)) eloc }
  | e1=expr; POW;           e2=expr;
  { let eloc = mk_loc $startpos $endpos in
    mk_expr (Call("exp", [e1; e2;])) eloc }
;
