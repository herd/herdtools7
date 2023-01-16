(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)


(*
  Goals:
    - Every valid ASLv1 program is accepted by this parser.
    - No warnings should be emitted by menhir.
    - Being somewhat readable

  Non-goals:
    - Having a 1-to-1 representations of the BNF rules.
    - Constructing a representative AST of the input program.
    - Being the reference parser of ASL.

  Notations:
    - [unimplemented_XXX] discards the production by the rule and returns a
      dummy value.

  Notes:
    - Usually, big blocks where all rules end with <> are not implemented in
      the AST yet.

 *)


(* ------------------------------------------------------------------------

                                   Helpers

  ------------------------------------------------------------------------- *)

%{

let func (name, args, return_type, body) =
  AST.(D_Func { name; args; return_type; body })

let getter (name, args, return_type, body) =
  let name = ASTUtils.getter_name name
  and return_type = Some(return_type) in
  AST.(D_Func { name; args; return_type; body })

let setter (name, args, new_val, body) =
  let args = new_val :: args
  and name = ASTUtils.setter_name name
  and return_type = None in
  AST.(D_Func { name; args; return_type; body })

%}

(* ------------------------------------------------------------------------

                                   Tokens

  ------------------------------------------------------------------------- *)

%token AND ARRAY ARROW AS ASSERT BAND BEGIN BEQ BIT BITS BNOT BOOLEAN BOR CASE
%token CATCH COLON COLON_COLON COMMA CONCAT CONFIG CONSTANT DIV DO DOT DOWNTO
%token ELSE ELSIF END ENUMERATION EOF EOR EQ EQ_OP EXCEPTION FOR FUNC GEQ
%token GETTER GT IF IMPL IN INTEGER LBRACE LBRACKET LEQ LET LPAR LT MINUS MOD
%token MUL NEQ NOT OF OR OTHERWISE PASS PLUS PLUS_COLON POW PRAGMA RBRACE
%token RBRACKET RDIV REAL RECORD REPEAT RETURN RPAR SEMI_COLON SETTER SHL SHR
%token SLICING STRING SUBTYPES THEN THROW TO TRY TYPE UNKNOWN UNTIL VAR WHEN
%token WHERE WHILE WITH ZTYPE

%token <string> IDENTIFIER STRING_LIT MASK_LIT
%token <string> BITVECTOR_LIT
%token <int> INT_LIT
%token <float> REAL_LIT
%token <bool> BOOL_LIT


(* ------------------------------------------------------------------------

                           Associativity and priority

  ------------------------------------------------------------------------- *)

(*
   This section on associativity uses menhir associativity and priority
   features. Internally, it is used by menhir to resolve some conflicts that
   could arrise from different conflicting expressions, e.g. [3 + 4 + 5].

   For a quick intro, menhir assigns a priority level to tokens that have a
   [left], [right], or [nonassoc] declaration in the order in which they are
   declared. For example, here [PLUS]'s associativity is declared before [MUL]
   so [3 + 4 * 5] will be parsed as [3 + (4 * 5)].

   Associativity is straigh-forward.

   Priority declarations that follow are created because of the fusion of
   multiple recursive bnf rules into one, e.g. [expr] is the fusion of [expr]
   and many others such as [cexpr].
   The rule tree that I am translating here into priority rules is the
   following:

     expr <-----------------------|IF|----------------------< cexpr
     cexpr <----|binop_boolean, checked_type_constraint|----< cexpr_cmp
     cexpr_cmp <-----------|binop_comparison|---------------< cexpr_add_sub
     cexpr_add_sub <------|binop_add_sub_logic|-------------< cexpr_mul_div
     cexpr_mul_div <------|binop_mul_div_shift|-------------< cexpr_pow
     cepxr_pow <---------------|binop_pow|------------------< bexpr
     bexpr <---------------------|unop|---------------------< expr_term
     expr_term <------------------|IN|----------------------< expr_atom
     expr_atom <-----------|DOT, brackets, ...|-------------< expr

*)

(* IF *)
%nonassoc ELSE

(* binop_boolean, checked_type_constraint *)
%left BOR BAND IMPL BEQ AS

(* binop_comparison *)
%left EQ_OP NEQ
%nonassoc GT GEQ LT LEQ

(* binop_add_sub_logic *)
%left PLUS MINUS OR EOR AND

(* binop_mul_div_shift *)
%left MUL DIV RDIV MOD SHL SHR

(* binop_pow *)
%left POW CONCAT

(* unop *)
%nonassoc BNOT NOT

(* IN *)
%nonassoc IN

(* DOT, brackets, etc. *)
%left DOT LBRACKET

(* ------------------------------------------------------------------------- *)

%type <AST.t> ast
%start ast

%%

(* ------------------------------------------------------------------------

                                   Helpers

  ------------------------------------------------------------------------- *)

(* Pair matching *)

let pared(x) == delimited(LPAR, x, RPAR)
let braced(x) == delimited(LBRACE, x, RBRACE)
let bracketed(x) == delimited(LBRACKET, x, RBRACKET)

(* Option handling *)
(* [some] returns an option, but ensures it is there. *)
let some(x) == ~ = x ; <Some>

(* We reverse the standard [terminated] to increase clarity on some complex
   rules. *)
let terminated_by(x, y) == terminated(y, x)

(* ------------------------------------------------------------------------- *)
(* List handling *)

(* A trailing separator list.

   This recognise a possibly-empty, separated, with potentially a trailing
   separator list.
 *)
let trailing_list(sep, x) :=
  | { [] }
  | x=x; { [ x ] }
  | h=x; sep; t=trailing_list(sep, x); { h :: t }

(* A non-empty comma-separated list. *)
let nclist(x) == separated_nonempty_list(COMMA, x)

(* A comma separated list. *)
let clist(x) == { [] } | nclist(x)

(* A comma-separated trailing list. *)
let tclist(x) == trailing_list(COMMA, x)

(* A parenthesised comma-separated list *)
let plist(x) == pared(clist(x))

(* A parenthesised comma-separated list with at least 2 elements. *)
let plist2(x) == pared(
  ~=x; COMMA; ~=separated_nonempty_list(COMMA, x); <List.cons>
)

(* ------------------------------------------------------------------------

                             First parsing rules

  ------------------------------------------------------------------------- *)

let value == (* Also called literal_expr in grammar.bnf *)
  | i=INT_LIT ;        <AST.V_Int>
  | b=BOOL_LIT ;       <AST.V_Bool>
  | r=REAL_LIT ;       <AST.V_Real>
  | b=BITVECTOR_LIT ;  <AST.V_BitVector>
  | STRING_LIT ;       { AST.V_Bool false }
  (* Unsupported now: string_lit and hex_lit *)

let unop ==
  | BNOT ;  { AST.BNOT }
  | MINUS ; { AST.NEG }
  | NOT ;   { AST.NOT }

let unimplemented_binop(x) == x ; { AST.PLUS }

let binop ==
  | AND ;   { AST.AND }
  | BAND ;  { AST.BAND }
  | BOR ;   { AST.BOR }
  | BEQ ;   { AST.EQ_OP }
  | DIV ;   { AST.DIV }
  | EOR ;   { AST.EOR }
  | EQ_OP ; { AST.EQ_OP }
  | NEQ ;   { AST.NEQ }
  | GT ;    { AST.GT }
  | GEQ ;   { AST.GEQ }
  | IMPL ;  { AST.IMPL }
  | LT ;    { AST.LT }
  | LEQ ;   { AST.LEQ }
  | PLUS ;  { AST.PLUS }
  | MINUS ; { AST.MINUS }
  | MOD ;   { AST.MOD }
  | MUL ;   { AST.MUL }
  | OR ;    { AST.OR }
  | RDIV ;  { AST.RDIV }
  | SHL ;   { AST.SHL }
  | SHR ;   { AST.SHR }

  | unimplemented_binop(
    | POW; <>
    | CONCAT; <>
  )

(* ------------------------------------------------------------------------

                                Expressions

  ------------------------------------------------------------------------- *)

let unimplemented_expr(x) == x ; { AST.E_Literal (AST.V_Bool false) }
let field_assign == separated_pair(IDENTIFIER, EQ, expr)

let e_else :=
  | ELSE; expr
  | ELSIF; c=expr; THEN; e=expr; ~=e_else; <AST.E_Cond>

let expr :=
  (* A union of cexpr, cexpr_cmp, cexpr_add_sub, cepxr mul_div, cexpr_pow,
     bexpr, expr_term, expr_atom *)
  | ~=value ;                                  <AST.E_Literal>
  | ~=IDENTIFIER ;                             <AST.E_Var>
  | e1=expr; op=binop; e2=expr;                { AST.E_Binop (op, e1, e2) }
  | op=unop; e=expr;                           <AST.E_Unop>
  | IF; e1=expr; THEN; e2=expr; ~=e_else;      <AST.E_Cond>
  | x=IDENTIFIER; args=plist(expr);            <AST.E_Call>
  | e=expr; ~=slices;                          <AST.E_Slice>
  | e=expr; DOT; x=IDENTIFIER; ~=without_ta;   <AST.E_GetField>

  | t=IDENTIFIER; fields=braced(clist(field_assign));
      { AST.E_Record (AST.T_Named t, fields, AST.TA_None) }

  | pared(expr)
  | terminated(expr, type_assertion)

  | unimplemented_expr(
      | plist2(expr);                             <>
      | expr; IN; pattern_set;                    <>
      | UNKNOWN; COLON_COLON; type_desc;          <>
      | expr; DOT; bracketed(nclist(IDENTIFIER)); <>
      | bracketed(nclist(expr));                  <>
    )

(* ------------------------------------------------------------------------

                                Types

  ------------------------------------------------------------------------- *)

(* Constrained types helpers *)

let int_constraints == braced(nclist(int_constraint_elt))
let int_constraint_elt ==
  | ~=expr;                     <AST.Constraint_Exact>
  | e1=expr; SLICING; e2=expr;  <AST.Constraint_Range>

let bits_constraint ==
| e = expr ;                            <AST.BitWidth_Determined>
| MINUS ; COLON_COLON ; t = type_desc ; <AST.BitWidth_ConstrainedFormType>
| c = int_constraints ;                 <AST.BitWidth_Constrained>

(* Pattern sets -- Not yet implemented *)
let pattern_set == ioption(BNOT); braced(pattern_list)
let pattern_list == nclist(pattern)
let pattern ==
  | expr
  | unimplemented_expr(
    | MINUS;                <>
    | MASK_LIT;             <>
    | expr; SLICING; expr;  <>
    | LEQ; expr;            <>
    | GEQ; expr;            <>
    | pattern_set;          <>
  )

let fields_opt == { [] } | braced(tclist(typed_identifier))

(* Slices *)
let nslices == bracketed(nclist(slice))
let  slices == bracketed( clist(slice))
let slice ==
  | ~=expr;                       <AST.Slice_Single>
  | e1=expr; COLON; e2=expr;      <AST.Slice_Range>
  | e1=expr; PLUS_COLON; e2=expr; <AST.Slice_Length>

(* Bitfields *)
let bitfields == ioption(braced(tclist(bitfield)))
let bitfield == ~=nslices ; ~=IDENTIFIER ; bitfield_spec; <>
(* Bitfield spec -- not yet implemented *)
let bitfield_spec ==
  | as_ty; <>
  | bitfields ; <>

(* Also called ty in grammar.bnf *)
let type_desc :=
  | INTEGER; c = ioption(int_constraints);        <AST.T_Int>
  | REAL;                                         { AST.T_Real }
  | BOOLEAN;                                      { AST.T_Bool }
  | STRING;                                       { AST.T_String }
  | BIT;                                          { AST.T_Bit }
  | BITS; ~=pared(bits_constraint); ~=bitfields;  <AST.T_Bits>
  | ENUMERATION; l=braced(tclist(IDENTIFIER));    <AST.T_Enum>
  | l=plist(type_desc);                           <AST.T_Tuple>
  | ARRAY; e=bracketed(expr); OF; t=type_desc;    <AST.T_Array>
  | RECORD; l=fields_opt;                         <AST.T_Record>
  | EXCEPTION; l=fields_opt;                      <AST.T_Exception>
  | ZTYPE; t=pared(type_desc);                    <AST.T_ZType>
  | name=IDENTIFIER;                              <AST.T_Named>

(* Constructs on type_desc *)

let as_ty == COLON_COLON; type_desc
let typed_identifier == pair(IDENTIFIER, as_ty)
let ty_opt == ioption(as_ty)
let without_ta == { AST.TA_None }
let type_assertion ==
  preceded(AS,
    | type_desc
    | ~=some(int_constraints) ; <AST.T_Int>
  )


(* ------------------------------------------------------------------------

                                Statements

  ------------------------------------------------------------------------- *)

(* Left-hand-side expressions and helpers *)
let le_var == ~=IDENTIFIER ; <AST.LE_Var>
let lexpr_ignore == { AST.LE_Var "-" }
let unimplemented_lexpr(x) == x ; lexpr_ignore

let lexpr ==
  | MINUS; lexpr_ignore
  | lexpr_atom

  | unimplemented_lexpr( pared(nclist(lexpr)) )

let lexpr_atom :=
  | le_var
  | le=lexpr_atom; ~=slices; <AST.LE_Slice>
  | le=lexpr_atom; DOT; field=IDENTIFIER; ~=without_ta; <AST.LE_SetField>

  | unimplemented_lexpr(
    | lexpr; DOT; bracketed(clist(IDENTIFIER)); <>
    | bracketed(nclist(lexpr_atom)); <>
  )

(* Decl items are another kind of left-hand-side expressions, that appear only
   on declarations. They cannot have setter calls or set record fields, they
   have to declare new variables. *)

let decl_item ==
  terminated(
    | IDENTIFIER;               <>
    | pared(nclist(decl_item)); <>
    | MINUS;                    <>
  , ty_opt)
let unimplemented_decl_item ==
  terminated(
    | pared(nclist(decl_item)); <>
    | MINUS;                    <>
  , ty_opt)

(* ------------------------------------------------------------------------- *)
(* Statement helpers *)

let assignment_keyword == LET | CONSTANT | VAR
let storage_keyword    == LET | CONSTANT | VAR | CONFIG
let pass == { AST.S_Pass }
let unimplemented_stmt(x) == x ; pass
let assign(x, y) == ~=x ; EQ ; ~=y ; <AST.S_Assign>

let direction == TO | DOWNTO

let alt == WHEN; ~=pattern_list; ioption(WHERE; expr); COLON; ~=stmt_list; <>
let otherwise == OTHERWISE; COLON; stmt_list
let otherwise_opt == ioption(otherwise)
let catcher ==
  | WHEN; IDENTIFIER; as_ty; COLON; stmt_list
  | WHEN; type_desc;         COLON; stmt_list

let stmt ==
  | terminated_by(END,
    | IF; e=expr; THEN; s1=stmt_list; s2=s_else;    <AST.S_Cond>
    | CASE; e=expr; OF; ~=list(alt); otherwise_opt; <AST.S_Case>

    | unimplemented_stmt(
      | FOR; IDENTIFIER; EQ; expr; direction; expr; DO; stmt_list;    <>
      | WHILE; expr; DO; stmt_list;                                   <>
      | TRY; stmt_list; CATCH; nonempty_list(catcher); otherwise_opt; <>
    )
  )
  | terminated_by(SEMI_COLON,
    | PASS; pass
    | RETURN;         { AST.S_Return [   ] }
    | RETURN; e=expr; { AST.S_Return [ e ] }
    | x=IDENTIFIER; args=plist(expr); <AST.S_Call>

    | assign(lexpr, expr)
    | assignment_keyword; assign(~=le_var; ty_opt; <>, expr)

    | unimplemented_stmt(
      | ASSERT; expr;                                                       <>
      | THROW; ioption(expr);                                               <>
      | REPEAT; stmt_list; UNTIL; expr;                                     <>
      (* We have to manually expend the list otherwise we have a shift/reduce conflict. *)
      | VAR; IDENTIFIER;                            as_ty;                  <>
      | VAR; IDENTIFIER; COMMA; nclist(IDENTIFIER); as_ty;                  <>
      | assignment_keyword; unimplemented_decl_item; EQ; expr;              <>
      | PRAGMA; IDENTIFIER; clist(expr);                                    <>
    )
  )

let stmt_list == ~ = nonempty_list(stmt) ; <ASTUtils.stmt_from_list>

let s_else :=
  | ELSIF; e=expr; THEN; s1=stmt_list; s2=s_else; <AST.S_Cond>
  | ELSE; stmt_list
  | pass

(* ------------------------------------------------------------------------

                                Declarations

  ------------------------------------------------------------------------- *)

let subtype_opt == ioption(SUBTYPES; type_desc)
let unimplemented_decl(x) ==
  x ; { AST.(D_GlobalConst ("-", T_Int None, E_Literal (V_Int 0))) }

let opt_type_identifier == pair(IDENTIFIER, ty_opt)
let return_type == ARROW; type_desc
let params_opt == { [] } | braced(clist(opt_type_identifier))
let access_args_opt == { [] } | bracketed(clist(typed_identifier))
let func_args == plist(typed_identifier)
let func_body == delimited(BEGIN, stmt_list, END)

let decl ==
  | FUNC  ; ~=IDENTIFIER; params_opt; ~=func_args; ~=ioption(return_type);
        ~=func_body; <func>
  | GETTER; ~=IDENTIFIER; params_opt; ~=access_args_opt; ~=return_type;
        ~=func_body; <getter>
  | SETTER; ~=IDENTIFIER; params_opt; ~=access_args_opt; EQ; ~=typed_identifier;
        ~=func_body; <setter>

  | terminated_by(SEMI_COLON,
    | storage_keyword; x=IDENTIFIER; t=as_ty; EQ; e=expr; <AST.D_GlobalConst>
    | TYPE; x=IDENTIFIER; OF; t=type_desc; subtype_opt;   <AST.D_TypeDecl>

    | unimplemented_decl(
      | VAR; typed_identifier;                                            <>
      | storage_keyword; IDENTIFIER; EQ; expr;                            <>
      | PRAGMA; IDENTIFIER; clist(expr);                                  <>
      | TYPE; IDENTIFIER; SUBTYPES; type_desc; ioption(WITH; fields_opt); <>
    )
  )

let ast := terminated(nonempty_list(decl), EOF)

