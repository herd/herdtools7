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

open AST
open ASTUtils

let func (name, args, return_type, body) =
  D_Func { name; args; return_type; body }

let getter (name, args, return_type, body) =
  let name = getter_name name
  and return_type = Some(return_type) in
  D_Func { name; args; return_type; body }

let setter (name, args, new_val, body) =
  let args = new_val :: args
  and name = setter_name name
  and return_type = None in
  D_Func { name; args; return_type; body }

let opn_start stmt = [ func ("main", [], None, stmt) ]

let build_dot_fields (e, fields) =
    let one_field f = E_GetField (e, f, TA_None) in
    let one_field s = add_pos_from e @@ one_field s in
    E_Concat (List.map one_field fields)

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
%token <Bitvector.t> BITVECTOR_LIT
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
     cexpr <----|binop_boolean, checked_type_constraint|---<  cexpr_cmp
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

(* This start-point is for .opn files in arm-pseudocodes for instructions. *)
%type <AST.t> opn
%start opn

%%

(* ------------------------------------------------------------------------

                                   Helpers

  ------------------------------------------------------------------------- *)

(* Pair matching *)

let     pared(x) == delimited(    LPAR, x, RPAR    )
let    braced(x) == delimited(  LBRACE, x, RBRACE  )
let bracketed(x) == delimited(LBRACKET, x, RBRACKET)

(* Option handling *)
(* [some] returns an option, but ensures it is there. *)
let some(x) == ~ = x ; <Some>

(* We reverse the standard [terminated] to increase clarity on some complex
   rules. *)
let terminated_by(x, y) == terminated(y, x)

(* Position annotation *)
let annotated(x) == desc = x; { { desc; pos_start=$symbolstartpos; pos_end=$endpos } }

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
  ~=x; COMMA; li=separated_nonempty_list(COMMA, x); { x :: li }
)

(* ------------------------------------------------------------------------

                             First parsing rules

  ------------------------------------------------------------------------- *)

let value == (* Also called literal_expr in grammar.bnf *)
  | i=INT_LIT       ; < V_Int         >
  | b=BOOL_LIT      ; < V_Bool        >
  | r=REAL_LIT      ; < V_Real        >
  | b=BITVECTOR_LIT ; < V_BitVector   >
  | STRING_LIT      ; { V_Bool false  }
  (* Unsupported now: string_lit and hex_lit *)

let unop ==
  | BNOT  ; { BNOT }
  | MINUS ; { NEG }
  | NOT   ; { NOT }

let unimplemented_binop(x) == x ; { PLUS }

let binop ==
  | AND   ; { AND    }
  | BAND  ; { BAND   }
  | BOR   ; { BOR    }
  | BEQ   ; { EQ_OP  }
  | DIV   ; { DIV    }
  | EOR   ; { EOR    }
  | EQ_OP ; { EQ_OP  }
  | NEQ   ; { NEQ    }
  | GT    ; { GT     }
  | GEQ   ; { GEQ    }
  | IMPL  ; { IMPL   }
  | LT    ; { LT     }
  | LEQ   ; { LEQ    }
  | PLUS  ; { PLUS   }
  | MINUS ; { MINUS  }
  | MOD   ; { MOD    }
  | MUL   ; { MUL    }
  | OR    ; { OR     }
  | RDIV  ; { RDIV   }
  | SHL   ; { SHL    }
  | SHR   ; { SHR    }

  | unimplemented_binop(
    | POW   ; <>
    | CONCAT; <>
  )

(* ------------------------------------------------------------------------

                                Expressions

  ------------------------------------------------------------------------- *)

let unimplemented_expr(x) == annotated ( x ; { E_Literal (V_Bool false) })
let field_assign == separated_pair(IDENTIFIER, EQ, expr)

let e_else :=
  | ELSE; expr
  | annotated ( ELSIF; c=expr; THEN; e=expr; ~=e_else; <E_Cond> )

let expr :=
  annotated (
    (* A union of cexpr, cexpr_cmp, cexpr_add_sub, cepxr mul_div, cexpr_pow,
       bexpr, expr_term, expr_atom *)
    | ~=value ;                                     < E_Literal            >
    | ~=IDENTIFIER ;                                < E_Var                >
    | e1=expr; op=binop; e2=expr;                   { E_Binop (op, e1, e2) }
    | op=unop; e=expr;                              < E_Unop               >
    | IF; e1=expr; THEN; e2=expr; ~=e_else;         < E_Cond               >
    | x=IDENTIFIER; args=plist(expr);               < E_Call               >
    | e=expr; ~=slices;                             < E_Slice              >
    | e=expr; DOT; x=IDENTIFIER; ~=without_ta;      < E_GetField           >
    | ~=bracketed(nclist(expr));                    < E_Concat             >
    | ~=plist2(expr);                               < E_Tuple              >
    | ~=expr; DOT; ~=bracketed(nclist(IDENTIFIER)); < build_dot_fields     >

    | t=annotated(IDENTIFIER); fields=braced(clist(field_assign));
        { E_Record (add_pos_from t (T_Named t.desc), fields, TA_None) }
  )

  | pared(expr)
  | terminated(expr, type_assertion)

  | unimplemented_expr (
      | expr; IN; pattern_set;             <>
      | UNKNOWN; COLON_COLON; ty;          <>
  )

(* ------------------------------------------------------------------------

                                Types

  ------------------------------------------------------------------------- *)

(* Constrained types helpers *)

let int_constraints == braced(nclist(int_constraint_elt))
let int_constraint_elt ==
  | ~=expr;                     < Constraint_Exact >
  | e1=expr; SLICING; e2=expr;  < Constraint_Range >

let bits_constraint ==
  | e = expr ;                      < BitWidth_Determined           >
  | MINUS ; COLON_COLON ; t = ty ;  < BitWidth_ConstrainedFormType  >
  | c = int_constraints ;           < BitWidth_Constrained          >

(* Pattern sets -- Not yet implemented *)
let pattern_set == ioption(BNOT); braced(pattern_list)
let pattern_list == nclist(pattern)
let pattern ==
  | expr
  | unimplemented_expr (
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
  | ~=expr;                       < Slice_Single  >
  | e1=expr; COLON; e2=expr;      < Slice_Range   >
  | e1=expr; PLUS_COLON; e2=expr; < Slice_Length  >

(* Bitfields *)
let bitfields == ioption(braced(tclist(bitfield)))
let bitfield == ~=nslices ; ~=IDENTIFIER ; bitfield_spec; <>
(* Bitfield spec -- not yet implemented *)
let bitfield_spec==
  | as_ty     ; <>
  | bitfields ; <>

(* Also called ty in grammar.bnf *)
let ty :=
  annotated (
    | INTEGER; c = ioption(int_constraints);        < T_Int       >
    | REAL;                                         { T_Real      }
    | BOOLEAN;                                      { T_Bool      }
    | STRING;                                       { T_String    }
    | BIT;                                          { T_Bit       }
    | BITS; ~=pared(bits_constraint); ~=bitfields;  < T_Bits      >
    | ENUMERATION; l=braced(tclist(IDENTIFIER));    < T_Enum      >
    | l=plist(ty);                                  < T_Tuple     >
    | ARRAY; e=bracketed(expr); OF; t=ty;           < T_Array     >
    | RECORD; l=fields_opt;                         < T_Record    >
    | EXCEPTION; l=fields_opt;                      < T_Exception >
    | ZTYPE; t=pared(ty);                           < T_ZType     >
    | name=IDENTIFIER;                              < T_Named     >
  )

(* Constructs on ty *)
let as_ty == COLON_COLON; ty
let typed_identifier == pair(IDENTIFIER, as_ty)
let ty_opt == ioption(as_ty)
let without_ta == { TA_None }
let type_assertion ==
  preceded(AS,
    | ty
    | annotated ( ~=some(int_constraints) ; <T_Int> )
  )


(* ------------------------------------------------------------------------

                                Statements

  ------------------------------------------------------------------------- *)

(* Left-hand-side expressions and helpers *)
let le_var == ~=IDENTIFIER ; <LE_Var>
let lexpr_ignore == { LE_Ignore }
let unimplemented_lexpr(x) == x ; lexpr_ignore

let lexpr ==
  annotated(
    | MINUS; lexpr_ignore
    | lexpr_atom
    | ~=pared(nclist(lexpr)); <LE_TupleUnpack>
  )

let lexpr_atom :=
  | le_var
  | le=annotated(lexpr_atom); ~=slices; <LE_Slice>
  | le=annotated(lexpr_atom); DOT; field=IDENTIFIER; ~=without_ta; <LE_SetField>

  | unimplemented_lexpr(
    | lexpr; DOT; bracketed(clist(IDENTIFIER)); <>
    | bracketed(nclist(lexpr_atom)); <>
  )

(* Decl items are another kind of left-hand-side expressions, that appear only
   on declarations. They cannot have setter calls or set record fields, they
   have to declare new variables. *)

let decl_item ==
  annotated ( terminated (
    | le_var
    | MINUS; lexpr_ignore
    | ~=pared(nclist(decl_item)); <LE_TupleUnpack>
  , ty_opt))

(* ------------------------------------------------------------------------- *)
(* Statement helpers *)

let assignment_keyword == LET | CONSTANT | VAR
let storage_keyword    == LET | CONSTANT | VAR | CONFIG

let pass == { S_Pass }
let unimplemented_stmt(x) == x ; pass

let assign(x, y) == ~=x ; EQ ; ~=y ; <S_Assign>

let direction == TO | DOWNTO

let alt == annotated (
  WHEN; ~=pattern_list; ioption(WHERE; expr); COLON; ~=stmt_list; <>
)
let otherwise == annotated(OTHERWISE; COLON; stmt_list)
let otherwise_opt == ioption(otherwise)
let catcher ==
  | WHEN; IDENTIFIER; as_ty; COLON; stmt_list
  | WHEN; ty;                COLON; stmt_list

let stmt ==
  annotated (
    | terminated_by(END,
      | IF; e=expr; THEN; s1=stmt_list; s2=s_else;    <S_Cond>
      | CASE; ~=expr; OF; alt=list(alt);              <S_Case>
      | CASE; e=expr; OF; alt=list(alt); ~=otherwise;
          { S_Case (e, alt @ [ map_desc (fun o -> ([], o.desc)) otherwise ]) }

      | unimplemented_stmt(
        | FOR; IDENTIFIER; EQ; expr; direction; expr; DO; stmt_list;    <>
        | WHILE; expr; DO; stmt_list;                                   <>
        | TRY; stmt_list; CATCH; nonempty_list(catcher); otherwise_opt; <>
      )
    )
    | terminated_by(SEMI_COLON,
      | PASS; pass
      | RETURN; ~=ioption(expr);              < S_Return >
      | x=IDENTIFIER; args=plist(expr);       < S_Call   >
      | ASSERT; e=expr;                       < S_Assert >

      | assign(lexpr, expr)
      | assignment_keyword; assign(decl_item, expr)

      | unimplemented_stmt(
        | THROW; ioption(expr);                                               <>
        | REPEAT; stmt_list; UNTIL; expr;                                     <>
        (* We have to manually expend the list otherwise we have a shift/reduce conflict. *)
        | VAR; IDENTIFIER;                            as_ty;                  <>
        | VAR; IDENTIFIER; COMMA; nclist(IDENTIFIER); as_ty;                  <>
        | PRAGMA; IDENTIFIER; clist(expr);                                    <>
      )
    )
  )

let stmt_list == ~ = nonempty_list(stmt) ; <stmt_from_list>

let s_else :=
  annotated (
    | ELSIF; e=expr; THEN; s1=stmt_list; s2=s_else; <S_Cond>
    | pass
  )
  | ELSE; stmt_list

(* ------------------------------------------------------------------------

                                Declarations

  ------------------------------------------------------------------------- *)

let subtype_opt == ioption(SUBTYPES; ty)
let unimplemented_decl(x) ==
  x ; {
    let e = literal (V_Int 0) and ty = add_dummy_pos (T_Int None) in
    (D_GlobalConst ("-", ty, e))
  }

let opt_type_identifier == pair(IDENTIFIER, ty_opt)
let return_type == ARROW; ty
let params_opt == { [] } | braced(clist(opt_type_identifier))
let access_args_opt == { [] } | bracketed(clist(typed_identifier))
let func_args == plist(typed_identifier)
let func_body == delimited(ioption(BEGIN), stmt_list, END)

let decl ==
  | FUNC  ; ~=IDENTIFIER; params_opt; ~=func_args; ~=ioption(return_type);
        ~=func_body; <func>
  | GETTER; ~=IDENTIFIER; params_opt; ~=access_args_opt; ~=return_type;
        ~=func_body; <getter>
  | SETTER; ~=IDENTIFIER; params_opt; ~=access_args_opt; EQ; ~=typed_identifier;
        ~=func_body; <setter>

  | terminated_by(SEMI_COLON,
    | storage_keyword; x=IDENTIFIER; t=as_ty; EQ; e=expr; <D_GlobalConst>
    | TYPE; x=IDENTIFIER; OF; t=ty; subtype_opt;   <D_TypeDecl>

    | unimplemented_decl(
      | storage_keyword; MINUS; ty_opt; EQ; expr;                         <>
      | VAR; typed_identifier;                                            <>
      | storage_keyword; IDENTIFIER; EQ; expr;                            <>
      | PRAGMA; IDENTIFIER; clist(expr);                                  <>
      | TYPE; IDENTIFIER; SUBTYPES; ty; ioption(WITH; fields_opt);        <>
    )
  )

let ast := terminated(nonempty_list(decl), EOF)

let opn := ~=stmt; <opn_start>
