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

let t_bit = T_Bits (BitWidth_SingleExpr (E_Literal (L_Int Z.one) |> add_dummy_pos), [])

let make_ldi_tuple xs ty =
  LDI_Tuple (List.map (fun x -> LDI_Var (x, None)) xs, Some ty)

%}

(* ------------------------------------------------------------------------

                                   Tokens

  ------------------------------------------------------------------------- *)

%token AND ARRAY ARROW AS ASSERT BAND BEGIN BEQ BIT BITS BNOT BOOLEAN BOR CASE
%token CATCH COLON COLON_COLON COMMA CONCAT CONFIG CONSTANT
%token DEBUG DIV DO DOT DOWNTO
%token ELSE ELSIF END ENUMERATION EOF EOR EQ EQ_OP EXCEPTION FOR FUNC GEQ
%token GETTER GT IF IMPL IN INTEGER LBRACE LBRACKET LEQ LET LPAR LT MINUS MOD
%token MUL NEQ NOT OF OR OTHERWISE PASS PLUS PLUS_COLON POW PRAGMA RBRACE
%token RBRACKET RDIV REAL RECORD REPEAT RETURN RPAR STAR_COLON SEMI_COLON
%token SETTER SHL SHR SLICING STRING SUBTYPES THEN THROW TO TRY TYPE UNKNOWN
%token UNTIL VAR WHEN WHERE WHILE WITH

%token <string> IDENTIFIER STRING_LIT
%token <Bitvector.mask> MASK_LIT
%token <Bitvector.t> BITVECTOR_LIT
%token <Z.t> INT_LIT
%token <Q.t> REAL_LIT
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

%type <unit AST.t> ast
%start ast

(* This start-point is for .opn files in arm-pseudocodes for instructions. *)
%type <unit AST.t> opn
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

(* A comma separated list with at least 2 elements. *)
let clist2(x) == ~=x; COMMA; li=nclist(x); { x :: li }

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
  | i=INT_LIT       ; < L_Int         >
  | b=BOOL_LIT      ; < L_Bool        >
  | r=REAL_LIT      ; < L_Real        >
  | b=BITVECTOR_LIT ; < L_BitVector   >
  | s=STRING_LIT    ; < L_String      >

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
  | POW   ; { POW    }

  | unimplemented_binop(
    | CONCAT; <>
  )

(* ------------------------------------------------------------------------

                                Expressions

  ------------------------------------------------------------------------- *)

let unimplemented_expr(x) == annotated ( x ; { E_Literal (V_Bool false) })
let field_assign == separated_pair(IDENTIFIER, EQ, expr)
let nargs == { [] }

let e_else :=
  | ELSE; expr
  | annotated ( ELSIF; c=expr; THEN; e=expr; ~=e_else; <E_Cond> )

let expr :=
  | make_expr (expr)
  | annotated (
    | ~=plist2(expr);                                             < E_Tuple              >
  )

let make_expr(sub_expr) ==
  annotated (
    (* A union of cexpr, cexpr_cmp, cexpr_add_sub, cepxr mul_div, cexpr_pow,
       bexpr, expr_term, expr_atom *)
    | ~=value ;                                                   < E_Literal            >
    | ~=IDENTIFIER ;                                              < E_Var                >
    | e1=sub_expr; op=binop; e2=expr;                             { E_Binop (op, e1, e2) }
    | op=unop; e=expr;                                            < E_Unop               >
    | IF; e1=expr; THEN; e2=expr; ~=e_else;                       < E_Cond               >
    | x=IDENTIFIER; args=plist(expr); ~=nargs;                    < E_Call               >
    | e=sub_expr; ~=slices;                                       < E_Slice              >
    | e=sub_expr; DOT; x=IDENTIFIER;                              < E_GetField           >
    | e=sub_expr; DOT; fs=bracketed(nclist(IDENTIFIER));          < E_GetFields          >
    | ~=bracketed(nclist(expr));                                  < E_Concat             >
    | ~=sub_expr; AS; ~=ty;                                       < E_Typed              >
    | ~=sub_expr; AS; ~=implicit_t_int;                           < E_Typed              >

    | ~=sub_expr; IN; ~=pattern_set;                              < E_Pattern            >
    | e=sub_expr; IN; m=MASK_LIT;                                 { E_Pattern (e, Pattern_Mask m) }
    | UNKNOWN; COLON_COLON; ~=ty;                                 < E_Unknown            >

    | t=annotated(IDENTIFIER); fields=braced(clist(field_assign));
        { E_Record (add_pos_from t (T_Named t.desc), fields) }
  )

  | pared(sub_expr)

(* ------------------------------------------------------------------------

                                Types

  ------------------------------------------------------------------------- *)

let colon_for_type == COLON | COLON_COLON

(* Constrained types helpers *)

let int_constraints == braced(nclist(int_constraint_elt))
let int_constraint_elt ==
  | ~=expr;                     < Constraint_Exact >
  | e1=expr; SLICING; e2=expr;  < Constraint_Range >

let bits_constraint ==
  | e = expr ;                      < BitWidth_SingleExpr           >
  | MINUS ; colon_for_type ; ~=ty ; < BitWidth_ConstrainedFormType  >
  | c = int_constraints ;           < BitWidth_Constraints          >

let expr_pattern := make_expr (expr_pattern)
let pattern_set ==
  | BNOT; ~=braced(pattern_list); < Pattern_Not >
  | braced(pattern_list)
let pattern_list == ~=nclist(pattern); < Pattern_Any >
let pattern :=
  | ~=expr_pattern; < Pattern_Single >
  | e1=expr_pattern; SLICING; e2=expr; < Pattern_Range >
  | MINUS; { Pattern_All }
  | LEQ; ~=expr; < Pattern_Leq >
  | GEQ; ~=expr; < Pattern_Geq >
  | ~=MASK_LIT; < Pattern_Mask >
  | ~=plist2(pattern); < Pattern_Tuple >
  | pattern_set

let fields_opt == { [] } | braced(tclist(typed_identifier))

(* Slices *)
let nslices == bracketed(nclist(slice))
let  slices == bracketed( clist(slice))
let slice ==
  | ~=expr;                       < Slice_Single  >
  | e1=expr; COLON; e2=expr;      < Slice_Range   >
  | e1=expr; PLUS_COLON; e2=expr; < Slice_Length  >
  | e1=expr; STAR_COLON; e2=expr; < Slice_Star    >

(* Bitfields *)
let bitfields == loption(braced(tclist(bitfield)))
let bitfield == s=nslices ; x=IDENTIFIER ; bitfield_spec; { (x, s) }
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
    | BIT;                                          { t_bit       }
    | BITS; ~=pared(bits_constraint); ~=bitfields;  < T_Bits      >
    | ENUMERATION; l=braced(tclist(IDENTIFIER));    < T_Enum      >
    | l=plist(ty);                                  < T_Tuple     >
    | ARRAY; e=bracketed(expr); OF; t=ty;           < T_Array     >
    | RECORD; l=fields_opt;                         < T_Record    >
    | EXCEPTION; l=fields_opt;                      < T_Exception >
    | name=IDENTIFIER;                              < T_Named     >
  )

(* Constructs on ty *)
let as_ty == colon_for_type; ty
let typed_identifier == pair(IDENTIFIER, as_ty)
let ty_opt == ioption(as_ty)
let implicit_t_int == annotated ( ~=some(int_constraints) ; <T_Int> )


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
  | le=annotated(lexpr_atom); DOT; field=IDENTIFIER; <LE_SetField>
  | le=annotated(lexpr_atom); DOT; li=bracketed(clist(IDENTIFIER)); <LE_SetFields>

  | unimplemented_lexpr(
    | bracketed(nclist(lexpr_atom)); <>
  )

(* Decl items are another kind of left-hand-side expressions, that appear only
   on declarations. They cannot have setter calls or set record fields, they
   have to declare new variables. *)

let decl_item ==
  | ~=IDENTIFIER               ; ~=ty_opt ; < LDI_Var    >
  | MINUS                      ; ~=ty_opt ; < LDI_Ignore >
  | ~=pared(nclist(decl_item)) ; ~=ty_opt ; < LDI_Tuple  >

(* ------------------------------------------------------------------------- *)
(* Statement helpers *)

let local_decl_keyword ==
  | LET       ; { LDK_Let       }
  | CONSTANT  ; { LDK_Constant  }

let storage_keyword ==
  | LET       ; { GDK_Let      }
  | CONSTANT  ; { GDK_Constant }
  | VAR       ; { GDK_Var      }
  | CONFIG    ; { GDK_Config   }

let pass == { S_Pass }
let unimplemented_stmt(x) == x ; pass

let assign(x, y) == ~=x ; EQ ; ~=y ; { S_Assign (x,y,V1) }

let direction == | TO; { AST.Up } | DOWNTO; { AST.Down }

let alt_delim == ARROW | COLON
let alt == annotated (
  | WHEN; ~=pattern_list; ioption(WHERE; expr); alt_delim; ~=stmt_list; <>
  | OTHERWISE; alt_delim; s=stmt_list; { (Pattern_All, s) }
)

let otherwise == OTHERWISE; ARROW; stmt_list
let otherwise_opt == ioption(otherwise)
let catcher == WHEN; ~=ioption(terminated(IDENTIFIER, COLON)); ~=ty; ARROW; ~=stmt_list; <>

let stmt ==
  annotated (
    | terminated_by(END,
      | IF; e=expr; THEN; s1=stmt_list; s2=s_else;    <S_Cond>
      | CASE; ~=expr; OF; alt=list(alt);              <S_Case>
      | WHILE; ~=expr; DO; ~=stmt_list;               <S_While>
      | FOR; id=IDENTIFIER; EQ; e1=expr;
        d=direction; e2=expr; DO; s=stmt_list;        <S_For>
      | TRY; s=stmt_list; CATCH; c=nonempty_list(catcher); o=otherwise_opt; < S_Try >
    )
    | terminated_by(SEMI_COLON,
      | PASS; pass
      | RETURN; ~=ioption(expr);                             < S_Return >
      | x=IDENTIFIER; args=plist(expr); ~=nargs;             < S_Call   >
      | ASSERT; e=expr;                                      < S_Assert >
      | DEBUG; e=expr;                                       < S_Debug >
      | le=lexpr; EQ; e=expr;
          {  S_Assign (le,e,V1) }
      | ~=local_decl_keyword; ~=decl_item; EQ; ~=some(expr); < S_Decl   >
      | VAR; ldi=decl_item; e=ioption(EQ; expr);             { S_Decl (LDK_Var, ldi, e) }
      | REPEAT; ~=stmt_list; UNTIL; ~=expr;                  < S_Repeat >
      | THROW; e=expr;                                       { S_Throw (Some (e, None)) }
      | THROW;                                               { S_Throw None             }

      | VAR; xs=clist2(IDENTIFIER); colon_for_type; ~=ty;
          { S_Decl (LDK_Var, make_ldi_tuple xs ty, None) }

      | unimplemented_stmt(
        | PRAGMA; IDENTIFIER; clist(expr);                   <>
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

let subtype_opt == option(SUBTYPES; IDENTIFIER)
let unimplemented_decl(x) ==
  x ; {
    let e = literal (L_Int Z.zero) and ty = add_dummy_pos (T_Int None) in
    (D_GlobalStorage { name="-"; keyword=GDK_Constant; ty=Some ty; initial_value = Some e})
  }

let opt_type_identifier == pair(IDENTIFIER, ty_opt)
let return_type == ARROW; ty
let params_opt == { [] } | braced(clist(opt_type_identifier))
let access_args_opt == { [] } | bracketed(clist(typed_identifier))
let func_args == plist(typed_identifier)
let func_body == delimited(ioption(BEGIN), stmt_list, END)

let decl ==
  annotated (
    | FUNC; name=IDENTIFIER; ~=params_opt; ~=func_args; ~=return_type; body=func_body;
        {
          D_Func {
            name;
            parameters = params_opt;
            args = func_args;
            body = SB_ASL body;
            return_type = Some return_type;
            subprogram_type = ST_Function;
          }
        }
    | FUNC; name=IDENTIFIER; ~=params_opt; ~=func_args; body=func_body;
        {
          D_Func {
            name;
            parameters = params_opt;
            args = func_args;
            body = SB_ASL body;
            return_type = None;
            subprogram_type = ST_Procedure;
          }
        }
    | GETTER; name=IDENTIFIER; ~=params_opt; ~=access_args_opt; ret=return_type;
        ~=func_body;
        {
          D_Func
            {
              name;
              parameters = params_opt;
              args = access_args_opt;
              return_type = Some ret;
              body = SB_ASL func_body;
              subprogram_type = ST_Getter;
            }
        }
    | SETTER; name=IDENTIFIER; ~=params_opt; ~=access_args_opt; EQ; v=typed_identifier;
        ~=func_body;
        {
          D_Func
            {
              name;
              parameters = params_opt;
              args = v :: access_args_opt;
              return_type = None;
              body = SB_ASL func_body;
              subprogram_type = ST_Setter;
            }
        }

    | terminated_by(SEMI_COLON,
      | TYPE; x=IDENTIFIER; OF; t=ty; ~=subtype_opt;       < D_TypeDecl >
      | TYPE; x=IDENTIFIER; SUBTYPES; s=IDENTIFIER;
        { D_TypeDecl (x, ASTUtils.add_dummy_pos (T_Named s), Some s) }

      | keyword=storage_keyword; name=IDENTIFIER;
        ty=ioption(as_ty); EQ; initial_value=some(expr);
        { D_GlobalStorage { keyword; name; ty; initial_value } }
      | VAR; name=IDENTIFIER; ty=some(as_ty);
        { D_GlobalStorage { keyword=GDK_Var; name; ty; initial_value=None}}

      | unimplemented_decl(
        | storage_keyword; MINUS; ty_opt; EQ; expr;                <>
        | PRAGMA; IDENTIFIER; clist(expr);                         <>
        | TYPE; IDENTIFIER; SUBTYPES; ty; WITH; fields_opt;        <>
      )
    )
  )

let ast := terminated(list(decl), EOF)

let opn := body=stmt;
    {
      [
        D_Func
          {
            name = "main";
            args = [];
            parameters = [];
            body = SB_ASL body;
            return_type = None;
            subprogram_type = ST_Procedure;
          }
        |> ASTUtils.add_pos_from body
      ]
    }
