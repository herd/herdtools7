(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(*
  Goals:
    - Every valid ASLv1 specification is accepted by this parser.
    - No warnings should be emitted by menhir.
    - Being somewhat readable

  Notations:
    - [unimplemented_XXX] discards the production by the rule and returns a
      dummy value.

  Notes:
    - In some cases, rules ending with <> are not implemented in the AST yet.
 *)

(* ------------------------------------------------------------------------

                               Parser Config

  ------------------------------------------------------------------------- *)
%parameter<Config : ParserConfig.CONFIG>

(* ------------------------------------------------------------------------

                                   Helpers

  ------------------------------------------------------------------------- *)

%{

open AST
open ASTUtils
open Desugar

let version = V1

let t_bit =
  T_Bits (E_Literal (L_Int Z.one) |> add_dummy_annotation ~version, [])

let zero = E_Literal (L_Int Z.zero) |> add_dummy_annotation ~version

let make_ldi_vars (xs, ty) =
  let make_one x =
    S_Decl (LDK_Var, LDI_Typed (LDI_Var x, ty), None)
    |> add_dummy_annotation ~version
  in
  List.map make_one xs |> stmt_from_list |> desc

let make_ty_decl_subtype (x, s) =
  let name, _fields = s.desc in
  let ty = ASTUtils.add_pos_from s (T_Named name) in
  D_TypeDecl (x, ty, Some s.desc)

let prec =
  let open AST in
  function
  | BOR | BAND | IMPL | BEQ -> 1
  | EQ_OP | NEQ -> 2
  | PLUS | MINUS | OR | EOR | AND -> 3
  | MUL | DIV | DIVRM | RDIV | MOD | SHL | SHR -> 4
  | POW -> 5
  | _ -> 0 (* Non assoc *)

let check_not_same_prec loc op op' =
  if prec op = prec op' then Error.(fatal_from loc CannotParse)

let check_not_binop_same_prec op e =
  match e.desc with
  | E_Binop (op', _, _) when op != op' -> check_not_same_prec e op op'
  | _ -> ()

let e_binop (e1, op, e2) =
  let () = check_not_binop_same_prec op e1
  and () = check_not_binop_same_prec op e2
  in
  E_Binop (op, e1, e2)

let e_call call = E_Call { call with call_type = ST_Function }
let s_call call = S_Call { call with call_type = ST_Procedure }

%}

(* ------------------------------------------------------------------------- *)

%type <AST.t> spec
%start spec

(* This start-point is for .opn files in arm-pseudocodes for instructions. *)
%type <AST.t> opn
%start opn

(* Parse statements, as one *)
%type <AST.stmt> stmts
%start stmts

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
let annotated(x) == desc = x; { { desc; pos_start=$symbolstartpos; pos_end=$endpos; version } }

(* ------------------------------------------------------------------------- *)
(* List handling *)

(* A trailing separator list.

   This recognises a possibly-empty, separated, with potentially a trailing
   separator list.
 *)
let trailing_list(sep, x) == loption(non_empty_trailing_list(sep, x))

(* Same but in non-empty. *)
let non_empty_trailing_list(sep, x) :=
  | x=x; ioption(sep); { [ x ] }
  | h=x; sep; t=non_empty_trailing_list(sep, x); { h :: t }

(* A non-empty comma-separated list. *)
let nclist(x) == separated_nonempty_list(COMMA, x)

(* A comma separated list. *)
let clist(x) == { [] } | nclist(x)

(* A comma separated list with at least 2 elements. *)
let clist2(x) == ~=x; COMMA; li=nclist(x); { x :: li }

(* A comma-separated trailing list. *)
let tclist(x) == trailing_list(COMMA, x)

(* A comma-separated non-empty trailing list. *)
let ntclist(x) == non_empty_trailing_list(COMMA, x)

(* A parenthesised comma-separated list *)
let plist(x) == pared(clist(x))

(* A parenthesised comma-separated list with at least 2 elements. *)
let plist2(x) == pared(
  ~=x; COMMA; li=separated_nonempty_list(COMMA, x); { x :: li }
)

(* Produces a list of [x]s, optionally followed by a single [y] that would go
   in the tail of the list *)
let nlist_opt_terminated(x, y) :=
  | ~=x; { [ x ] }
  | ~=x; ~=y; { [ x; y ] }
  | ~=x; l=nlist_opt_terminated(x, y); { x :: l }

let end_semicolon := END; opt=option(SEMI_COLON); {
      if Option.is_none opt && not Config.allow_no_end_semicolon then
          Error.fatal_here $startpos $endpos @@ Error.ObsoleteSyntax "Missing ';' after 'end' keyword.";
  }


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
  | AND         ; { AND    }
  | BAND        ; { BAND   }
  | BOR         ; { BOR    }
  | BEQ         ; { EQ_OP  }
  | DIV         ; { DIV    }
  | DIVRM       ; { DIVRM  }
  | EOR         ; { EOR    }
  | EQ_OP       ; { EQ_OP  }
  | NEQ         ; { NEQ    }
  | GT          ; { GT     }
  | GEQ         ; { GEQ    }
  | IMPL        ; { IMPL   }
  | LT          ; { LT     }
  | LEQ         ; { LEQ    }
  | PLUS        ; { PLUS   }
  | MINUS       ; { MINUS  }
  | MOD         ; { MOD    }
  | MUL         ; { MUL    }
  | OR          ; { OR     }
  | RDIV        ; { RDIV   }
  | SHL         ; { SHL    }
  | SHR         ; { SHR    }
  | POW         ; { POW    }
  | COLON_COLON ; { BV_CONCAT }
  | unimplemented_binop(
    | CONCAT; <>
  )

(* ------------------------------------------------------------------------

                                Expressions

  ------------------------------------------------------------------------- *)

let field_assign == separated_pair(IDENTIFIER, EQ, expr)

let e_else :=
  | ELSE; expr
  | annotated ( ELSIF; c=expr; THEN; e=expr; ~=e_else; <E_Cond> )

let expr :=
  annotated (
    (* A union of cexpr, cexpr_cmp, cexpr_add_sub, cepxr mul_div, cexpr_pow,
       bexpr, expr_term, expr_atom *)
    | ~=value ;                                                   < E_Literal            >
    | ~=IDENTIFIER ;                                              < E_Var                >
    | e1=expr; op=binop; e2=expr;                             < e_binop              >
    | op=unop; e=expr;                                            < E_Unop               > %prec UNOPS
    | IF; e1=expr; THEN; e2=expr; ~=e_else;                       < E_Cond               >
    | ~=call;                                                     < e_call               >
    | e=expr; ~=slices;                                       < E_Slice              >
    | e1=expr; LLBRACKET; e2=expr; RRBRACKET;                 < E_GetArray           >
    | e=expr; DOT; x=IDENTIFIER;                              < E_GetField           >
    | e=expr; DOT; fs=bracketed(nclist(IDENTIFIER));          < E_GetFields          >

    | ~=expr; AS; ~=ty;                                       < E_ATC                >
    | ~=expr; AS; ~=implicit_t_int;                           < E_ATC                >

    | ~=expr; IN; ~=pattern_set;                              < E_Pattern            >
    | ~=expr; EQ_OP; ~=pattern_mask;                          < E_Pattern            >
    | e=expr; NEQ; p=pattern_mask;                            { E_Pattern (e, Pattern_Not (p) |> add_pos_from p) }
    | ARBITRARY; colon_for_type; ~=ty;                        < E_Arbitrary        >
    | e=pared(expr);                                          { E_Tuple [ e ]        }

    | t=annotated(IDENTIFIER); fields=braced(clist(field_assign));
        { E_Record (add_pos_from t (T_Named t.desc), fields) }
    (* Excluded from expr_pattern *)
    | ~=plist2(expr);                                             < E_Tuple              >
  )

(* ------------------------------------------------------------------------

                                Types

  ------------------------------------------------------------------------- *)

let colon_for_type == COLON | COLON_COLON

(* Constrained types helpers *)

let constraint_kind_opt == constraint_kind | { UnConstrained }
let constraint_kind ==
  | ~=braced(nclist(int_constraint)); < WellConstrained >
  | braced(MINUS); { PendingConstrained }

let int_constraint ==
  | ~=expr;                     < Constraint_Exact >
  | e1=expr; SLICING; e2=expr;  < Constraint_Range >


let expr_pattern :=
  annotated(
    (* A union of cexpr, cexpr_cmp, cexpr_add_sub, cepxr mul_div, cexpr_pow,
       bexpr, expr_term, expr_atom *)
    | ~=value ;                                                   < E_Literal            >
    | ~=IDENTIFIER ;                                              < E_Var                >
    | e1=expr_pattern; op=binop; e2=expr;                             < e_binop              >
    | op=unop; e=expr;                                            < E_Unop               > %prec UNOPS
    | IF; e1=expr; THEN; e2=expr; ~=e_else;                       < E_Cond               >
    | ~=call;                                                     < e_call               >
    | e=expr_pattern; ~=slices;                                       < E_Slice              >
    | e1=expr_pattern; LLBRACKET; e2=expr; RRBRACKET;                 < E_GetArray           >
    | e=expr_pattern; DOT; x=IDENTIFIER;                              < E_GetField           >
    | e=expr_pattern; DOT; fs=bracketed(nclist(IDENTIFIER));          < E_GetFields          >

    | ~=expr_pattern; AS; ~=ty;                                       < E_ATC                >
    | ~=expr_pattern; AS; ~=implicit_t_int;                           < E_ATC                >

    | ~=expr_pattern; IN; ~=pattern_set;                              < E_Pattern            >
    | ~=expr_pattern; EQ_OP; ~=pattern_mask;                          < E_Pattern            >
    | e=expr_pattern; NEQ; p=pattern_mask;                            { E_Pattern (e, Pattern_Not (p) |> add_pos_from p) }

    | ARBITRARY; colon_for_type; ~=ty;                                < E_Arbitrary        >
    | e=pared(expr_pattern);                                          { E_Tuple [ e ]        }

    | t=annotated(IDENTIFIER); fields=braced(clist(field_assign));
        { E_Record (add_pos_from t (T_Named t.desc), fields) }
  )

let pattern_mask == annotated(~=MASK_LIT; < Pattern_Mask >)
let pattern_list == annotated(~=nclist(pattern); < Pattern_Any >)

let pattern :=
  annotated (
    | ~=expr_pattern; < Pattern_Single >
    | e1=expr_pattern; SLICING; e2=expr; < Pattern_Range >
    | MINUS; { Pattern_All }
    | LEQ; ~=expr; < Pattern_Leq >
    | GEQ; ~=expr; < Pattern_Geq >
    | ~=plist2(pattern); < Pattern_Tuple >
  )
  | pattern_mask
  | pattern_set

let pattern_set ==
  | braced(pattern_list)
  | annotated (
      BNOT; ~=braced(pattern_list); < Pattern_Not >
    )

let fields == braced(tclist(typed_identifier))
let fields_opt == { [] } | fields

(* Slices *)
let slices == bracketed(nclist(slice))
let slice ==
  | ~=expr;                       < Slice_Single  >
  | e1=expr; COLON; e2=expr;      < Slice_Range   >
  | e1=expr; PLUS_COLON; e2=expr; < Slice_Length  >
  | COLON; e=expr;                { Slice_Length(zero, e) }
  | e1=expr; STAR_COLON; e2=expr; < Slice_Star    >

(* Bitfields *)
let bitfields_opt == loption(bitfields)
let bitfields == braced(tclist(bitfield))
let bitfield ==
  | s=slices ; x=IDENTIFIER ;                 { BitField_Simple (x, s)     }
  | s=slices ; x=IDENTIFIER ; bf=bitfields ;  { BitField_Nested (x, s, bf) }
  | s=slices ; x=IDENTIFIER ; ty=as_ty     ;  { BitField_Type   (x, s, ty) }

(* Also called ty in grammar.bnf *)
let ty :=
  annotated (
    | INTEGER; c = constraint_kind_opt;                 < T_Int       >
    | REAL;                                             { T_Real      }
    | BOOLEAN;                                          { T_Bool      }
    | STRING;                                           { T_String    }
    | BIT;                                              { t_bit       }
    | BITS; ~=pared(expr); ~=bitfields_opt;             < T_Bits      >
    | l=plist(ty);                                      < T_Tuple     >
    | name=IDENTIFIER;                                  < T_Named     >
    | ARRAY; e=bracketed(expr); OF; t=ty;               { T_Array (ArrayLength_Expr e, t) }
  )

let ty_decl := ty |
  annotated (
    | ENUMERATION; l=braced(ntclist(IDENTIFIER));       < T_Enum      >
    | RECORD; l=fields_opt;                             < T_Record    >
    | EXCEPTION; l=fields_opt;                          < T_Exception >
  )

(* Constructs on ty *)
(* Begin AsTy *)
let as_ty == colon_for_type; ty
(* End *)

(* Begin TypedIdentifier *)
let typed_identifier == pair(IDENTIFIER, as_ty)
(* End *)

let ty_opt == ioption(as_ty)
let implicit_t_int == annotated ( ~=constraint_kind ; <T_Int> )


(* ------------------------------------------------------------------------

                                Statements

  ------------------------------------------------------------------------- *)

(* Left-hand-side expressions and helpers *)
let lexpr :=
  | lexpr_atom
  | annotated (MINUS; { LE_Discard })
  | annotated (
      ~=pared(clist2(lexpr)); <LE_Destructuring>
    )

let lexpr_atom_desc :=
  | ~=IDENTIFIER ; <LE_Var>
  | le=lexpr_atom; ~=slices; <LE_Slice>
  | le=lexpr_atom; LLBRACKET; ~=expr; RRBRACKET; <LE_SetArray>
  | le=lexpr_atom; DOT; field=IDENTIFIER; <LE_SetField>
  | le=lexpr_atom; DOT; li=bracketed(clist(IDENTIFIER)); { LE_SetFields (le, li, []) }

let lexpr_atom == annotated(lexpr_atom_desc)

(* Decl items are another kind of left-hand-side expressions, which appear only
   on declarations. They cannot have setter calls or set record fields, they
   have to declare new variables. *)

let decl_item :=
  | ~=untyped_decl_item ; ~=as_ty ; < LDI_Typed   >
  | untyped_decl_item

let untyped_decl_item ==
  | ~=IDENTIFIER          ; < LDI_Var     >
  | MINUS                 ; { LDI_Discard }
  | ~=plist2(decl_item)   ; < LDI_Tuple   >

(* ------------------------------------------------------------------------- *)
(* Statement helpers *)

let local_decl_keyword ==
  | LET       ; { LDK_Let       }
  | CONSTANT  ; { LDK_Constant  }
  (* We can't have VAR here otherwise there is a conflict (grammar is not LR1).
  | VAR       ; { LDK_Var       }
  *)

let storage_keyword ==
  | LET       ; { GDK_Let      }
  | CONSTANT  ; { GDK_Constant }
  | VAR       ; { GDK_Var      }
  | CONFIG    ; { GDK_Config   }

let pass == { S_Pass }
let assign(x, y) == ~=x ; EQ ; ~=y ; < S_Assign >
let direction == | TO; { AST.Up } | DOWNTO; { AST.Down }

let case_alt ==
  WHEN; pattern=pattern_list; where=ioption(WHERE; expr); ARROW; stmt=stmt_list;
      { { pattern; where; stmt } }

let case_otherwise ==
  loc=annotated(OTHERWISE); ARROW; stmt=stmt_list;
  { { pattern = add_pos_from loc Pattern_All; where = None; stmt } }

let case_alt_list ==
  nlist_opt_terminated(annotated(case_alt), annotated(case_otherwise))

let otherwise == OTHERWISE; ARROW; stmt_list
let otherwise_opt == ioption(otherwise)
let catcher == WHEN; ~=ioption(terminated(IDENTIFIER, COLON)); ~=ty; ARROW; ~=stmt_list; <>
let loop_limit == ioption(LOOPLIMIT; expr)

let stmt ==
  annotated (
    | terminated_by(end_semicolon,
      | IF; e=expr; THEN; s1=stmt_list; s2=s_else;    <S_Cond>
      | CASE; ~=expr; OF; alt=case_alt_list;          <S_Case>
      | WHILE; ~=expr; ~=loop_limit; DO; ~=stmt_list; <S_While>
      | FOR; index_name=IDENTIFIER; EQ; start_e=expr; dir=direction;
          end_e=expr; limit=loop_limit; DO; body=stmt_list;
          { S_For { index_name; start_e; end_e; dir; body; limit } }
      | TRY; s=stmt_list; CATCH; c=nonempty_list(catcher); o=otherwise_opt; < S_Try >
      | ARROBASE_LOOPLIMIT; looplimit=pared(expr); WHILE; cond=expr; DO; body=stmt_list;
          { S_While (cond, Some looplimit, body) }
    )
    | terminated_by(SEMI_COLON,
      | PASS; pass
      | RETURN; ~=ioption(expr);                             < S_Return >
      | ~=call;                                              < s_call >
      | ASSERT; e=expr;                                      < S_Assert >
      | ~=local_decl_keyword; ~=decl_item; EQ; ~=some(expr); < S_Decl   >
      | le=lexpr; EQ; e=expr;                                < S_Assign >
      | call=annotated(call); EQ; rhs=expr;
        { desugar_setter call [] rhs }
      | call=annotated(call); DOT; fld=IDENTIFIER; EQ; rhs=expr;
        { desugar_setter call [fld] rhs }
      | call=annotated(call); DOT; flds=bracketed(clist2(IDENTIFIER)); EQ; rhs=expr;
        { desugar_setter call flds rhs }
      | ldk=local_decl_keyword; lhs=decl_item; EQ; call=annotated(elided_param_call);
        { desugar_elided_parameter ldk lhs call}
      | VAR; ldi=decl_item; e=ioption(EQ; expr);             { S_Decl (LDK_Var, ldi, e) }
      | VAR; ~=clist2(IDENTIFIER); ~=as_ty;                  < make_ldi_vars >
      | VAR; lhs=decl_item; EQ; call=annotated(elided_param_call);
        { desugar_elided_parameter LDK_Var lhs call}
      | PRINTLN; args=plist(expr);                           { S_Print { args; newline = true; debug = false } }
      | PRINT; args=plist(expr);                             { S_Print { args; newline = false; debug = false } }
      | DEBUG; args=plist(expr);                             { S_Print { args; newline = true; debug = true } }
      | UNREACHABLE; LPAR; RPAR;                             { S_Unreachable }
      | REPEAT; ~=stmt_list; UNTIL; ~=expr; ~=loop_limit;    < S_Repeat >
      | ARROBASE_LOOPLIMIT; looplimit=pared(expr); REPEAT; body=stmt_list; UNTIL; cond=expr;
          { S_Repeat (body, cond, Some looplimit) }
      | THROW; e=expr;                                       { S_Throw (Some (e, None)) }
      | THROW;                                               { S_Throw None             }
      | PRAGMA; x=IDENTIFIER; e=clist(expr);                 < S_Pragma >
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

let subtype == SUBTYPES; ~=IDENTIFIER; ~=loption(WITH; fields); <>
let subtype_opt == option(subtype)

let opt_typed_identifier == pair(IDENTIFIER, ty_opt)
let return_type == ARROW; ty
let params_opt == loption(braced(nclist(opt_typed_identifier)))
(* Uses a dummy call_type, overriden when used above *)
let call ==
  | name=IDENTIFIER; args=plist(expr);
    { { name; params=[]; args; call_type = ST_Function } }
  | name=IDENTIFIER; params=braced(nclist(expr)); args=loption(plist(expr));
    { { name; params; args; call_type = ST_Function } }
let elided_param_call ==
  | name=IDENTIFIER; LBRACE; RBRACE; args=plist(expr);
    { { name; params=[]; args; call_type = ST_Function } }
  | name=IDENTIFIER; LBRACE; COMMA; params=nclist(expr); RBRACE; args=loption(plist(expr));
    { { name; params; args; call_type = ST_Function } }
let func_args == plist(typed_identifier)
let maybe_empty_stmt_list == stmt_list | annotated({ S_Pass })
let func_body == delimited(BEGIN, maybe_empty_stmt_list, end_semicolon)
let recurse_limit == ioption(RECURSELIMIT; expr)
let ignored_or_identifier ==
  | MINUS; { global_ignored () }
  | IDENTIFIER

let decl ==
  annotated (
    (* Begin func_decl *)
    | FUNC; name=IDENTIFIER; ~=params_opt; ~=func_args; ~=return_type; ~=recurse_limit; body=func_body;
        {
          D_Func {
            name;
            parameters = params_opt;
            args = func_args;
            body = SB_ASL body;
            return_type = Some return_type;
            subprogram_type = ST_Function;
            recurse_limit;
            builtin = false;
          }
        }
    (* End *)
    (* Begin procedure_decl *)
    | FUNC; name=IDENTIFIER; ~=params_opt; ~=func_args; body=func_body;
        {
          D_Func {
            name;
            parameters = params_opt;
            args = func_args;
            body = SB_ASL body;
            return_type = None;
            subprogram_type = ST_Procedure;
            recurse_limit = None;
            builtin = false;
          }
        }
    (* End *)
    (* Begin getter *)
    | GETTER; name=IDENTIFIER; ~=params_opt; ~=func_args; ~=return_type;
        ~=func_body;
        {
          D_Func
            {
              name;
              parameters = params_opt;
              args = func_args;
              return_type = Some return_type;
              body = SB_ASL func_body;
              subprogram_type = ST_Getter;
              recurse_limit = None;
              builtin = false;
            }
        }
    (* End *)
    (* Begin setter *)
    | SETTER; name=IDENTIFIER; ~=params_opt; ~=func_args; EQ; v=typed_identifier;
        ~=func_body;
        {
          D_Func
            {
              name;
              parameters = params_opt;
              args = v :: func_args;
              return_type = None;
              body = SB_ASL func_body;
              subprogram_type = ST_Setter;
              recurse_limit = None;
              builtin = false;
            }
        }
    (* End *)
    | terminated_by(SEMI_COLON,
      (* Begin type_decl *)
      | TYPE; x=IDENTIFIER; OF; t=ty_decl; ~=subtype_opt; < D_TypeDecl           >
      (* End *)
      (* Begin subtype_decl *)
      | TYPE; x=IDENTIFIER; s=annotated(subtype);         < make_ty_decl_subtype >
      (* End *)
      (* Begin global_storage *)
      | keyword=storage_keyword; name=ignored_or_identifier;
        ty=ioption(as_ty); EQ; initial_value=some(expr);
        { D_GlobalStorage { keyword; name; ty; initial_value } }
      (* End *)
      (* Begin global_uninit_var *)
      | VAR; name=ignored_or_identifier; ty=some(as_ty);
        { D_GlobalStorage { keyword=GDK_Var; name; ty; initial_value=None}}
      (* End *)
      (* Begin global_pragma *)
      | PRAGMA; x=IDENTIFIER; e=clist(expr); < D_Pragma >
      (* End *)
    )
  )

(* Begin AST *)
let spec := terminated(list(decl), EOF)
(* End *)

let opn := body=stmt; EOF;
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
            recurse_limit = None;
            builtin = false;
          }
        |> ASTUtils.add_pos_from body
      ]
    }

let stmts := terminated(stmt_list,EOF)
