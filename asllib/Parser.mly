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

(*
  Parser Attributes:
    * [@internal: bool] - marks which parse nodes, productions or symbols are not
      a documented part of the ASL language.

  Notes:
    * While attributes can be attached to productions, this appears to
      not always propagate when a production is wrapped by a
      parameterized production. In such cases the attribute has been
      attached to an arbitrary symbol inside the production as a
      workaround as those remain after the inlining completes.
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
let t_bit ~loc = T_Bits (E_Literal (L_Int Z.one) |> add_pos_from loc, [])
let zero ~loc = E_Literal (L_Int Z.zero) |> add_pos_from loc

let make_ldi_vars (xs, ty) =
  let make_one x =
    S_Decl (LDK_Var, LDI_Var x.desc, Some ty, None) |> add_pos_from x
  in
  List.map make_one xs |> stmt_from_list |> desc

let make_ty_decl_subtype (x, s) =
  let name, _fields = s.desc in
  let ty = ASTUtils.add_pos_from s (T_Named name) in
  D_TypeDecl (x, ty, Some s.desc)

let prec =
  let open AST in
  function
  | `BOR | `BAND | `IMPL | `BEQ -> 1
  | `EQ_OP | `NEQ -> 2
  | `PLUS | `MINUS | `OR | `XOR | `AND | `CONCAT -> 3
  | `MUL | `DIV | `DIVRM | `RDIV | `MOD | `SHL | `SHR -> 4
  | `POW -> 5
  | `GT | `GEQ | `LT | `LEQ -> 0 (* Non assoc *)

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

let le_var x = LE_Var x.desc |> add_pos_from x
let lhs_access ~base ~index ~fields = { base; index; fields; slices=add_dummy_annotation ~version [] }

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

(* A non-empty comma-separated list. *)
let clist1(x) :=
  | x=x; { [ x ] }
  | h=x; COMMA; t=clist1(x); { h :: t }

(* A comma separated list. *)
let clist0(x) := { [] } | clist1(x)

(* A comma separated list with at least 2 elements. *)
let clist2(x) := ~=x; COMMA; li=clist1(x); { x :: li }

(* A comma-separated trailing list. *)
let tclist0(x) := { [] } | tclist1(x)

(* A comma-separated non-empty trailing list. *)
let tclist1(x) :=
  | x=x; ioption(COMMA); { [ x ] }
  | h=x; COMMA; t=tclist1(x); { h :: t }

(* A parenthesised comma-separated list *)
let plist0(x) == pared(clist0(x))

(* A parenthesised comma-separated list with at least 2 elements. *)
let plist2(x) == pared(clist2(x))

(* A parameterized list with at least 1 element *)
let list1(x) :=
  | ~=x; { [ x ] }
  | ~=x; l=list1(x); { x :: l }

let end_semicolon ==
  | END; SEMI_COLON; <>
  | END [@internal true]; {
      if not Config.allow_no_end_semicolon then
          Error.fatal_here $startpos $endpos @@ Error.ObsoleteSyntax "Missing ';' after 'end' keyword.";
  }


(* ------------------------------------------------------------------------

                             First parsing rules

  ------------------------------------------------------------------------- *)

let value := (* Also called literal_expr in grammar.bnf *)
  | i=INT_LIT       ; < L_Int         >
  | b=BOOL_LIT      ; < L_Bool        >
  | r=REAL_LIT      ; < L_Real        >
  | b=BITVECTOR_LIT ; < L_BitVector   >
  | s=STRING_LIT    ; < L_String      >

let unop ==
  | BNOT  ; { BNOT }
  | MINUS ; { NEG }
  | NOT   ; { NOT }

let binop ==
  | AND         ; { `AND    }
  | BAND        ; { `BAND   }
  | BOR         ; { `BOR    }
  | BEQ         ; { `BEQ    }
  | DIV         ; { `DIV    }
  | DIVRM       ; { `DIVRM  }
  | XOR         ; { `XOR    }
  | EQ_OP       ; { `EQ_OP  }
  | NEQ         ; { `NEQ    }
  | GT          ; { `GT     }
  | GEQ         ; { `GEQ    }
  | IMPL        ; { `IMPL   }
  | LT          ; { `LT     }
  | LEQ         ; { `LEQ    }
  | PLUS        ; { `PLUS   }
  | MINUS       ; { `MINUS  }
  | MOD         ; { `MOD    }
  | MUL         ; { `MUL    }
  | OR          ; { `OR     }
  | RDIV        ; { `RDIV   }
  | SHL         ; { `SHL    }
  | SHR         ; { `SHR    }
  | POW         ; { `POW    }
  | COLON_COLON ; { `CONCAT }

(* ------------------------------------------------------------------------

                                Expressions

  ------------------------------------------------------------------------- *)

let field_assign := separated_pair(IDENTIFIER, EQ, expr)

let e_else :=
  | ELSE; expr
  | annotated ( ELSIF [@internal true]; c=expr; THEN; e1=expr; e2=e_else; {
      if Config.allow_expression_elsif then E_Cond (c, e1, e2)
      else Error.fatal_here $startpos $endpos @@ Error.ObsoleteSyntax "Expression-level 'elsif'."
    } )

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
    | e=expr; DOT; fs=bracketed(clist1(IDENTIFIER));          < E_GetFields          >

    | ~=expr; AS; ~=ty;                                       < E_ATC                >
    | ~=expr; AS; ~=implicit_t_int;                           < E_ATC                >

    | ~=expr; IN; ~=pattern_set;                              < E_Pattern            >
    | ~=expr; EQ_OP; ~=pattern_mask;                          < E_Pattern            >
    | e=expr; NEQ; p=pattern_mask;                            { E_Pattern (e, Pattern_Not (p) |> add_pos_from p) }
    | ARBITRARY; COLON; ~=ty;                                 < E_Arbitrary        >
    | e=pared(expr);                                          { E_Tuple [ e ]        }
    | t=annotated(IDENTIFIER); LBRACE; MINUS; RBRACE;
        { E_Record (add_pos_from t (T_Named t.desc), []) }
    | t=annotated(IDENTIFIER); fields=braced(clist1(field_assign));
        { E_Record (add_pos_from t (T_Named t.desc), fields) }
    (* Excluded from expr_pattern *)
    | ~=plist2(expr);                                             < E_Tuple              >
  )

(* ------------------------------------------------------------------------

                                Types

  ------------------------------------------------------------------------- *)

(* Constrained types helpers *)

let constraint_kind_opt := constraint_kind | { UnConstrained }
let constraint_kind :=
  | cs=braced(clist1(int_constraint)); { WellConstrained (cs, Precision_Full) }
  | braced(MINUS); { PendingConstrained }

let int_constraint :=
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
    | e=expr_pattern; DOT; fs=bracketed(clist1(IDENTIFIER));          < E_GetFields          >

    | ~=expr_pattern; AS; ~=ty;                                       < E_ATC                >
    | ~=expr_pattern; AS; ~=implicit_t_int;                           < E_ATC                >

    | ~=expr_pattern; IN; ~=pattern_set;                              < E_Pattern            >
    | ~=expr_pattern; EQ_OP; ~=pattern_mask;                          < E_Pattern            >
    | e=expr_pattern; NEQ; p=pattern_mask;                            { E_Pattern (e, Pattern_Not (p) |> add_pos_from p) }

    | ARBITRARY; COLON; ~=ty;                                         < E_Arbitrary        >
    | e=pared(expr_pattern);                                          { E_Tuple [ e ]        }
    | t=annotated(IDENTIFIER); LBRACE; MINUS; RBRACE;
        { E_Record (add_pos_from t (T_Named t.desc), []) }
    | t=annotated(IDENTIFIER); fields=braced(clist1(field_assign));
        { E_Record (add_pos_from t (T_Named t.desc), fields) }
  )

let pattern_mask == annotated(~=MASK_LIT; < Pattern_Mask >)
let pattern_list := annotated(~=clist1(pattern); < Pattern_Any >)

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

let pattern_set :=
  | braced(pattern_list)
  | annotated (
      BNOT; ~=braced(pattern_list); < Pattern_Not >
    )

let fields :=
    braced(MINUS); { [] }
  | braced(tclist1(typed_identifier))
let fields_opt := { [] } | fields

(* Slices *)
let slices := bracketed(clist1(slice))
let slice :=
  | ~=expr;                       < Slice_Single  >
  | e1=expr; COLON; e2=expr;      < Slice_Range   >
  | e1=expr; PLUS_COLON; e2=expr; < Slice_Length  >
  | loc=annotated(COLON); e=expr; { Slice_Length(zero ~loc, e) }
  | e1=expr; STAR_COLON; e2=expr; < Slice_Star    >

(* Bitfields *)
let bitfields_opt := { [] } | bitfields
let bitfields := braced(tclist0(bitfield))
let bitfield :=
  | s=slices ; x=IDENTIFIER ;                 { BitField_Simple (x, s)     }
  | s=slices ; x=IDENTIFIER ; bf=bitfields ;  { BitField_Nested (x, s, bf) }
  | s=slices ; x=IDENTIFIER ; ty=as_ty     ;  { BitField_Type   (x, s, ty) }

(* Also called ty in grammar.bnf *)
let ty :=
  annotated (
    | INTEGER; c = constraint_kind_opt;                 < T_Int        >
    | REAL;                                             { T_Real       }
    | BOOLEAN;                                          { T_Bool       }
    | STRING;                                           { T_String     }
    | loc=annotated(BIT);                               { t_bit ~loc   }
    | BITS; ~=pared(expr); ~=bitfields_opt;             < T_Bits       >
    | l=plist0(ty);                                     < T_Tuple      >
    | name=IDENTIFIER;                                  < T_Named      >
    | ARRAY; LLBRACKET; e=expr; RRBRACKET; OF; t=ty;    { T_Array (ArrayLength_Expr e, t) }
  )

let ty_decl := ty |
  annotated (
    | ENUMERATION; l=braced(tclist1(IDENTIFIER));       < T_Enum       >
    | RECORD; l=fields_opt;                             < T_Record     >
    | EXCEPTION; l=fields_opt;                          < T_Exception  >
    | COLLECTION; l=fields_opt;                         < T_Collection >
  )

(* Constructs on ty *)
(* Begin AsTy *)
let as_ty := COLON; ty
(* End *)

(* Begin TypedIdentifier *)
let typed_identifier := pair(IDENTIFIER, as_ty)
(* End *)

let ty_opt == ioption(as_ty)
let implicit_t_int == annotated ( ~=constraint_kind ; <T_Int> )


(* ------------------------------------------------------------------------

                                Statements

  ------------------------------------------------------------------------- *)

(* Left-hand-side expressions and helpers *)
let basic_lexpr :=
  | base=annotated(IDENTIFIER); fields=nested_fields;
    { lhs_access ~base ~index:None ~fields }
  | base=annotated(IDENTIFIER); LLBRACKET; idx=expr; RRBRACKET; fields=nested_fields;
    { lhs_access ~base ~index:(Some idx) ~fields }

let nested_fields :=
  | { [] }
  | DOT; h=annotated(IDENTIFIER); t=nested_fields; { h :: t }

let sliced_basic_lexpr :=
  | ~=basic_lexpr; <>
  | basic=basic_lexpr; slices=annotated(slices); { {basic with slices} }

let discard_or_sliced_basic_lexpr :=
  | MINUS;                { None }
  | ~=sliced_basic_lexpr; < Some >

let discard_or_field :=
  | MINUS;                   { None }
  | ~=annotated(IDENTIFIER); < Some >

let lexpr :=
  | ~=sliced_basic_lexpr; < desugar_lhs_access >
  | ~=annotated(plist2(discard_or_sliced_basic_lexpr)); < desugar_lhs_tuple >
  | annotated(
    | MINUS; { LE_Discard }
    | x=annotated(IDENTIFIER); DOT; flds=bracketed(clist2(IDENTIFIER));
      { LE_SetFields (le_var x, flds, []) }
    | x=annotated(IDENTIFIER); DOT; flds=pared(clist2(discard_or_field));
      { desugar_lhs_fields_tuple x flds }
  )

(* Decl items are another kind of left-hand-side expressions, which appear only
   on declarations. They cannot have setter calls or set record fields, they
   have to declare new variables. *)

let discard_or_identifier :=
  | MINUS;         { local_ignored () }
  | ~=IDENTIFIER;  <>

let decl_item :=
  | MINUS [@internal true]           ; {
      if Config.allow_storage_discards then LDI_Var (local_ignored ())
      else Error.fatal_here $startpos $endpos @@ Error.ObsoleteSyntax "Discarded storage declaration."
    }
  | ~=IDENTIFIER                     ; < LDI_Var >
  | vs=plist2(discard_or_identifier) ; {
      if List.for_all is_local_ignored vs && not Config.allow_storage_discards then
        Error.fatal_here $startpos $endpos @@ Error.ObsoleteSyntax "Discarded storage declaration."
      else LDI_Tuple vs
    }

(* ------------------------------------------------------------------------- *)
(* Statement helpers *)

let local_decl_keyword_non_var :=
  | LET       ; { LDK_Let       }
  | CONSTANT  ; { LDK_Constant  }
  (* Var is inlined inside stmt as it has differing production choices
  | VAR       ; { LDK_Var       }
  *)

let global_let_or_constant :=
  | LET       ; { GDK_Let      }
  | CONSTANT  ; { GDK_Constant }

let pass == { S_Pass }
let assign(x, y) == ~=x ; EQ ; ~=y ; < S_Assign >
let direction := | TO; { AST.Up } | DOWNTO; { AST.Down }

let case_alt :=
  annotated(
    WHEN; pattern=pattern_list; where=ioption(WHERE; expr); ARROW; stmt=stmt_list;
      { { pattern; where; stmt } }
  )

let case_otherwise ==
    OTHERWISE; ARROW; otherwise_stmt=stmt_list;
      { otherwise_stmt }

let case_alt_list == list1(case_alt)

let otherwise == OTHERWISE; ARROW; stmt_list
let otherwise_opt := ioption(otherwise)
let catcher := WHEN; ~=ioption(terminated(IDENTIFIER, COLON)); ~=ty; ARROW; ~=stmt_list; <>
let loop_limit := ioption(LOOPLIMIT; expr)

let option_eq_expr := ioption(EQ; expr)

let stmt :=
  annotated (
    | terminated_by(end_semicolon,
      | IF; e=expr; THEN; s1=stmt_list; s2=s_else;    <S_Cond>
      | CASE; ~=expr; OF; alt=case_alt_list;
          { desugar_case_stmt expr alt (S_Unreachable |> ASTUtils.add_pos_from expr)}
      | CASE; ~=expr; OF; alt=case_alt_list; ~=case_otherwise;
          { desugar_case_stmt expr alt case_otherwise }
      | WHILE; ~=expr; ~=loop_limit; DO; ~=stmt_list; <S_While>
      | FOR; index_name=IDENTIFIER; EQ; start_e=expr; dir=direction;
          end_e=expr; limit=loop_limit; DO; body=stmt_list;
          { S_For { index_name; start_e; end_e; dir; body; limit } }
      | TRY; s=stmt_list; CATCH; c=list1(catcher); o=otherwise_opt; < S_Try >
    )
    | terminated_by(SEMI_COLON,
      | PASS; pass
      | RETURN; ~=option(expr);                             < S_Return >
      | ~=call;                                              < s_call >
      | ASSERT; e=expr;                                      < S_Assert >
      | ~=local_decl_keyword_non_var; ~=decl_item; ~=ty_opt; EQ; ~=some(expr); < S_Decl   >
      | le=lexpr; EQ; e=expr;                                < S_Assign >
      | call=annotated(call); EQ; rhs=expr;
        { desugar_setter call [] rhs }
      | call=annotated(call); DOT; fld=IDENTIFIER; EQ; rhs=expr;
        { desugar_setter call [fld] rhs }
      | call=annotated(call); DOT; flds=bracketed(clist2(IDENTIFIER)); EQ; rhs=expr;
        { desugar_setter call flds rhs }
      | ldk=local_decl_keyword_non_var; lhs=decl_item; ty=as_ty; EQ; call=annotated(elided_param_call);
        { desugar_elided_parameter ldk lhs ty call}
      | VAR; ldi=decl_item; ty=ty_opt; e=option_eq_expr;      { S_Decl (LDK_Var, ldi, ty, e) }
      | VAR; ~=clist2(annotated(IDENTIFIER)); ~=as_ty;        < make_ldi_vars >
      | VAR; lhs=decl_item; ty=as_ty; EQ; call=annotated(elided_param_call);
        { desugar_elided_parameter LDK_Var lhs ty call}
      | PRINTLN; args=plist0(expr);                           { S_Print { args; newline = true; debug = false } }
      | PRINT; args=plist0(expr);                             { S_Print { args; newline = false; debug = false } }
      | DEBUG; args=plist0(expr);            { S_Print { args; newline = true; debug = true } }
      | UNREACHABLE; LPAR; RPAR;                             { S_Unreachable }
      | REPEAT; ~=stmt_list; UNTIL; ~=expr; ~=loop_limit;    < S_Repeat >
      | THROW; e=expr;                                       { S_Throw (Some (e, None)) }
      | THROW;                                               { S_Throw None             }
      | PRAGMA; x=IDENTIFIER; e=clist0(expr);                 < S_Pragma >
    )
  )

let stmt_list := ~ = list1(stmt) ; <stmt_from_list>

let s_else :=
  annotated (
    | ELSIF; e=expr; THEN; s1=stmt_list; s2=s_else; <S_Cond>
    | pass
  )
  | ELSE; stmt_list

(* ------------------------------------------------------------------------

                                Declarations

  ------------------------------------------------------------------------- *)

let with_opt == { [] } | WITH; ~=fields; <>

let subtype := SUBTYPES; ~=IDENTIFIER; ~=with_opt; <>
let subtype_opt := ioption(subtype)

let opt_typed_identifier := pair(IDENTIFIER, ty_opt)
let return_type := ARROW; ty
let params_opt := { [] } | braced(clist1(opt_typed_identifier))
(* Uses a dummy call_type, overriden when used above *)
let opt_call_args == { [] } | plist0(expr)
let call :=
  | name=IDENTIFIER; args=plist0(expr);
    { { name; params=[]; args; call_type = ST_Function } }
  | name=IDENTIFIER; params=braced(clist1(expr)); args=opt_call_args;
    { { name; params; args; call_type = ST_Function } }
let elided_param_call :=
  | name=IDENTIFIER; LBRACE; RBRACE; args=opt_call_args;
    { { name; params=[]; args; call_type = ST_Function } }
  | name=IDENTIFIER; LBRACE; COMMA; params=clist1(expr); RBRACE; args=opt_call_args;
    { { name; params; args; call_type = ST_Function } }
let func_args := plist0(typed_identifier)
let maybe_empty_stmt_list := stmt_list | annotated({ S_Pass })
let func_body == delimited(BEGIN, maybe_empty_stmt_list, end_semicolon)
let recurse_limit := ioption(RECURSELIMIT; expr)
let ignored_or_identifier :=
  | MINUS [@internal true]; {
      if Config.allow_storage_discards then global_ignored ()
      else Error.fatal_here $startpos $endpos @@ Error.ObsoleteSyntax "Discarded storage declaration."
    }
  | IDENTIFIER
let override ==
  ioption(
    | IMPDEF; { Impdef }
    | IMPLEMENTATION; { Implementation })

let accessors :=
  | GETTER; getter=maybe_empty_stmt_list; end_semicolon;
    SETTER; setter=maybe_empty_stmt_list; end_semicolon;
    { { getter; setter } }
  | SETTER; setter=maybe_empty_stmt_list; end_semicolon;
    GETTER; getter=maybe_empty_stmt_list; end_semicolon;
    { { getter; setter } }

let decl :=
  | d=annotated (
    (* Begin func_decl *)
    | ~=override; FUNC; name=IDENTIFIER; ~=params_opt; ~=func_args; ~=return_type; ~=recurse_limit; body=func_body;
        {
          D_Func {
            name;
            parameters = params_opt;
            args = func_args;
            body = SB_ASL body;
            return_type = Some return_type;
            subprogram_type = ST_Function;
            recurse_limit;
            override;
            builtin = false;
          }
        }
    (* End *)
    (* Begin procedure_decl *)
    | ~=override; FUNC; name=IDENTIFIER; ~=params_opt; ~=func_args; body=func_body;
        {
          D_Func {
            name;
            parameters = params_opt;
            args = func_args;
            body = SB_ASL body;
            return_type = None;
            subprogram_type = ST_Procedure;
            recurse_limit = None;
            override;
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
      | keyword=global_let_or_constant; name=ignored_or_identifier;
        ty=option(as_ty); EQ; initial_value=some(expr);
        { D_GlobalStorage { keyword; name; ty; initial_value } }
      | CONFIG; name=ignored_or_identifier;
        ty=as_ty; EQ; initial_value=some(expr);
        { D_GlobalStorage { keyword=GDK_Config; name; ty=Some ty; initial_value } }
      | VAR; name=ignored_or_identifier;
        ty=option(as_ty); EQ; initial_value=some(expr);
        { D_GlobalStorage { keyword=GDK_Var; name; ty; initial_value } }
      (* End *)
      (* Begin global_uninit_var *)
      | VAR; name=ignored_or_identifier; ty=some(as_ty);
        { D_GlobalStorage { keyword=GDK_Var; name; ty; initial_value=None}}
      (* End *)
      (* Begin global_pragma *)
      | PRAGMA; x=IDENTIFIER; e=clist0(expr); < D_Pragma >
      (* End *)
    )
  ); { [d] }
  | ~=override; ACCESSOR; name=IDENTIFIER; ~=params_opt; ~=func_args; BIARROW; setter_arg=IDENTIFIER; ~=as_ty;
    ~=accessor_body;
    { desugar_accessor_pair override name params_opt func_args setter_arg as_ty accessor_body }

let accessor_body == BEGIN; ~=accessors; end_semicolon;
  { accessors }

(* Begin AST *)
let spec := ~=terminated(list(decl), EOF); < List.concat >
(* End *)

let opn [@internal true] := body=stmt; EOF;
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
            override = None;
            builtin = false;
          }
        |> ASTUtils.add_pos_from body
      ]
    }

let stmts [@internal true] := terminated(stmt_list,EOF)
