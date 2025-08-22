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

%{

  module Prelude = struct
    open AST
    open ASTUtils

    let version = V0

    let build_expr_conds =
      let make_cond { desc = c, e_then; _ } e_else =
        E_Cond (c, e_then, e_else)
      in
      fun (elseifs, e) -> List.fold_right (map2_desc make_cond) elseifs e

    let build_stmt_conds (s_elsifs, s_else) =
      let s_else = match s_else with Some s -> s | None -> s_pass in
      let folder { desc = c, s_then; _ } s_else = S_Cond (c, s_then, s_else) in
      List.fold_right (map2_desc folder) s_elsifs s_else

    let t_bit =
      T_Bits (E_Literal (L_Int Z.one) |> add_dummy_annotation ~version, [])


    let make_ldi_vars (ty, xs) =
      let make_one x =
        S_Decl (LDK_Var, LDI_Var x, Some ty, None)
        |> add_dummy_annotation ~version
      in
      List.map make_one xs |> stmt_from_list |> desc

    let make_func name args return_type body =
      let subprogram_type =
        match return_type with | Some _ -> ST_Function | None -> ST_Procedure
      and parameters = []
      and recurse_limit = None
      in
      D_Func {
        name;
        args;
        return_type;
        body;
        parameters;
        subprogram_type;
        recurse_limit;
        override = None;
        qualifier = None;
        builtin = false;
      }

    let make_concat es =
      match es with
      | [] -> E_Literal (L_BitVector Bitvector.empty)
      | [ bv ] -> bv |> desc
      | bv :: bvs -> List.fold_left (binop `BV_CONCAT) bv bvs |> desc

  end

  open Prelude
  open Desugar
%}

%token <string> IDENTIFIER STRING_LIT
%token <Bitvector.mask> MASK_LIT
%token <Bitvector.t> BITS_LIT
%token <Z.t> INT_LIT
%token <Q.t> REAL_LIT
%token <bool> BOOL_LIT
%token <string> QUALIFIER

%token AMP
%token AMP_AMP
%token AND
%token ARRAY
%token ASSERT
%token BANG
%token BANG_EQ
%token BAR_BAR
%token BIT
%token BITS
%token BOOLEAN
%token CARET
%token CASE
%token CATCH
%token COLON
%token COMMA
%token CONFIG
%token CONSTANT
%token CONSTRAINED_UNPRED
%token DEBUG
%token DEDENT
%token DIV
%token DO
%token DOT
%token DOT_DOT
%token DOWNTO
%token ELSE
%token ELSIF
%token ENUMERATION
%token EOF
%token EOL
%token XOR
%token EQ
%token EQ_EQ
%token EQ_GT
%token FOR
%token GT
%token GT_EQ
%token GT_GT
%token IF
%token IFF
%token IMPLEM_DEFINED
%token IMPLIES
%token IN
%token INDENT
%token INTEGER
%token IS
%token LET
%token LBRACE
%token LBRACE_LBRACE
%token LBRACK
%token LIMIT
%token LPAREN
%token LT
%token LT_EQ
%token LT_LT
%token MINUS
%token MOD
%token NOT
%token OF
%token OR
%token OTHERWISE
%token PLUS
%token PLUS_COLON
%token PLUS_PLUS
%token QUOT
%token RBRACE
%token RBRACE_RBRACE
%token RBRACK
%token REAL
%token RECORD
%token REM
%token REPEAT
%token RETURN
%token RPAREN
%token SEE
%token SEMICOLON
%token SLASH
%token STAR
%token THEN
%token THROW
%token TO
%token TRY
%token TYPE
%token TYPEOF
%token UNDEFINED
%token UNKNOWN
%token UNPREDICTABLE
%token UNREACHABLE
%token UNTIL
%token UU_ARRAY
%token UU_BUILTIN
%token UU_CONDITIONAL
%token UU_CONFIG
%token UU_DECODE
%token UU_ENCODING
%token UU_EVENT
%token UU_EXCEPTIONTAKEN
%token UU_EXECUTE
%token UU_FIELD
%token UU_FUNCTION
%token UU_GUARD
%token UU_INSTRUCTION
%token UU_INSTRUCTION_SET
%token UU_MAP
%token UU_NEWEVENT
%token UU_NEWMAP
%token UU_NOP
%token UU_OPCODE
%token UU_OPERATOR_ONE
%token UU_OPERATOR_TWO
%token UU_POSTDECODE
%token UU_READWRITE
%token UU_REGISTER
%token UU_UNALLOCATED
%token UU_UNPREDICTABLE
%token UU_UNPREDICTABLE_UNLESS
%token UU_WRITE
%token WHEN
%token WHILE

%type <AST.stmt> simple_stmts
%type <AST.stmt> simple_stmt_list
%type <AST.stmt> stmts
%type <AST.t> spec
%type <AST.t> opn
%start spec
%start opn

%nonassoc ELSE
%left COLON
%left AMP_AMP BAR_BAR IMPLIES
%left EQ_EQ BANG_EQ
%nonassoc GT_EQ LT_EQ LT GT
%left PLUS MINUS XOR AND OR
%left STAR SLASH MOD LT_LT GT_GT DIV
%left CARET
%nonassoc UNOPS
%nonassoc IN
%left LBRACK
%left DOT

%%

let filter(x) == ~=x; { List.filter_map Fun.id x }
let some(x) == ~=x; < Some >

let spec := list(EOL); terminated (filter(list(decl)), EOF)

let opn := list(EOL); body=list(stmts); EOF;
    {
      let body = ASTUtils.stmt_from_list body in
      AST.[
        D_Func {
          name = "main";
          args = [];
          parameters = [];
          body = SB_ASL body;
          return_type = None;
          subprogram_type = ST_Procedure;
          recurse_limit = None;
          override = None;
          qualifier = None;
          builtin = false;
        } |> ASTUtils.add_pos_from body
      ]
    }

let decl ==
  | variable_decl
  | function_decl
  | procedure_decl
  | getter_decl
  | setter_decl
  | type_decl

let annotated(x) == desc = x; { AST.{ desc; pos_start=$symbolstartpos; pos_end=$endpos; version }}

let unimplemented_decl(x) == x; { None }
let unimplemented_ty(x) == x; { AST.(T_Bits (ASTUtils.expr_of_int 0, [])) }


let type_decl ==
  some (
    annotated (
      | terminated_by(SEMICOLON; EOL,
        | TYPE; x=tidentdecl; EQ; ~=ty;
          { AST.(D_TypeDecl (x, ty, None)) }
        | RECORD; x=tidentdecl; fields=annotated(braced(nlist(field)));
          { AST.(D_TypeDecl (x, ASTUtils.add_pos_from fields (T_Record fields.desc), None)) }
        | ENUMERATION; x=tidentdecl; li=annotated(braced(ntclist(ident)));
          { AST.(D_TypeDecl (x, ASTUtils.add_pos_from li (T_Enum li.desc), None)) }

        | TYPE; t=tidentdecl; ty=annotated(unimplemented_ty(<>));
          { AST.D_TypeDecl (t, ty, None) }
      )

      | TYPE; x=tidentdecl; IS; li=annotated(pared(ntclist(field_ns))); EOL;
        { AST.(D_TypeDecl (x, ASTUtils.add_pos_from li (T_Record li.desc), None)) }
    )
  )

let tidentdecl ==
  | ident
  | q=QUALIFIER; DOT; i=ident; { q ^ "_" ^ i }

let field == terminated(field_ns, SEMICOLON)
let field_ns == t=ty; x=ident; { (x, t) }

let bracketed(x) == delimited(LBRACK, x, RBRACK)
let braced(x) == delimited(LBRACE, x, RBRACE)
let pared(x) == delimited(LPAREN, x, RPAREN)
let clist(x) == { [] } | nclist(x)
let nlist(x) == nonempty_list(x)
let nclist(x) == separated_nonempty_list(COMMA, x)
let nnclist(x) == h=x; COMMA; t=nclist(x); { h::t }
let terminated_by (y, x) == terminated(x, y)

let ntclist(x) :=
    | ~=x; ioption(COMMA);      { [ x ]  }
    | ~=x; COMMA; t=ntclist(x); { x :: t }

let tclist(x) == loption(ntclist(x))
let ty :=
  annotated (
    | ty_non_tuple
    | ~=pared(nnclist(ty)); < AST.T_Tuple >
  )

let bitfields == braced(tclist(bitfield))
let bitfield == s=nclist(slice); x=ident; { AST.BitField_Simple(x, s) }

let ty_non_tuple ==
  | INTEGER;              { AST.(T_Int UnConstrained) }
  | REAL;                 { AST.T_Real                }
  | BOOLEAN;              { AST.T_Bool                }
  | ~=tident;             < AST.T_Named               >
  | BIT;                  { t_bit                     }
  | BITS; e=pared(expr);  { AST.(T_Bits (e, []))      }
  | BITS; e=pared(expr); b=bitfields; { AST.(T_Bits (e, b)) }
  (* | tident; pared(clist(expr)); <> *)

  | unimplemented_ty (
    | TYPEOF; pared(expr); <>
    | ARRAY; bracketed(ixtype); OF; ty; <>
  )

let ixtype ==
  | tident; <>
  | expr; DOT_DOT; expr; <>

let tident ==
  | typeident
  | q=QUALIFIER; DOT; i=typeident; { q ^ "_" ^ i }

let typeident == typeid
let typeid == ident
let ident == IDENTIFIER
let ident_plus_record == ident | RECORD; { "record" }

let qualident ==
    | q=QUALIFIER; DOT; i=ident; { q ^ "_" ^ i }
    | ident
    | RECORD; { "record" }

let unimplemented_expr(x) == x; { AST.(E_Literal (L_Bool true)) }
let nargs == { [] }

let sexpr := binop_expr(sexpr, abinop)
let expr :=
  | binop_expr(expr, binop)
  | annotated (
      e1=expr; COLON; e2=expr;
          { AST.E_Binop (`BV_CONCAT, e1, e2) }
  )

let binop_expr(e, b) ==
  | pared(expr)
  | annotated (
      | ~=literal_expression;                         < AST.E_Literal   >
      | ~=qualident;                                  < AST.E_Var       >
      | name=qualident; args=pared(clist(expr)); params=nargs;
        { AST.E_Call { name; args; params; call_type = ST_Function } }
      | name=qualident; params=braced(clist(expr)); args=pared(clist(expr));
        { AST.E_Call { name; args; params; call_type = ST_Function } }
      | ~=unop; ~=e;                                  < AST.E_Unop      > %prec UNOPS
      | e1=e; op=b; e2=e;                             { AST.E_Binop (op, e1, e2) }
      | ~=pared(nnclist(expr));                       < AST.E_Tuple     >
      | IF; c=expr; THEN; e=expr; ~=e_else;           < AST.E_Cond      >
      | ~=e; DOT; ~=ident;                            < AST.E_GetField  >
      | ~=e; DOT; ~=bracketed(clist(ident));          < AST.E_GetFields >
      | ~=e; ~=bracketed(clist(slice));               < AST.E_Slice     >
      | ~=bracketed(clist(expr));                     < make_concat     >
      | ~=e; IN; ~=bpattern;                          < AST.E_Pattern   >
      | ~=e; EQ_EQ; ~=pattern_mask;                   < AST.E_Pattern   >
      | ~=e; ~=annotated(BANG_EQ; pm=pattern_mask; < AST.Pattern_Not >); < AST.E_Pattern >
      | ~=annotated(ty_non_tuple); UNKNOWN;           < AST.E_Arbitrary >
      (*
      | ~=e; LT; ~=clist(slice); GT;          < AST.E_Slice     >
      *)

      | unimplemented_expr(
        | ty_non_tuple; IMPLEM_DEFINED; ioption(STRING_LIT); <>
      )
    )

let e_elseif == annotated ( ELSIF; c=expr; THEN; e=expr; <> )
let e_else == ~=list(e_elseif); ELSE; ~=expr; < build_expr_conds >

let slice ==
  | ~=sexpr;                        < AST.Slice_Single >
  | e1=sexpr; COLON; e2=sexpr;      < AST.Slice_Range  >
  | e1=sexpr; PLUS_COLON; e2=sexpr; < AST.Slice_Length >

let literal_expression ==
  | ~=BOOL_LIT;      < AST.L_Bool       >
  | ~=INT_LIT;       < AST.L_Int        >
  | ~=REAL_LIT;      < AST.L_Real       >
  | ~=BITS_LIT;      < AST.L_BitVector  >
  | ~=STRING_LIT;    < AST.L_String     >

let array_length ==
  | expr
  | expr; DOT_DOT; expr

let gdk ==
  | CONSTANT;   { AST.GDK_Constant  }
  | CONFIG;     { AST.GDK_Config    }
  |             { AST.GDK_Var       }

let variable_decl ==
  terminated_by (SEMICOLON; EOL,
    | some (annotated (
      | gdk=gdk; t=ty; x=qualident; EQ; e=expr;
          { AST.D_GlobalStorage {
            keyword = gdk;
            name = x;
            initial_value = Some e;
            ty = Some t;
          } }
      | ty=ty; x=qualident;
          { AST.D_GlobalStorage {
            keyword = GDK_Var;
            name = x;
            ty = Some ty;
            initial_value = None;
          }}
      | ARRAY; ty=ty; x=qualident; e=bracketed(array_length);
          { AST.(D_GlobalStorage {
            keyword = GDK_Var;
            name = x;
            ty = Some (T_Array (ArrayLength_Expr e, ty)
                       |> ASTUtils.add_dummy_annotation ~version);
            initial_value = None;
          })}
      )))

let function_decl ==
  | some (annotated (
      ty=ty; name=qualident; args=pared(clist(formal)); body=indented_block;
        { make_func name args (Some ty) (SB_ASL body) }
    ))
  | some (annotated (
      LPAREN; RPAREN; name=qualident; args=pared(clist(formal)); body=indented_block;
        { make_func name args None (SB_ASL body) }
    ))
  | unimplemented_decl (
      some(ty); qualident; pared(clist(formal)); ioption(SEMICOLON); EOL
    )

let getter_decl ==
  | some (annotated (
    | ~=ty; name=qualident; body = indented_block;
      {
        let open AST in
        let return_type = Some ty
        and args = []
        and body = SB_ASL body
        and subprogram_type = ST_EmptyGetter
        and recurse_limit = None
        and parameters = [] in
        D_Func {
          name;
          args;
          return_type;
          body;
          parameters;
          subprogram_type;
          recurse_limit;
          override = None;
          qualifier = None;
          builtin = false;
        }
      }
    | ~=ty; name=qualident; body=opt_indented_block; SEMICOLON; EOL;
      {
        let open AST in
        let return_type = Some ty
        and args = []
        and body = SB_ASL body
        and recurse_limit = None
        and subprogram_type = ST_EmptyGetter
        and parameters = [] in
        D_Func {
          name;
          args;
          return_type;
          body;
          parameters;
          subprogram_type;
          recurse_limit;
          override = None;
          qualifier = None;
          builtin = false;
        }
      }
    | ~=ty; name=qualident; args=bracketed(clist(formal)); body=indented_block;
      {
        let open AST in
        let return_type = Some ty
        and subprogram_type = ST_Getter
        and recurse_limit = None
        and body = SB_ASL body
        and parameters = [] in
        D_Func {
          name;
          args;
          return_type;
          body;
          parameters;
          subprogram_type;
          recurse_limit;
          override = None;
          qualifier = None;
          builtin = false;
        }
      }
  ))
  | unimplemented_decl (
      ty; qualident; bracketed(clist(formal)); ioption(SEMICOLON); EOL
  )

let setter_args == bracketed(clist(sformal))
let setter_decl ==
  some (annotated (
    | name=qualident; EQ; ~=ty; ~=ident; body=indented_block;
      {
        let open AST in
        let return_type = None
        and parameters = []
        and body = SB_ASL body
        and subprogram_type = ST_EmptySetter
        and recurse_limit = None
        and args = [ (ident, ty) ] in
        D_Func {
          name;
          args;
          return_type;
          body;
          parameters;
          subprogram_type;
          recurse_limit;
          override = None;
          qualifier = None;
          builtin = false;
        }
      }
    | name=qualident; args=setter_args; EQ; ~=ty; ~=ident; body=indented_block;
      {
        let open AST in
        let return_type = None
        and parameters = []
        and body = SB_ASL body
        and subprogram_type = ST_Setter
        and recurse_limit = None
        and args = (ident, ty) :: args in
        D_Func {
          name;
          args;
          return_type;
          body;
          parameters;
          subprogram_type;
          recurse_limit;
          override = None;
          qualifier = None;
          builtin = false;
        }
      }
  ))
  | unimplemented_decl (
      qualident; ioption(setter_args); EQ; ty; ident; ioption(SEMICOLON); EOL
    )

let procedure_decl ==
  | some (annotated (
      name=qualident; args=pared(clist(formal)); body=indented_block;
        {
          let open AST in
          let return_type = None
          and parameters = []
          and body = SB_ASL body
          and recurse_limit = None
          and subprogram_type = ST_Procedure in
          D_Func {
            name;
            args;
            return_type;
            body;
            parameters;
            subprogram_type;
            recurse_limit;
            override = None;
            qualifier = None;
            builtin = false;
          }
        }
    ))
  | unimplemented_decl (
      qualident; pared(clist(formal)); ioption(SEMICOLON); EOL
    )

let sformal == t=ty; ioption(AMP); x=ident; { (x, t) }
let formal == t=ty; x=ident; { (x, t) }

let s_eol == EOL; { ASTUtils.s_pass }

let opt_indented_block ==
  | indented_block
  | s_eol

let indented_block ==
  | EOL; INDENT; ~=nlist(stmts); DEDENT; < ASTUtils.stmt_from_list >

(* Always terminated by EOL or indented block. *)
let possibly_empty_block ==
  | indented_block
  | simple_stmts
  | s_eol

let unimplemented_stmts(x) == x; { AST.S_Pass }

let stmts ==
  | simple_stmts
  | compound_stmt

(* Always terminated by EOL *)
let simple_stmts ==
  | annotated (
    ~=simple_stmt_list; ~=simple_if_stmt; < AST.S_Seq >
  )
  | terminated(simple_stmt_list, EOL)

let simple_stmt_list == ~=nlist(simple_stmt); < ASTUtils.stmt_from_list >

let simple_stmt ==
  | assignment_stmt
  | annotated ( terminated_by (SEMICOLON,
    | name=qualident; args=pared(clist(expr)); params=nargs;
      { AST.S_Call { name; args; params ; call_type = ST_Procedure } }
    | name=qualident; params=braced(nclist(expr)); args=pared(clist(expr));
      { AST.S_Call { name; args; params ; call_type = ST_Procedure } }
    | RETURN; ~=ioption(expr);                    < AST.S_Return >
    | ASSERT; ~=expr;                             < AST.S_Assert >
    | DEBUG; e=expr;                              { AST.S_Print { args = [ e ]; newline = true; debug = true } }
    | UNREACHABLE; LPAREN; RPAREN;                { AST.S_Unreachable }

    | unimplemented_stmts (
      | UNPREDICTABLE; ioption(pared(<>)); <>
      | CONSTRAINED_UNPRED; <>
      | IMPLEM_DEFINED; pared(ident); <>
      | UNDEFINED; ioption(pared(<>)); <>
      | IMPLEM_DEFINED; ioption(STRING_LIT); <>
      | SEE; pared(expr); <>
      | SEE; STRING_LIT; <>
      | SEE; ident; <>
      | THROW; ident; <>
    )
  ))

let ident_or_record_or_discard ==
  | ~=ident_plus_record; <>
  | MINUS;               { ASTUtils.fresh_var "__ldi_discard" }

let assignment_stmt ==
  annotated (
    terminated_by(SEMICOLON,
      | le=lexpr; EQ; e=expr; < AST.S_Assign >
      | ty=annotated(ty_non_tuple); x=ident_plus_record; EQ; ~=expr;
        { AST.(S_Decl (LDK_Var, LDI_Var x, Some ty, Some expr)) }
      | CONSTANT; ty=annotated(ty_non_tuple); x=ident_plus_record; EQ; ~=expr;
        { AST.(S_Decl (LDK_Let, LDI_Var x, Some ty, Some expr)) }
      | CONSTANT; x=ident; EQ; ~=expr;
        { AST.(S_Decl (LDK_Let, LDI_Var x, None, Some expr)) }
      | CONSTANT; names=pared(nnclist(ident_or_record_or_discard)); EQ; e=expr;
        { AST.S_Decl (LDK_Let, LDI_Tuple names, None, Some e) }
      | t=annotated(ty_non_tuple); li=nclist(ident); < make_ldi_vars >
      ))

let le_var == ~=qualident; < AST.LE_Var >
let lexpr_ignore == { AST.LE_Discard }
let unimplemented_lexpr(x) == x; lexpr_ignore

let lexpr :=
  annotated (
    | MINUS; lexpr_ignore
    | le_var
    | ~=lexpr; ~=bracketed(clist(slice));      < AST.LE_Slice       >
    | ~=lexpr; LT; ~=clist(slice); GT;         < AST.LE_Slice       >
    | ~=pared(nclist(lexpr));                  < AST.LE_Destructuring >
    | ~=lexpr; DOT; ~=ident;                   < AST.LE_SetField    >
    | l=lexpr; DOT; f=bracketed(clist(ident)); { AST.LE_SetFields (l, f, []) }

    | unimplemented_lexpr (
      | bracketed(nclist(lexpr)); <>
    )
  )

let simple_if_stmt ==
    annotated (
      IF; ~=expr; THEN; ~=simple_stmt_list; ~=simple_else_opt; EOL; < AST.S_Cond >
    )

let simple_else_opt == ~=list(simple_elsif); ~=ioption(ELSE; simple_stmt_list); < build_stmt_conds >
let simple_else     == ~=list(simple_elsif); ~=   some(ELSE; simple_stmt_list); < build_stmt_conds >
let simple_elsif == annotated ( ELSIF; ~=expr; THEN; ~=simple_stmt_list; <> )

let compound_stmt ==
  annotated (
    | conditional_stmt
    | repetitive_stmt
    |  unimplemented_stmts (
      | catch_stmt; <>
    )
  )

let conditional_stmt ==
  (* The first two cases of asl.ott are united in this simpler rule. *)
  | IF; ~=expr; THEN; ~=possibly_empty_block; ~=s_else;       < AST.S_Cond >
  | IF; ~=expr; THEN; ~=simple_stmt_list; ~=simple_else; EOL; < AST.S_Cond >
  | CASE; ~=expr; OF; EOL; INDENT; alts=list(alt); DEDENT;
    { desugar_case_stmt expr alts (AST.S_Unreachable |> ASTUtils.add_pos_from expr) }

let s_elsif == annotated ( ELSIF; ~=expr; THEN; ~=possibly_empty_block; <> )
let s_else == ~=list(s_elsif); ~=ioption(ELSE; possibly_empty_block); < build_stmt_conds >

let alt ==
  annotated (
    | WHEN; pattern=pattern_list; where=opt_where; stmt=possibly_empty_block;
        { AST.{ pattern; where; stmt } }
    | WHEN; pattern=pattern_list; where=opt_where; stmt=simple_if_stmt;
        { AST.{ pattern; where; stmt } }
    | loc=annotated(OTHERWISE); stmt=possibly_empty_block;
        { AST.{ pattern = ASTUtils.add_pos_from loc Pattern_All; where = None; stmt } }
  )

let opt_where ==
  | { None }
  | ~=ioption(AND; expr); EQ_GT; <>

let otherwise == annotated (OTHERWISE; possibly_empty_block)

let pattern_mask == annotated(~=MASK_LIT; <AST.Pattern_Mask>)
let pattern_all == annotated(MINUS; { AST.Pattern_All })
let pattern_list == annotated(~=nclist(pattern); < AST.Pattern_Any >)
let pattern ==
    | bpattern
    | pattern_mask
    | pattern_all
    | annotated (
      | ~=annotated (
        | ~=literal_expression; < AST.E_Literal >
        | ~=qualident; < AST.E_Var >
      ); < AST.Pattern_Single >
    )
let bpattern == annotated(braced(~=nclist(apattern); < AST.Pattern_Any >))
let apattern ==
  | annotated (
    | ~=expr; < AST.Pattern_Single >
    | e1=expr; DOT_DOT; e2=expr; < AST.Pattern_Range >
  )
  | pattern_all
  | pattern_mask

let repetitive_stmt ==
  | FOR; index_name=ident; EQ; start_e=expr; dir=direction; end_e=expr;
      limit=ioption(LIMIT; expr); body=indented_block;
      { AST.S_For { index_name; start_e; dir; end_e; body; limit } }
  | WHILE; ~=expr; ~=ioption(LIMIT; expr); DO; ~=indented_block; <AST.S_While>
  | REPEAT; ~=indented_block; UNTIL; ~=expr; ~=ioption(LIMIT; expr);
      SEMICOLON; EOL; <AST.S_Repeat>

let direction == | TO; { AST.Up } | DOWNTO; { AST.Down }

let catch_stmt ==
  | TRY; indented_block; CATCH; ident; EOL; INDENT; nlist(catcher); ioption(otherwise); DEDENT; <>

let catcher == WHEN; expr; opt_indented_block; <>

let unop ==
  | BANG  ; { AST.BNOT }
  | MINUS ; { AST.NEG }
  | NOT   ; { AST.NOT }

let unimplemented_binop(x) == x ; { `ADD }

let abinop ==
  | AND        ; { `AND    }
  | AMP_AMP    ; { `BAND   }
  | BAR_BAR    ; { `BOR    }
  | DIV        ; { `DIV    }
  | XOR        ; { `XOR    }
  | EQ_EQ      ; { `EQ     }
  | BANG_EQ    ; { `NE     }
  | GT_EQ      ; { `GE     }
  | IMPLIES    ; { `IMPL   }
  | LT_EQ      ; { `LE     }
  | PLUS       ; { `ADD    }
  | MINUS      ; { `SUB    }
  | MOD        ; { `MOD    }
  | STAR       ; { `MUL    }
  | OR         ; { `OR     }
  | SLASH      ; { `RDIV   }
  | LT_LT      ; { `SHL    }
  | GT_GT      ; { `SHR    }

  | unimplemented_binop(
    | CARET
  )

let binop ==
  | abinop
  | LT         ; { `LT     }
  | GT         ; { `GT     }
