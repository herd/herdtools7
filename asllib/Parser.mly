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

%token AND UNKNOWN ARRAY ASSUMES CALL CLASS DO END ENDEVENT ENDIF ENDPROPERTY ENDTRY EXCEPTION FEATURE GIVES IMPORT INVARIANT MAP NEWMAP PARALLEL PRIVATE PUBLIC REQUIRES SET STRING THROW TYPEOF VAR WITH DIV NOT UNSTABLE AS BIT CASE CONFIG DOWNTO ENDCASE ENDFOR ENDMODULE ENDRULE ENDWHILE EXPORT FOR IF INTEGER IS MODULE OF PASS PROFILE REAL RETHROW SETTER SUBTYPES TO UNION WHEN ZTYPE
%token EOR IN OR SAMPLE
%token ANY ASSERT ASSUME BITS BOOLEAN CAST CATCH CONSTANT DICT ELSE ELSIF ENDCATCH ENDCLASS ENDFUNC ENDGETTER ENDNAMESPACE ENDPACKAGE ENDSETTER ENDTEMPLATE ENUMERATION EVENT EXTENDS EXTERN FUNC GETTER IFF IMPLIES INTERSECT INTRINSIC LET LIST NAMESPACE NEWEVENT OTHERWISE PACKAGE PORT PRAGMA PROPERTY PROTECTED RECORD REPEAT RETURN RULE SHARED SIGNAL TEMPLATE THEN TRY TYPE UNTIL USING WHERE WHILE
%token EOF COMMA LT SHR BAND IMPL SHL RBRACKET RPAR SLICING EQ LBRACE NEQ MINUS BEQ LBRACKET LPAR DOT LEQ POW MUL RDIV EQ_OP BOR BNOT PLUS COLON ARROW RBRACE CONCAT COLON_COLON GT PLUS_COLON SEMI_COLON GEQ MOD
%token MEM X PSTATE
%token <string> IDENTIFIER
%token <string> INT_LIT REAL_LIT BITVECTOR_LIT
%token <bool> BOOL_LIT

%type <AST.t> ast

%nonassoc BNOT NOT
%left MINUS
%left PLUS SHR SHL RDIV OR MUL EOR DIV BOR BAND AND IMPL
%nonassoc GT GEQ EQ_OP LT LEQ NEQ
%right SEMI_COLON

%start ast

%%

plist(X):
| xs = delimited(LPAR, separated_list(COMMA, X), RPAR)
    { xs }

value:
| INT_LIT   { AST.V_Int (int_of_string $1) }

%inline unop:
| BNOT { AST.BNOT }
| MINUS { AST.NEG }
| NOT { AST.NOT }

%inline binop:
| AND { AST.AND }
| BAND { AST.BAND }
| BOR { AST.BOR }
| DIV { AST.DIV }
| EOR { AST.EOR }
| EQ_OP { AST.EQ_OP }
| NEQ { AST.NEQ }
| GT { AST.GT }
| GEQ { AST.GEQ }
| IMPL { AST.IMPL }
| LT { AST.LT }
| LEQ { AST.LEQ }
| PLUS { AST.PLUS }
| MINUS { AST.MINUS }
| MUL { AST.MUL }
| OR { AST.OR }
| RDIV { AST.RDIV }
| SHL { AST.SHL }
| SHR { AST.SHR }

expr:
| v=value
    { AST.E_Literal v }
| x=IDENTIFIER
    { AST.E_Var x }
| e1=expr op=binop e2=expr
    { AST.E_Binop (op, e1, e2) }
| op=unop e=expr
    { AST.E_Unop (op, e) }
| IF e1=expr THEN e2=expr ELSE e3=expr END
    { AST.E_Cond (e1, e2, e3) }
| x=IDENTIFIER args=plist(expr)
    { AST.E_Call (x, args) }
| LPAR e=expr RPAR
    { e }
| x=IDENTIFIER LBRACKET args=separated_list(COMMA, expr) RBRACKET
    { AST.E_Get (x, args) }

int_constraint_elt:
| e=expr
  { AST.Constraint_Exact e }
| e1=expr SLICING e2=expr
  { AST.Constraint_Range (e1, e2) }

%inline int_constraints:
| LBRACE l=separated_nonempty_list(COMMA, int_constraint_elt) RBRACE
  { l }

bits_constraint:
| e = expr
  { AST.BitWidth_Determined e }
| MINUS COLON_COLON t = type_desc
  { AST.BitWidth_ConstrainedFormType t }
| c = int_constraints
  { AST.BitWidth_Constrained c }

%inline fields_opt:
| l=loption(delimited(LBRACE, separated_list(COMMA, typed_identifier), RBRACE))
  { l }

type_desc:
| INTEGER
  { AST.T_Int None }
| INTEGER c = int_constraints
  { AST.T_Int (Some c) }
| REAL
  { AST.T_Real }
| BOOLEAN
  { AST.T_Bool }
| STRING
  { AST.T_String }
| BIT
  { AST.T_Bit }
| BITS c = bits_constraint
  { AST.T_Bits c }
| ENUMERATION RBRACE l=separated_nonempty_list(COMMA, IDENTIFIER) COMMA? RBRACE
  { AST.T_Enum l }
| l=plist(type_desc)
  { AST.T_Tuple l }
| ARRAY LBRACKET e=expr RBRACKET OF t=type_desc
  { AST.T_Array (e, t) }
| RECORD l=fields_opt
  { AST.T_Record l }
| EXCEPTION l=fields_opt
  { AST.T_Exception l }
| ZTYPE LPAR t=type_desc RPAR
  { AST.T_ZType t }
| name=IDENTIFIER
  { AST.T_Named name }

%inline typed_identifier:
| n=IDENTIFIER COLON_COLON t=type_desc
  { n, t }

lexpr:
| x=IDENTIFIER
    { AST.LEVar x }
| x=IDENTIFIER LBRACKET args=separated_list(COMMA, expr) RBRACKET
    { AST.LESet (x, args) }

stmt:
| PASS
    { AST.S_Pass }
| stmt SEMI_COLON stmt
    { AST.S_Then ($1, $3) }
| lexpr EQ expr
    { AST.S_Assign ($1, $3) }
| IF e=expr THEN s1=stmt SEMI_COLON? ELSE s2=stmt SEMI_COLON? END
    { AST.S_Cond (e, s1, s2) }
| x=IDENTIFIER args=plist(expr)
    { AST.S_Call (x, args) }
| RETURN es=separated_list(COMMA, expr)
    { AST.S_Return es }

%inline func_keyword:
| FUNC
| GETTER
| SETTER
    { () }

decl:
| CONSTANT x=IDENTIFIER EQ e=expr SEMI_COLON
    { AST.D_GlobalConst (x, e) }
| TYPE x=IDENTIFIER OF t=type_desc
    { AST.D_TypeDecl (x, t) }
| func_keyword name=IDENTIFIER args=plist(typed_identifier) return_type=option(preceded(ARROW, type_desc)) body=stmt SEMI_COLON? END
    { AST.(D_Func { name; args; body; return_type }) }

ast:
| ds=decl* EOF
    { ds }
