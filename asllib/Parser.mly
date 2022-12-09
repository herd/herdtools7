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

%token AND UNKNOWN ARRAY ASSUMES CALL CLASS DO END ENDEVENT ENDIF ENDPROPERTY ENDTRY EXCEPTION FEATURE GIVES IMPORT INVARIANT MAP NEWMAP PARALLEL PRIVATE PUBLIC REQUIRES SET STRING THROW TYPEOF VAR WITH DIV NOT UNSTABLE AS BIT CASE CONFIG DOWNTO ENDCASE ENDFOR ENDMODULE ENDRULE ENDWHILE EXPORT FOR IF INTEGER IS MODULE OF PASS PROFILE REAL RETHROW SETTER SUBTYPES TO UNION WHEN ZTYPE EOR IN OR SAMPLE ANY ASSERT ASSUME BITS BOOLEAN CAST CATCH CONSTANT DICT ELSE ELSIF ENDCATCH ENDCLASS ENDFUNC ENDGETTER ENDNAMESPACE ENDPACKAGE ENDSETTER ENDTEMPLATE ENUMERATION EVENT EXTENDS EXTERN FUNC GETTER IFF IMPLIES INTERSECT INTRINSIC LET LIST NAMESPACE NEWEVENT OTHERWISE PACKAGE PORT PRAGMA PROPERTY PROTECTED RECORD REPEAT RETURN RULE SHARED SIGNAL TEMPLATE THEN TRY TYPE UNTIL USING WHERE WHILE
%token EOF COMMA LT SHR BAND IMPL SHL RBRACKET RPAR SLICING EQ LBRACE NEQ MINUS BEQ LBRACKET LPAR DOT LEQ POW MUL RDIV EQ_OP BOR BNOT PLUS COLON ARROW RBRACE CONCAT COLON_COLON GT PLUS_COLON SEMI_COLON GEQ MOD
%token MEM X PSTATE
%token <string> IDENTIFIER
%token <string> INT_LIT REAL_LIT BITVECTOR_LIT
%token <bool> BOOL_LIT

%type <AST.parsed_t> ast

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
| INT_LIT   { AST.VInt (int_of_string $1) }

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
    { AST.ELiteral v }
| x=IDENTIFIER
    { AST.EVar x }
| e1=expr op=binop e2=expr
    { AST.EBinop (op, e1, e2) }
| op=unop e=expr
    { AST.EUnop (op, e) }
| IF e1=expr THEN e2=expr ELSE e3=expr END
    { AST.ECond (e1, e2, e3) }
| x=IDENTIFIER args=plist(expr)
    { AST.ECall (x, args) }
| LPAR e=expr RPAR
    { e }
| x=IDENTIFIER LBRACKET args=separated_list(COMMA, expr) RBRACKET
    { AST.EGet (x, args) }

lexpr:
| x=IDENTIFIER
    { AST.LEVar x }
| x=IDENTIFIER LBRACKET args=separated_list(COMMA, expr) RBRACKET
    { AST.LESet (x, args) }

stmt:
| PASS
    { AST.SPass }
| stmt SEMI_COLON stmt
    { AST.SThen ($1, $3) }
| lexpr EQ expr
    { AST.SAssign ($1, $3) }
| IF e=expr THEN s1=stmt SEMI_COLON? ELSE s2=stmt SEMI_COLON? END
    { AST.SCond (e, s1, s2) }
| x=IDENTIFIER args=plist(expr)
    { AST.SCall (x, args) }
| RETURN es=separated_list(COMMA, expr)
    { AST.SReturn es }

%inline func_keyword:
| FUNC
| GETTER
| SETTER
    { () }

decl:
| CONSTANT x=IDENTIFIER EQ e=expr SEMI_COLON
    { AST.GlobalConst (x, e) }
| TYPE IDENTIFIER OF ENUMERATION LBRACE ls=separated_list(COMMA, IDENTIFIER) RBRACE
    { AST.Enum ls }
| func_keyword x=IDENTIFIER args=plist(IDENTIFIER) body=stmt SEMI_COLON? ENDFUNC
    { AST.Func (x, args, body) }

ast:
| ds=decl* EOF
    { ds }
