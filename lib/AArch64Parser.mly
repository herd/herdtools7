%{
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


open AArch64Base
%}

%token EOF
%token <AArch64Base.reg> ARCH_XREG
%token <string> SYMB_XREG
%token <AArch64Base.reg> ARCH_WREG
%token <string> SYMB_WREG
%token <int> NUM
%token <string> NAME
%token <string> META
%token <string> CODEVAR
%token <int> PROC

%token SEMI COMMA PIPE COLON LBRK RBRK
%token SXTW

/* Instructions */
%token NOP
%token B BR BEQ BNE CBZ CBNZ EQ NE
%token BL BLR RET
%token LDR LDP LDNP STP STNP LDRB LDRH STR STRB STRH STLR STLRB STLRH
%token CMP MOV ADR
%token  LDAR LDARB LDARH LDAPR LDAPRB LDAPRH  LDXR LDXRB LDXRH LDAXR LDAXRB LDAXRH
%token STXR STXRB STXRH STLXR STLXRB STLXRH
%token <AArch64Base.op> OP
%token CSEL CSINC CSINV CSNEG CSET
%token DMB DSB ISB
%token SY ST LD
%token OSH OSHST OSHLD
%token ISH ISHST ISHLD
%token NSH NSHST NSHLD
%token CAS CASA CASL CASAL CASB CASAB CASLB CASALB CASH CASAH CASLH CASALH
%token SWP SWPA SWPL SWPAL SWPB SWPAB SWPLB SWPALB SWPH SWPAH SWPLH SWPALH
%token LDADD LDADDA LDADDL LDADDAL LDADDH LDADDAH LDADDLH LDADDALH
%token LDADDB LDADDAB LDADDLB LDADDALB
%token STADD STADDL STADDH STADDLH STADDB STADDLB
%token LDEOR LDEORA LDEORL LDEORAL LDEORH LDEORAH LDEORLH LDEORALH
%token LDEORB LDEORAB LDEORLB LDEORALB
%token STEOR STEORL STEORH STEORLH STEORB STEORLB
%token LDSET LDSETA LDSETL LDSETAL LDSETH LDSETAH LDSETLH LDSETALH
%token LDSETB LDSETAB LDSETLB LDSETALB
%token STSET STSETL STSETH STSETLH STSETB STSETLB
%token LDCLR LDCLRA LDCLRL LDCLRAL LDCLRH LDCLRAH LDCLRLH LDCLRALH
%token LDCLRB LDCLRAB LDCLRLB LDCLRALB
%token STCLR STCLRL STCLRH STCLRLH STCLRB STCLRLB
%token LDSMAX LDSMAXA LDSMAXL LDSMAXAL LDSMAXH LDSMAXAH LDSMAXLH LDSMAXALH
%token LDSMAXB LDSMAXAB LDSMAXLB LDSMAXALB
%token STSMAX STSMAXL STSMAXH STSMAXLH STSMAXB STSMAXLB
%token LDSMIN LDSMINA LDSMINL LDSMINAL LDSMINH LDSMINAH LDSMINLH LDSMINALH
%token LDSMINB LDSMINAB LDSMINLB LDSMINALB
%token STSMIN STSMINL STSMINH STSMINLH STSMINB STSMINLB
%token LDUMAX LDUMAXA LDUMAXL LDUMAXAL LDUMAXH LDUMAXAH LDUMAXLH LDUMAXALH
%token LDUMAXB LDUMAXAB LDUMAXLB LDUMAXALB
%token STUMAX STUMAXL STUMAXH STUMAXLH STUMAXB STUMAXLB
%token LDUMIN LDUMINA LDUMINL LDUMINAL LDUMINH LDUMINAH LDUMINLH LDUMINALH
%token LDUMINB LDUMINAB LDUMINLB LDUMINALB
%token STUMIN STUMINL STUMINH STUMINLH STUMINB STUMINLB
%token IC DC IVAU
%token <AArch64Base.IC.op> IC_OP
%token <AArch64Base.DC.op> DC_OP
%token <AArch64Base.sysreg> SYSREG
%token MRS TST RBIT
%token STG LDG

%type <int list * (AArch64Base.parsedPseudo) list list> main
%type <AArch64Base.parsedPseudo list> instr_option_seq
%start  main instr_option_seq

%nonassoc SEMI
%%
main:
| semi_opt proc_list iol_list EOF { $2,$3 }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI
    {[$1]}

| PROC PIPE proc_list  { $1::$3 }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list
      {$1::$3}

instr_option_seq :
  | instr_option
      {[$1]}
  | instr_option SEMI instr_option_seq
      {$1::$3}

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| CODEVAR    { Symbolic $1 }
| instr      { Instruction $1}

reg:
| SYMB_XREG { V64,Symbolic_reg $1 }
| ARCH_XREG { V64,$1 }
| SYMB_WREG { V32,Symbolic_reg $1 }
| ARCH_WREG { V32,$1 }

xreg:
| SYMB_XREG { Symbolic_reg $1 }
| ARCH_XREG { $1 }

wreg:
| SYMB_WREG { Symbolic_reg $1 }
| ARCH_WREG { $1 }

k:
| NUM  { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

kr:
| k { K $1 }
| xreg { RV (V64,$1) }
| wreg COMMA SXTW { RV (V32,$1) }

kr0:
| { K (MetaConst.zero) }
| COMMA kr { $2 }

kwr:
| k { K $1 }
| wreg { RV (V32,$1) }

zeroopt:
| { () }
| COMMA NUM { if $2 <> 0 then raise Parsing.Parse_error }

ldp_instr:
| LDP
  { (fun v r1 r2 r3 kr -> I_LDP (TT,v,r1,r2,r3,kr)) }
| LDNP
  { (fun v r1 r2 r3 kr -> I_LDP (NT,v,r1,r2,r3,kr)) }

stp_instr:
| STP
  { (fun v r1 r2 r3 kr -> I_STP (TT,v,r1,r2,r3,kr)) }
| STNP
  { (fun v r1 r2 r3 kr -> I_STP (NT,v,r1,r2,r3,kr)) }

cond:
| EQ { EQ }
| NE { NE }

instr:
| NOP { I_NOP }
/* Branch */
| B NAME { I_B $2 }
| BR xreg { I_BR $2 }
| BL NAME { I_BL $2 }
| BLR xreg { I_BLR $2 }
| RET  { I_RET None }
| RET xreg { I_RET (Some $2) }
| BEQ NAME { I_BC (EQ,$2) }
| BNE NAME { I_BC (NE,$2) }
| CBZ reg COMMA NAME   { let v,r = $2 in I_CBZ (v,r,$4) }
| CBNZ reg COMMA NAME  { let v,r = $2 in I_CBNZ (v,r,$4) }
/* Memory */
| LDR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in I_LDR (v,r,$5,$6) }
| ldp_instr wreg COMMA wreg COMMA LBRK xreg kr0 RBRK
  { $1 V32 $2 $4 $7 $8 }
| ldp_instr xreg COMMA xreg COMMA LBRK xreg kr0 RBRK
  { $1 V64 $2 $4 $7 $8 }
| stp_instr wreg COMMA wreg COMMA LBRK xreg kr0 RBRK
  { $1 V32 $2 $4 $7 $8 }
| stp_instr xreg COMMA xreg COMMA LBRK xreg kr0 RBRK
  { $1 V64 $2 $4 $7 $8 }
| LDRB wreg COMMA LBRK xreg kr0 RBRK
  { I_LDRBH (B,$2,$5,$6) }
| LDRH wreg COMMA LBRK xreg kr0 RBRK
  { I_LDRBH (H,$2,$5,$6) }
| LDAR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AA,r,$5) }
| LDARB wreg COMMA LBRK xreg RBRK
  { I_LDARBH (B,AA,$2,$5) }
| LDARH wreg COMMA LBRK xreg RBRK
  { I_LDARBH (H,AA,$2,$5) }
| LDXR reg COMMA LBRK xreg RBRK
    { let v,r = $2 in I_LDAR (v,XX,r,$5) }
| LDXRB wreg COMMA LBRK xreg RBRK
  { I_LDARBH (B,XX,$2,$5) }
| LDXRH wreg COMMA LBRK xreg RBRK
  { I_LDARBH (H,XX,$2,$5) }
| LDAXR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AX,r,$5) }
| LDAXRB wreg COMMA LBRK xreg RBRK
  { I_LDARBH (B,AX,$2,$5) }
| LDAXRH wreg COMMA LBRK xreg RBRK
  { I_LDARBH (H,AX,$2,$5) }
| LDAPR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AQ,r,$5) }
| LDAPRB wreg COMMA LBRK xreg RBRK
  { I_LDARBH (B,AQ,$2,$5) }
| LDAPRH wreg COMMA LBRK xreg RBRK
  { I_LDARBH (H,AQ,$2,$5) }
| STR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in I_STR (v,r,$5,$6) }
| STRB wreg COMMA LBRK xreg kr0 RBRK
  { I_STRBH (B,$2,$5,$6) }
| STRH wreg COMMA LBRK xreg kr0 RBRK
  { I_STRBH (H,$2,$5,$6) }
| STLR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_STLR (v,r,$5) }
| STLRB wreg COMMA LBRK xreg RBRK
  { I_STLRBH (B,$2,$5) }
| STLRH wreg COMMA LBRK xreg RBRK
  { I_STLRBH (H,$2,$5) }
| STXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in I_STXR (v,YY,$2,r,$7) }
| STXRB wreg COMMA wreg COMMA LBRK xreg RBRK
  { I_STXRBH (B,YY,$2,$4,$7) }
| STXRH wreg COMMA wreg COMMA LBRK xreg RBRK
  { I_STXRBH (H,YY,$2,$4,$7) }
| STLXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in I_STXR (v,LY,$2,r,$7) }
| STLXRB wreg COMMA wreg COMMA LBRK xreg RBRK
  { I_STXRBH (B,LY,$2,$4,$7) }
| STLXRH wreg COMMA wreg COMMA LBRK xreg RBRK
  { I_STXRBH (H,LY,$2,$4,$7) }
    /* Compare and swap */
| CAS wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V32,RMW_P,$2,$4,$7) }
| CAS xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V64,RMW_P,$2,$4,$7) }
| CASA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V32,RMW_A,$2,$4,$7) }
| CASA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V64,RMW_A,$2,$4,$7) }
| CASL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V32,RMW_L,$2,$4,$7) }
| CASL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V64,RMW_L,$2,$4,$7) }
| CASAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V32,RMW_AL,$2,$4,$7) }
| CASAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_CAS (V64,RMW_AL,$2,$4,$7) }
| CASB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (B,RMW_P,$2,$4,$7) }
| CASAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (B,RMW_A,$2,$4,$7) }
| CASLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (B,RMW_L,$2,$4,$7) }
| CASALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (B,RMW_AL,$2,$4,$7) }
| CASH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (H,RMW_P,$2,$4,$7) }
| CASAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (H,RMW_A,$2,$4,$7) }
| CASLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (H,RMW_L,$2,$4,$7) }
| CASALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_CASBH (H,RMW_AL,$2,$4,$7) }
/* Swap */
| SWP wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V32,RMW_P,$2,$4,$7) }
| SWP xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V64,RMW_P,$2,$4,$7) }
| SWPA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V32,RMW_A,$2,$4,$7) }
| SWPA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V64,RMW_A,$2,$4,$7) }
| SWPL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V32,RMW_L,$2,$4,$7) }
| SWPL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V64,RMW_L,$2,$4,$7) }
| SWPAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V32,RMW_AL,$2,$4,$7) }
| SWPAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWP (V64,RMW_AL,$2,$4,$7) }
| SWPB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (B,RMW_P,$2,$4,$7) }
| SWPAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (B,RMW_A,$2,$4,$7) }
| SWPLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (B,RMW_L,$2,$4,$7) }
| SWPALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (B,RMW_AL,$2,$4,$7) }
| SWPH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (H,RMW_P,$2,$4,$7) }
| SWPAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (H,RMW_A,$2,$4,$7) }
| SWPLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (H,RMW_L,$2,$4,$7) }
| SWPALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { I_SWPBH (H,RMW_AL,$2,$4,$7) }
/* Memory Tagging */
| STG xreg COMMA LBRK xreg kr0 RBRK
  { I_STG ($2,$5,$6) }
| LDG xreg COMMA LBRK xreg kr0 RBRK
  { I_LDG ($2,$5,$6) }

/* Fetch and ADD */
| LDADD wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V32,RMW_P,$2,$4,$7) }
| LDADD xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V64,RMW_P,$2,$4,$7) }
| LDADDA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V32,RMW_A,$2,$4,$7) }
| LDADDA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V64,RMW_A,$2,$4,$7) }
| LDADDL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V32,RMW_L,$2,$4,$7) }
| LDADDL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V64,RMW_L,$2,$4,$7) }
| LDADDAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V32,RMW_AL,$2,$4,$7) }
| LDADDAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_ADD,V64,RMW_AL,$2,$4,$7) }
| LDADDH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,H,RMW_P,$2,$4,$7) }
| LDADDAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,H,RMW_A,$2,$4,$7) }
| LDADDLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,H,RMW_L,$2,$4,$7) }
| LDADDALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,H,RMW_AL,$2,$4,$7) }
| LDADDB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,B,RMW_P,$2,$4,$7) }
| LDADDAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,B,RMW_A,$2,$4,$7) }
| LDADDLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,B,RMW_L,$2,$4,$7) }
| LDADDALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_ADD,B,RMW_AL,$2,$4,$7) }
| STADD wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_ADD,V32,W_P,$2,$5) }
| STADD xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_ADD,V64,W_P,$2,$5) }
| STADDL wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_ADD,V32,W_L,$2,$5) }
| STADDL xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_ADD,V64,W_L,$2,$5) }
| STADDH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_ADD,H,W_P,$2,$5) }
| STADDLH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_ADD,H,W_L,$2,$5) }
| STADDB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_ADD,B,W_P,$2,$5) }
| STADDLB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_ADD,B,W_L,$2,$5) }
/* Fetch and Xor */
| LDEOR wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V32,RMW_P,$2,$4,$7) }
| LDEOR xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V64,RMW_P,$2,$4,$7) }
| LDEORA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V32,RMW_A,$2,$4,$7) }
| LDEORA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V64,RMW_A,$2,$4,$7) }
| LDEORL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V32,RMW_L,$2,$4,$7) }
| LDEORL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V64,RMW_L,$2,$4,$7) }
| LDEORAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V32,RMW_AL,$2,$4,$7) }
| LDEORAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_EOR,V64,RMW_AL,$2,$4,$7) }
| LDEORH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,H,RMW_P,$2,$4,$7) }
| LDEORAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,H,RMW_A,$2,$4,$7) }
| LDEORLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,H,RMW_L,$2,$4,$7) }
| LDEORALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,H,RMW_AL,$2,$4,$7) }
| LDEORB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,B,RMW_P,$2,$4,$7) }
| LDEORAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,B,RMW_A,$2,$4,$7) }
| LDEORLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,B,RMW_L,$2,$4,$7) }
| LDEORALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_EOR,B,RMW_AL,$2,$4,$7) }
| STEOR wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_EOR,V32,W_P,$2,$5) }
| STEOR xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_EOR,V64,W_P,$2,$5) }
| STEORL wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_EOR,V32,W_L,$2,$5) }
| STEORL xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_EOR,V64,W_L,$2,$5) }
| STEORH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_EOR,H,W_P,$2,$5) }
| STEORLH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_EOR,H,W_L,$2,$5) }
| STEORB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_EOR,B,W_P,$2,$5) }
| STEORLB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_EOR,B,W_L,$2,$5) }
/* Fetch and Or */
| LDSET wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V32,RMW_P,$2,$4,$7) }
| LDSET xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V64,RMW_P,$2,$4,$7) }
| LDSETA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V32,RMW_A,$2,$4,$7) }
| LDSETA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V64,RMW_A,$2,$4,$7) }
| LDSETL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V32,RMW_L,$2,$4,$7) }
| LDSETL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V64,RMW_L,$2,$4,$7) }
| LDSETAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V32,RMW_AL,$2,$4,$7) }
| LDSETAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SET,V64,RMW_AL,$2,$4,$7) }
| LDSETH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,H,RMW_P,$2,$4,$7) }
| LDSETAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,H,RMW_A,$2,$4,$7) }
| LDSETLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,H,RMW_L,$2,$4,$7) }
| LDSETALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,H,RMW_AL,$2,$4,$7) }
| LDSETB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,B,RMW_P,$2,$4,$7) }
| LDSETAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,B,RMW_A,$2,$4,$7) }
| LDSETLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,B,RMW_L,$2,$4,$7) }
| LDSETALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SET,B,RMW_AL,$2,$4,$7) }
| STSET wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SET,V32,W_P,$2,$5) }
| STSET xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SET,V64,W_P,$2,$5) }
| STSETL wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SET,V32,W_L,$2,$5) }
| STSETL xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SET,V64,W_L,$2,$5) }
| STSETH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SET,H,W_P,$2,$5) }
| STSETLH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SET,H,W_L,$2,$5) }
| STSETB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SET,B,W_P,$2,$5) }
| STSETLB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SET,B,W_L,$2,$5) }
/* Fetch and AndNot2 */
| LDCLR wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V32,RMW_P,$2,$4,$7) }
| LDCLR xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V64,RMW_P,$2,$4,$7) }
| LDCLRA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V32,RMW_A,$2,$4,$7) }
| LDCLRA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V64,RMW_A,$2,$4,$7) }
| LDCLRL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V32,RMW_L,$2,$4,$7) }
| LDCLRL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V64,RMW_L,$2,$4,$7) }
| LDCLRAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V32,RMW_AL,$2,$4,$7) }
| LDCLRAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_CLR,V64,RMW_AL,$2,$4,$7) }
| LDCLRH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,H,RMW_P,$2,$4,$7) }
| LDCLRAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,H,RMW_A,$2,$4,$7) }
| LDCLRLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,H,RMW_L,$2,$4,$7) }
| LDCLRALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,H,RMW_AL,$2,$4,$7) }
| LDCLRB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,B,RMW_P,$2,$4,$7) }
| LDCLRAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,B,RMW_A,$2,$4,$7) }
| LDCLRLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,B,RMW_L,$2,$4,$7) }
| LDCLRALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_CLR,B,RMW_AL,$2,$4,$7) }
| STCLR wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_CLR,V32,W_P,$2,$5) }
| STCLR xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_CLR,V64,W_P,$2,$5) }
| STCLRL wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_CLR,V32,W_L,$2,$5) }
| STCLRL xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_CLR,V64,W_L,$2,$5) }
| STCLRH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_CLR,H,W_P,$2,$5) }
| STCLRLH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_CLR,H,W_L,$2,$5) }
| STCLRB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_CLR,B,W_P,$2,$5) }
| STCLRLB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_CLR,B,W_L,$2,$5) }
/* Fetch and Max, Signed */
| LDSMAX wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V32,RMW_P,$2,$4,$7) }
| LDSMAX xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V64,RMW_P,$2,$4,$7) }
| LDSMAXA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V32,RMW_A,$2,$4,$7) }
| LDSMAXA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V64,RMW_A,$2,$4,$7) }
| LDSMAXL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V32,RMW_L,$2,$4,$7) }
| LDSMAXL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V64,RMW_L,$2,$4,$7) }
| LDSMAXAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V32,RMW_AL,$2,$4,$7) }
| LDSMAXAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMAX,V64,RMW_AL,$2,$4,$7) }
| LDSMAXH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,H,RMW_P,$2,$4,$7) }
| LDSMAXAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,H,RMW_A,$2,$4,$7) }
| LDSMAXLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,H,RMW_L,$2,$4,$7) }
| LDSMAXALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,H,RMW_AL,$2,$4,$7) }
| LDSMAXB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,B,RMW_P,$2,$4,$7) }
| LDSMAXAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,B,RMW_A,$2,$4,$7) }
| LDSMAXLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,B,RMW_L,$2,$4,$7) }
| LDSMAXALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMAX,B,RMW_AL,$2,$4,$7) }
| STSMAX wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMAX,V32,W_P,$2,$5) }
| STSMAX xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMAX,V64,W_P,$2,$5) }
| STSMAXL wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMAX,V32,W_L,$2,$5) }
| STSMAXL xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMAX,V64,W_L,$2,$5) }
| STSMAXH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMAX,H,W_P,$2,$5) }
| STSMAXLH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMAX,H,W_L,$2,$5) }
| STSMAXB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMAX,B,W_P,$2,$5) }
| STSMAXLB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMAX,B,W_L,$2,$5) }
/* Fetch and Min, Signed */
| LDSMIN wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V32,RMW_P,$2,$4,$7) }
| LDSMIN xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V64,RMW_P,$2,$4,$7) }
| LDSMINA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V32,RMW_A,$2,$4,$7) }
| LDSMINA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V64,RMW_A,$2,$4,$7) }
| LDSMINL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V32,RMW_L,$2,$4,$7) }
| LDSMINL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V64,RMW_L,$2,$4,$7) }
| LDSMINAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V32,RMW_AL,$2,$4,$7) }
| LDSMINAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOP (A_SMIN,V64,RMW_AL,$2,$4,$7) }
| LDSMINH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,H,RMW_P,$2,$4,$7) }
| LDSMINAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,H,RMW_A,$2,$4,$7) }
| LDSMINLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,H,RMW_L,$2,$4,$7) }
| LDSMINALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,H,RMW_AL,$2,$4,$7) }
| LDSMINB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,B,RMW_P,$2,$4,$7) }
| LDSMINAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,B,RMW_A,$2,$4,$7) }
| LDSMINLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,B,RMW_L,$2,$4,$7) }
| LDSMINALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { I_LDOPBH (A_SMIN,B,RMW_AL,$2,$4,$7) }
| STSMIN wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMIN,V32,W_P,$2,$5) }
| STSMIN xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMIN,V64,W_P,$2,$5) }
| STSMINL wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMIN,V32,W_L,$2,$5) }
| STSMINL xreg COMMA LBRK xreg zeroopt RBRK
   { I_STOP (A_SMIN,V64,W_L,$2,$5) }
| STSMINH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMIN,H,W_P,$2,$5) }
| STSMINLH wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMIN,H,W_L,$2,$5) }
| STSMINB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMIN,B,W_P,$2,$5) }
| STSMINLB wreg COMMA LBRK xreg zeroopt RBRK
   { I_STOPBH (A_SMIN,B,W_L,$2,$5) }
/* Operations */
| MOV xreg COMMA kr
  { I_MOV (V64,$2,$4) }
| MOV wreg COMMA kwr
  { I_MOV (V32,$2,$4) }
| ADR xreg COMMA NAME
  { I_ADDR ($2,$4) }
| SXTW xreg COMMA wreg
  { I_SXTW ($2,$4) }
| OP xreg COMMA xreg COMMA kr
  { I_OP3 (V64,$1,$2,$4,$6) }
| OP wreg COMMA wreg COMMA kwr
    { I_OP3 (V32,$1,$2,$4,$6) }
| CMP wreg COMMA kwr
  { I_OP3 (V32,SUBS,ZR,$2,$4) }
| CMP xreg COMMA kr
  { I_OP3 (V64,SUBS,ZR,$2,$4) }
| TST wreg COMMA k
  { I_OP3 (V32,ANDS,ZR,$2,K $4) }
| TST xreg COMMA k
  { I_OP3 (V64,ANDS,ZR,$2,K $4) }
| RBIT wreg COMMA wreg
  { I_RBIT (V32,$2,$4) }
| RBIT xreg COMMA xreg
  { I_RBIT (V64,$2,$4) }
/* Misc */
| CSEL xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Cpy) }
| CSEL wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Cpy) }
| CSINC xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Inc) }
| CSINC wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Inc) }
| CSINV xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Inv) }
| CSINV wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Inv) }
| CSNEG xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Neg) }
| CSNEG wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Neg) }
| CSET wreg COMMA cond
  { I_CSEL (V32,$2,ZR,ZR,inverse_cond $4,Inc) }
| CSET xreg COMMA cond
  { I_CSEL (V64,$2,ZR,ZR,inverse_cond $4,Inc) }
/* Fences */
| DMB fenceopt
  { let d,t = $2 in I_FENCE (DMB (d,t)) }
| DSB fenceopt
  { let d,t = $2 in I_FENCE (DSB (d,t)) }
| ISB
  { I_FENCE ISB }
/* Cache Maintenance */
| IC IC_OP COMMA xreg
  { I_IC ($2,$4) }
| IC IVAU COMMA xreg
  { I_IC (IC.({ funct=I; typ=VA; point=U; domain=NO; }),$4) }
| DC IVAU COMMA xreg
  { I_DC (DC.({ funct=I; typ=VA; point=U; }),$4) }
| DC DC_OP COMMA xreg
  { I_DC ($2,$4) }
/* System register */
| MRS xreg COMMA SYSREG
  { I_MRS ($2,$4) }

fenceopt:
| SY
  { SY,FULL }
| ST
  { SY,ST }
| LD
  { SY,LD }
| OSH
  { OSH,FULL }
| OSHST
  { OSH,ST }
| OSHLD
  { OSH,LD }
| ISH
  { ISH,FULL }
| ISHST
  { ISH,ST }
| ISHLD
  { ISH,LD }
| NSH
  { NSH,FULL }
| NSHST
  { NSH,ST }
| NSHLD
  { NSH,LD}
