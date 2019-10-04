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

module A = AArch64Base
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

%token SEMI COMMA PIPE COLON LBRK RBRK LPAR RPAR SCOPES LEVELS REGIONS
%token SXTW

/* Instructions */
%token NOP
%token B BR BEQ BNE BGE BGT BLE BLT CBZ CBNZ EQ NE GE GT LE LT
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
/*
/*
%token LDUMAX LDUMAXA LDUMAXL LDUMAXAL LDUMAXH LDUMAXAH LDUMAXLH LDUMAXALH
/*
%token LDUMAXB LDUMAXAB LDUMAXLB LDUMAXALB
%token STUMAX STUMAXL STUMAXH STUMAXLH STUMAXB STUMAXLB
%token LDUMIN LDUMINA LDUMINL LDUMINAL LDUMINH LDUMINAH LDUMINLH LDUMINALH
%token LDUMINB LDUMINAB LDUMINLB LDUMINALB
%token STUMIN STUMINL STUMINH STUMINLH STUMINB STUMINLB
*/
%token IC DC IVAU TLBI
%token <AArch64Base.IC.op> IC_OP
%token <AArch64Base.DC.op> DC_OP
%token <AArch64Base.TLBI.op> TLBI_OP
%token <AArch64Base.sysreg> SYSREG
%token MRS TST RBIT
%token STG LDG
%type <(int * string list option) list * (AArch64Base.parsedPseudo) list list * MiscParser.extra_data> main
%type <AArch64Base.parsedPseudo list> instr_option_seq

%start  main
%start instr_option_seq


%%
main:
| semi_opt proc_list iol_list scopes_and_memory_map EOF
   { $2,$3,MiscParser.BellExtra $4 }

semi_opt:
| { () }
| SEMI { () }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list
      {$1::$3}

instr_option_seq:
  | do_instr_option_seq EOF { $1 }

do_instr_option_seq :
  | instr_option
      {[$1]}
  | instr_option SEMI do_instr_option_seq
      {$1::$3}

instr_option :
|            { A.Nop }
| NAME COLON instr_option {A.Label ($1,$3) }
| CODEVAR    { A.Symbolic $1 }
| instr      { A.Instruction $1}

reg:
| SYMB_XREG { A.V64,A.Symbolic_reg $1 }
| ARCH_XREG { A.V64,$1 }
| SYMB_WREG { A.V32,A.Symbolic_reg $1 }
| ARCH_WREG { A.V32,$1 }

xreg:
| SYMB_XREG { A.Symbolic_reg $1 }
| ARCH_XREG { $1 }

wreg:
| SYMB_WREG { A.Symbolic_reg $1 }
| ARCH_WREG { $1 }

k:
| NUM  { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

kr:
| k { A.K $1 }
| xreg { A.RV (A.V64,$1) }
| wreg COMMA SXTW { A.RV (A.V32,$1) }

kr0:
| { A.K (MetaConst.zero) }
| COMMA kr { $2 }

kwr:
| k { A.K $1 }
| wreg { A.RV (A.V32,$1) }

zeroopt:
| { () }
| COMMA NUM { if $2 <> 0 then raise Parsing.Parse_error }

ldp_instr:
| LDP
  { (fun v r1 r2 r3 kr -> A.I_LDP (A.TT,v,r1,r2,r3,kr)) }
| LDNP
  { (fun v r1 r2 r3 kr -> A.I_LDP (A.NT,v,r1,r2,r3,kr)) }

stp_instr:
| STP
  { (fun v r1 r2 r3 kr -> A.I_STP (A.TT,v,r1,r2,r3,kr)) }
| STNP
  { (fun v r1 r2 r3 kr -> A.I_STP (A.NT,v,r1,r2,r3,kr)) }

cond:
| EQ { A.EQ }
| NE { A.NE }
| GE { A.GE }
| GT { A.GT }
| LE { A.LE }
| LT { A.LT }


instr:
| NOP { A.I_NOP }
/* Branch */
| B NAME { A.I_B $2 }
| BR xreg { A.I_BR $2 }
| BL NAME { A.I_BL $2 }
| BLR xreg { A.I_BLR $2 }
| RET  { A.I_RET None }
| RET xreg { A.I_RET (Some $2) }
| BEQ NAME { A.I_BC (A.EQ,$2) }
| BNE NAME { A.I_BC (A.NE,$2) }
| BLE NAME { A.I_BC (A.LE,$2) }
| BLT NAME { A.I_BC (A.LT,$2) }
| BGE NAME { A.I_BC (A.GE,$2) }
| BGT NAME { A.I_BC (A.GT,$2) }
| CBZ reg COMMA NAME   { let v,r = $2 in A.I_CBZ (v,r,$4) }
| CBNZ reg COMMA NAME  { let v,r = $2 in A.I_CBNZ (v,r,$4) }
/* Memory */
| LDR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in A.I_LDR (v,r,$5,$6) }
| ldp_instr wreg COMMA wreg COMMA LBRK xreg kr0 RBRK
  { $1 A.V32 $2 $4 $7 $8 }
| ldp_instr xreg COMMA xreg COMMA LBRK xreg kr0 RBRK
  { $1 A.V64 $2 $4 $7 $8 }
| stp_instr wreg COMMA wreg COMMA LBRK xreg kr0 RBRK
  { $1 A.V32 $2 $4 $7 $8 }
| stp_instr xreg COMMA xreg COMMA LBRK xreg kr0 RBRK
  { $1 A.V64 $2 $4 $7 $8 }
| LDRB wreg COMMA LBRK xreg kr0 RBRK
  { A.I_LDRBH (A.B,$2,$5,$6) }
| LDRH wreg COMMA LBRK xreg kr0 RBRK
  { A.I_LDRBH (A.H,$2,$5,$6) }
| LDAR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in A.I_LDAR (v,A.AA,r,$5) }
| LDARB wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.B,A.AA,$2,$5) }
| LDARH wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.H,A.AA,$2,$5) }
| LDXR reg COMMA LBRK xreg RBRK
    { let v,r = $2 in A.I_LDAR (v,A.XX,r,$5) }
| LDXRB wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.B,A.XX,$2,$5) }
| LDXRH wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.H,A.XX,$2,$5) }
| LDAXR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in A.I_LDAR (v,A.AX,r,$5) }
| LDAXRB wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.B,A.AX,$2,$5) }
| LDAXRH wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.H,A.AX,$2,$5) }
| LDAPR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in A.I_LDAR (v,A.AQ,r,$5) }
| LDAPRB wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.B,A.AQ,$2,$5) }
| LDAPRH wreg COMMA LBRK xreg RBRK
  { A.I_LDARBH (A.H,A.AQ,$2,$5) }
| STR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in A.I_STR (v,r,$5,$6) }
| STRB wreg COMMA LBRK xreg kr0 RBRK
  { A.I_STRBH (A.B,$2,$5,$6) }
| STRH wreg COMMA LBRK xreg kr0 RBRK
  { A.I_STRBH (A.H,$2,$5,$6) }
| STLR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in A.I_STLR (v,r,$5) }
| STLRB wreg COMMA LBRK xreg RBRK
  { A.I_STLRBH (A.B,$2,$5) }
| STLRH wreg COMMA LBRK xreg RBRK
  { A.I_STLRBH (A.H,$2,$5) }
| STXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in A.I_STXR (v,A.YY,$2,r,$7) }
| STXRB wreg COMMA wreg COMMA LBRK xreg RBRK
  { A.I_STXRBH (A.B,A.YY,$2,$4,$7) }
| STXRH wreg COMMA wreg COMMA LBRK xreg RBRK
  { A.I_STXRBH (A.H,A.YY,$2,$4,$7) }
| STLXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in A.I_STXR (v,A.LY,$2,r,$7) }
| STLXRB wreg COMMA wreg COMMA LBRK xreg RBRK
  { A.I_STXRBH (A.B,A.LY,$2,$4,$7) }
| STLXRH wreg COMMA wreg COMMA LBRK xreg RBRK
  { A.I_STXRBH (A.H,A.LY,$2,$4,$7) }
    /* Compare and swap */
| CAS wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_P,$2,$4,$7) }
| CAS xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_P,$2,$4,$7) }
| CASA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_A,$2,$4,$7) }
| CASA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_A,$2,$4,$7) }
| CASL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_L,$2,$4,$7) }
| CASL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_L,$2,$4,$7) }
| CASAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_AL,$2,$4,$7) }
| CASAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_AL,$2,$4,$7) }
| CASB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_P,$2,$4,$7) }
| CASAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_A,$2,$4,$7) }
| CASLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_L,$2,$4,$7) }
| CASALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_AL,$2,$4,$7) }
| CASH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_P,$2,$4,$7) }
| CASAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_A,$2,$4,$7) }
| CASLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_L,$2,$4,$7) }
| CASALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_AL,$2,$4,$7) }
/* Swap */
| SWP wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_P,$2,$4,$7) }
| SWP xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_P,$2,$4,$7) }
| SWPA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_A,$2,$4,$7) }
| SWPA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_A,$2,$4,$7) }
| SWPL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_L,$2,$4,$7) }
| SWPL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_L,$2,$4,$7) }
| SWPAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_AL,$2,$4,$7) }
| SWPAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_AL,$2,$4,$7) }
| SWPB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_P,$2,$4,$7) }
| SWPAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_A,$2,$4,$7) }
| SWPLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_L,$2,$4,$7) }
| SWPALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_AL,$2,$4,$7) }
| SWPH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_P,$2,$4,$7) }
| SWPAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_A,$2,$4,$7) }
| SWPLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_L,$2,$4,$7) }
| SWPALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_AL,$2,$4,$7) }
/* Memory Tagging */
| STG xreg COMMA LBRK xreg kr0 RBRK
  { A.I_STG ($2,$5,$6) }
| LDG xreg COMMA LBRK xreg kr0 RBRK
  { A.I_LDG ($2,$5,$6) }

/* Fetch and ADD */
| LDADD wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_P,$2,$4,$7) }
| LDADD xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_P,$2,$4,$7) }
| LDADDA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_A,$2,$4,$7) }
| LDADDA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_A,$2,$4,$7) }
| LDADDL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_L,$2,$4,$7) }
| LDADDL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_L,$2,$4,$7) }
| LDADDAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_AL,$2,$4,$7) }
| LDADDAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_AL,$2,$4,$7) }
| LDADDH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_P,$2,$4,$7) }
| LDADDAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_A,$2,$4,$7) }
| LDADDLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_L,$2,$4,$7) }
| LDADDALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_AL,$2,$4,$7) }
| LDADDB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_P,$2,$4,$7) }
| LDADDAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_A,$2,$4,$7) }
| LDADDLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_L,$2,$4,$7) }
| LDADDALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_AL,$2,$4,$7) }
| STADD wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V32,A.W_P,$2,$5) }
| STADD xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V64,A.W_P,$2,$5) }
| STADDL wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V32,A.W_L,$2,$5) }
| STADDL xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V64,A.W_L,$2,$5) }
| STADDH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.H,A.W_P,$2,$5) }
| STADDLH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.H,A.W_L,$2,$5) }
| STADDB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.B,A.W_P,$2,$5) }
| STADDLB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.B,A.W_L,$2,$5) }
/* Fetch and Xor */
| LDEOR wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_P,$2,$4,$7) }
| LDEOR xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_P,$2,$4,$7) }
| LDEORA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_A,$2,$4,$7) }
| LDEORA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_A,$2,$4,$7) }
| LDEORL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_L,$2,$4,$7) }
| LDEORL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_L,$2,$4,$7) }
| LDEORAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_AL,$2,$4,$7) }
| LDEORAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_AL,$2,$4,$7) }
| LDEORH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_P,$2,$4,$7) }
| LDEORAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_A,$2,$4,$7) }
| LDEORLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_L,$2,$4,$7) }
| LDEORALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_AL,$2,$4,$7) }
| LDEORB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_P,$2,$4,$7) }
| LDEORAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_A,$2,$4,$7) }
| LDEORLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_L,$2,$4,$7) }
| LDEORALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_AL,$2,$4,$7) }
| STEOR wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V32,A.W_P,$2,$5) }
| STEOR xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V64,A.W_P,$2,$5) }
| STEORL wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V32,A.W_L,$2,$5) }
| STEORL xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V64,A.W_L,$2,$5) }
| STEORH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.H,A.W_P,$2,$5) }
| STEORLH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.H,A.W_L,$2,$5) }
| STEORB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.B,A.W_P,$2,$5) }
| STEORLB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.B,A.W_L,$2,$5) }
/* Fetch and Or */
| LDSET wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_P,$2,$4,$7) }
| LDSET xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_P,$2,$4,$7) }
| LDSETA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_A,$2,$4,$7) }
| LDSETA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_A,$2,$4,$7) }
| LDSETL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_L,$2,$4,$7) }
| LDSETL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_L,$2,$4,$7) }
| LDSETAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_AL,$2,$4,$7) }
| LDSETAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_AL,$2,$4,$7) }
| LDSETH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_P,$2,$4,$7) }
| LDSETAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_A,$2,$4,$7) }
| LDSETLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_L,$2,$4,$7) }
| LDSETALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_AL,$2,$4,$7) }
| LDSETB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_P,$2,$4,$7) }
| LDSETAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_A,$2,$4,$7) }
| LDSETLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_L,$2,$4,$7) }
| LDSETALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_AL,$2,$4,$7) }
| STSET wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V32,A.W_P,$2,$5) }
| STSET xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V64,A.W_P,$2,$5) }
| STSETL wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V32,A.W_L,$2,$5) }
| STSETL xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V64,A.W_L,$2,$5) }
| STSETH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.H,A.W_P,$2,$5) }
| STSETLH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.H,A.W_L,$2,$5) }
| STSETB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.B,A.W_P,$2,$5) }
| STSETLB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.B,A.W_L,$2,$5) }
/* Fetch and AndNot2 */
| LDCLR wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_P,$2,$4,$7) }
| LDCLR xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_P,$2,$4,$7) }
| LDCLRA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_A,$2,$4,$7) }
| LDCLRA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_A,$2,$4,$7) }
| LDCLRL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_L,$2,$4,$7) }
| LDCLRL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_L,$2,$4,$7) }
| LDCLRAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_AL,$2,$4,$7) }
| LDCLRAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_AL,$2,$4,$7) }
| LDCLRH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_P,$2,$4,$7) }
| LDCLRAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_A,$2,$4,$7) }
| LDCLRLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_L,$2,$4,$7) }
| LDCLRALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_AL,$2,$4,$7) }
| LDCLRB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_P,$2,$4,$7) }
| LDCLRAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_A,$2,$4,$7) }
| LDCLRLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_L,$2,$4,$7) }
| LDCLRALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_AL,$2,$4,$7) }
| STCLR wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V32,A.W_P,$2,$5) }
| STCLR xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V64,A.W_P,$2,$5) }
| STCLRL wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V32,A.W_L,$2,$5) }
| STCLRL xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V64,A.W_L,$2,$5) }
| STCLRH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.H,A.W_P,$2,$5) }
| STCLRLH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.H,A.W_L,$2,$5) }
| STCLRB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.B,A.W_P,$2,$5) }
| STCLRLB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.B,A.W_L,$2,$5) }
/* Fetch and Max, Signed */
| LDSMAX wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_P,$2,$4,$7) }
| LDSMAX xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_P,$2,$4,$7) }
| LDSMAXA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_A,$2,$4,$7) }
| LDSMAXA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_A,$2,$4,$7) }
| LDSMAXL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_L,$2,$4,$7) }
| LDSMAXL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_L,$2,$4,$7) }
| LDSMAXAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_AL,$2,$4,$7) }
| LDSMAXAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_AL,$2,$4,$7) }
| LDSMAXH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_P,$2,$4,$7) }
| LDSMAXAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_A,$2,$4,$7) }
| LDSMAXLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_L,$2,$4,$7) }
| LDSMAXALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_AL,$2,$4,$7) }
| LDSMAXB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_P,$2,$4,$7) }
| LDSMAXAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_A,$2,$4,$7) }
| LDSMAXLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_L,$2,$4,$7) }
| LDSMAXALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_AL,$2,$4,$7) }
| STSMAX wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V32,A.W_P,$2,$5) }
| STSMAX xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V64,A.W_P,$2,$5) }
| STSMAXL wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V32,A.W_L,$2,$5) }
| STSMAXL xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V64,A.W_L,$2,$5) }
| STSMAXH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.H,A.W_P,$2,$5) }
| STSMAXLH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.H,A.W_L,$2,$5) }
| STSMAXB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.B,A.W_P,$2,$5) }
| STSMAXLB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.B,A.W_L,$2,$5) }
/* Fetch and Min, Signed */
| LDSMIN wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_P,$2,$4,$7) }
| LDSMIN xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_P,$2,$4,$7) }
| LDSMINA wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_A,$2,$4,$7) }
| LDSMINA xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_A,$2,$4,$7) }
| LDSMINL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_L,$2,$4,$7) }
| LDSMINL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_L,$2,$4,$7) }
| LDSMINAL wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_AL,$2,$4,$7) }
| LDSMINAL xreg COMMA xreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_AL,$2,$4,$7) }
| LDSMINH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_P,$2,$4,$7) }
| LDSMINAH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_A,$2,$4,$7) }
| LDSMINLH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_L,$2,$4,$7) }
| LDSMINALH wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_AL,$2,$4,$7) }
| LDSMINB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_P,$2,$4,$7) }
| LDSMINAB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_A,$2,$4,$7) }
| LDSMINLB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_L,$2,$4,$7) }
| LDSMINALB wreg COMMA wreg COMMA  LBRK xreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_AL,$2,$4,$7) }
| STSMIN wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V32,A.W_P,$2,$5) }
| STSMIN xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V64,A.W_P,$2,$5) }
| STSMINL wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V32,A.W_L,$2,$5) }
| STSMINL xreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V64,A.W_L,$2,$5) }
| STSMINH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.H,A.W_P,$2,$5) }
| STSMINLH wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.H,A.W_L,$2,$5) }
| STSMINB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.B,A.W_P,$2,$5) }
| STSMINLB wreg COMMA LBRK xreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.B,A.W_L,$2,$5) }
/* Operations */
| MOV xreg COMMA kr
  { A.I_MOV (A.V64,$2,$4) }
| MOV wreg COMMA kwr
  { A.I_MOV (A.V32,$2,$4) }
| ADR xreg COMMA NAME
  { A.I_ADDR ($2,$4) }
| SXTW xreg COMMA wreg
  { A.I_SXTW ($2,$4) }
| OP xreg COMMA xreg COMMA kr
  { A.I_OP3 (A.V64,$1,$2,$4,$6) }
| OP wreg COMMA wreg COMMA kwr
    { A.I_OP3 (A.V32,$1,$2,$4,$6) }
| CMP wreg COMMA kwr
  { A.I_OP3 (A.V32,A.SUBS,A.ZR,$2,$4) }
| CMP xreg COMMA kr
  { A.I_OP3 (A.V64,A.SUBS,A.ZR,$2,$4) }
| TST wreg COMMA k
  { A.I_OP3 (A.V32,A.ANDS,A.ZR,$2,A.K $4) }
| TST xreg COMMA k
  { A.I_OP3 (A.V64,A.ANDS,A.ZR,$2,A.K $4) }
| RBIT wreg COMMA wreg
  { A.I_RBIT (A.V32,$2,$4) }
| RBIT xreg COMMA xreg
  { A.I_RBIT (A.V64,$2,$4) }
/* Misc */
| CSEL xreg COMMA  xreg COMMA  xreg COMMA cond
  { A.I_CSEL (A.V64,$2,$4,$6,$8,A.Cpy) }
| CSEL wreg COMMA  wreg COMMA  wreg COMMA cond
  { A.I_CSEL (A.V32,$2,$4,$6,$8,A.Cpy) }
| CSINC xreg COMMA  xreg COMMA  xreg COMMA cond
  { A.I_CSEL (A.V64,$2,$4,$6,$8,A.Inc) }
| CSINC wreg COMMA  wreg COMMA  wreg COMMA cond
  { A.I_CSEL (A.V32,$2,$4,$6,$8,A.Inc) }
| CSINV xreg COMMA  xreg COMMA  xreg COMMA cond
  { A.I_CSEL (A.V64,$2,$4,$6,$8,A.Inv) }
| CSINV wreg COMMA  wreg COMMA  wreg COMMA cond
  { A.I_CSEL (A.V32,$2,$4,$6,$8,A.Inv) }
| CSNEG xreg COMMA  xreg COMMA  xreg COMMA cond
  { A.I_CSEL (A.V64,$2,$4,$6,$8,A.Neg) }
| CSNEG wreg COMMA  wreg COMMA  wreg COMMA cond
  { A.I_CSEL (A.V32,$2,$4,$6,$8,A.Neg) }
| CSET wreg COMMA cond
  { A.I_CSEL (A.V32,$2,A.ZR,A.ZR,A.inverse_cond $4,A.Inc) }
| CSET xreg COMMA cond
  { A.I_CSEL (A.V64,$2,A.ZR,A.ZR,A.inverse_cond $4,A.Inc) }
/* Fences */
| DMB fenceopt
  { let d,t = $2 in A.I_FENCE (A.DMB (d,t)) }
| DSB fenceopt
  { let d,t = $2 in A.I_FENCE (A.DSB (d,t)) }
| ISB
  { A.I_FENCE A.ISB }
/* Cache Maintenance */
| IC IC_OP COMMA xreg
  { A.I_IC ($2,$4) }
| IC IVAU COMMA xreg
  { A.I_IC (A.IC.({ funct=I; typ=VA; point=U; domain=NO; }),$4) }
| DC IVAU COMMA xreg
  { A.I_DC (A.DC.({ funct=I; typ=VA; point=U; }),$4) }
| DC DC_OP COMMA xreg
  { A.I_DC ($2,$4) }
| TLBI TLBI_OP
  { A.I_TLBI ($2, A.ZR) }
| TLBI TLBI_OP COMMA xreg
  { A.I_TLBI ($2, $4) }

/* System register */
| MRS xreg COMMA SYSREG
  { A.I_MRS ($2,$4) }

fenceopt:
| SY
  { A.SY,A.FULL }
| ST
  { A.SY,A.ST }
| LD
  { A.SY,A.LD }
| OSH
  { A.OSH,A.FULL }
| OSHST
  { A.OSH,A.ST }
| OSHLD
  { A.OSH,A.LD }
| ISH
  { A.ISH,A.FULL }
| ISHST
  { A.ISH,A.ST }
| ISHLD
  { A.ISH,A.LD }
| NSH
  { A.NSH,A.FULL }
| NSHST
  { A.NSH,A.ST }
| NSHLD
  { A.NSH,A.LD}
