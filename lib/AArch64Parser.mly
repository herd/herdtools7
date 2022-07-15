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

(* No constant third argument for those *)
let check_op3 op kr =
  match op,kr with
  |(A.BIC|A.BICS),A.K _ -> raise Parsing.Parse_error
  | _ -> ()
%}

%token EOF
%token <AArch64Base.reg> ARCH_CREG
%token <string> SYMB_CREG
%token <AArch64Base.reg> ARCH_XREG
%token <string> SYMB_XREG
%token <AArch64Base.reg> ARCH_WREG
%token <string> SYMB_WREG
%token <AArch64Base.reg> ARCH_VREG
%token <AArch64Base.reg> ARCH_BREG
%token <AArch64Base.reg> ARCH_HREG
%token <AArch64Base.reg> ARCH_SREG
%token <AArch64Base.reg> ARCH_DREG
%token <AArch64Base.reg> ARCH_QREG
%token <int> INDEX
%token <int> NUM
%token <string> NAME
%token <string> META
%token <string> CODEVAR
%token <int> PROC

%token SEMI COMMA PIPE COLON LCRL RCRL LBRK RBRK LPAR RPAR SCOPES LEVELS REGIONS
%token SXTW

/* Inline Barrel Shift Operands */
%token LSL LSR ASR MSL UXTW

/* Instructions */
%token NOP HINT HLT
%token B BR BEQ BNE BGE BGT BLE BLT CBZ CBNZ EQ NE GE GT LE LT TBZ TBNZ
%token BL BLR RET ERET
%token LDR LDP LDNP LDPSW STP STNP LDRB LDRH LDUR STR STRB STRH STLR STLRB STLRH
%token LDRSB LDRSH
%token LD1 LD1R LD2 LD2R LD3 LD3R LD4 LD4R ST1 ST2 ST3 ST4 STUR /* Neon load/store */
%token CMP MOV MOVZ MOVK MOVI ADR
%token  LDAR LDARB LDARH LDAPR LDAPRB LDAPRH  LDXR LDXRB LDXRH LDAXR LDAXRB LDAXRH LDXP LDAXP
%token STXR STXRB STXRH STLXR STLXRB STLXRH STXP STLXP
%token <AArch64Base.op> OP
%token <AArch64Base.sc> SC
%token <AArch64Base.gc> GC
%token ADD SUB SUBS
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
%token MRS MSR TST RBIT
%token STG STZG LDG
%token ALIGND ALIGNU BUILD CHKEQ CHKSLD CHKTGD CLRTAG CPY CPYTYPE CPYVALUE CSEAL
%token LDCT SEAL STCT UNSEAL
%type <MiscParser.proc list * (AArch64Base.parsedPseudo) list list * MiscParser.extra_data> main
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
| SYMB_CREG { A.V128,A.Symbolic_reg $1 }
| ARCH_CREG { A.V128,$1 }
| SYMB_XREG { A.V64,A.Symbolic_reg $1 }
| ARCH_XREG { A.V64,$1 }
| SYMB_WREG { A.V32,A.Symbolic_reg $1 }
| ARCH_WREG { A.V32,$1 }

creg:
| SYMB_CREG { A.Symbolic_reg $1 }
| ARCH_CREG { $1 }

xreg:
| SYMB_XREG { A.Symbolic_reg $1 }
| ARCH_XREG { $1 }

cxreg:
| SYMB_CREG { A.Symbolic_reg $1 }
| SYMB_XREG { A.Symbolic_reg $1 }
| ARCH_XREG { $1 }
| ARCH_CREG { $1 }


wreg:
| SYMB_WREG { A.Symbolic_reg $1 }
| ARCH_WREG { $1 }

xwr:
| xreg { A.V64,$1 }
| wreg { A.V32,$1 }

vreg:
| ARCH_VREG { $1 }

vregs:
| vregs1 { [$1] }
| vregs2 { $1 }
| vregs3 { $1 }
| vregs4 { $1 }

vregs1:
| LCRL vreg RCRL { $2 }

vregs2:
| LCRL vreg COMMA vreg RCRL { [$2;$4] }

vregs3:
| LCRL vreg COMMA vreg COMMA vreg RCRL { [$2;$4;$6] }

vregs4:
| LCRL vreg COMMA vreg COMMA vreg COMMA vreg RCRL { [$2;$4;$6;$8] }

scalar_regs:
| breg { A.VSIMD8,$1 }
| hreg { A.VSIMD16,$1 }
| sreg { A.VSIMD32,$1 }
| dreg { A.VSIMD64,$1 }
| qreg { A.VSIMD128,$1 }

breg:
| ARCH_BREG { $1 }

hreg:
| ARCH_HREG { $1 }

sreg:
| ARCH_SREG { $1 }

dreg:
| ARCH_DREG { $1 }

qreg:
| ARCH_QREG { $1 }

bhsdregs:
| breg { A.VSIMD8,$1 }
| hreg { A.VSIMD16,$1 }
| sreg { A.VSIMD32,$1 }
| dreg { A.VSIMD64,$1 }

k:
| NUM  { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

k0:
| { None }
| COMMA k { Some $2}

kr:
| k { A.K $1 }
| xreg { A.RV (A.V64,$1) }
| wreg  { A.RV (A.V32,$1) }

kr_shift_address:
| k                { A.K $1, A.S_NOEXT }
| xreg             { (A.RV (A.V64,$1)),  A.S_NOEXT }
| wreg COMMA shift { (A.RV (A.V32, $1)), $3 }
| xreg COMMA shift { (A.RV (A.V64, $1)), $3 }

kr_shift:
| kr_shift_address { $1 }
| wreg { (A.RV (A.V32,$1)),  A.S_NOEXT }

(* For address argument only *)
kr0:
| { A.K (MetaConst.zero), A.S_NOEXT }
| COMMA kr_shift_address { $2 }

kr0_no_shift:
| { A.K (MetaConst.zero) }
| COMMA k { A.K $2 }
| COMMA xreg { A.RV (A.V64,$2) }
| COMMA wreg { A.RV (A.V32,$2) }
| COMMA wreg COMMA SXTW { A.RV (A.V32,$2) }

kx0_no_shift:
| { A.K (MetaConst.zero) }
| COMMA k { A.K $2 }
| COMMA xreg { A.RV (A.V64,$2) }

k0_no_shift:
| { A.K (MetaConst.zero) }
| COMMA k { A.K $2 }

/* Beware: for w-indexed accesses SXTW is considered always present.
   Far from ideal, one simple to get correct assembly output for
   the litmus tool. */
kwr:
| k { A.K $1 }
| wreg { A.RV (A.V32,$1) }

kxr:
| k { A.K $1 }
| xreg { A.RV (A.V64,$1) }

shift:
| LSL NUM  { A.S_LSL(MetaConst.Int $2)  }
| LSR NUM  { A.S_LSR(MetaConst.Int $2)  }
| ASR NUM  { A.S_ASR(MetaConst.Int $2)  }
| MSL NUM  { A.S_MSL(MetaConst.Int $2)  }
| SXTW { A.S_SXTW }
| UXTW { A.S_UXTW }

zeroopt:
| { () }
| COMMA NUM { if $2 <> 0 then raise Parsing.Parse_error }

ldp_instr:
| LDP
  { (fun v r1 r2 r3 kr -> A.I_LDP (A.TT,v,r1,r2,r3,kr)) }
| LDNP
  { (fun v r1 r2 r3 kr -> A.I_LDP (A.NT,v,r1,r2,r3,kr)) }

ldp_simd_instr:
| LDP
  { ( fun v r1 r2 r3 k0 k0' ->
      match k0' with
      | Some post ->
        if k0 = A.K MetaConst.zero then A.I_LDP_P_SIMD (A.TT,v,r1,r2,r3,post)
        else assert false
      | None -> A.I_LDP_SIMD (A.TT,v,r1,r2,r3,k0)
    )}
| LDNP
  { ( fun v r1 r2 r3 k0 k0' ->
      match k0' with
      | None -> A.I_LDP_SIMD (A.NT,v,r1,r2,r3,k0)
      | Some _ -> assert false
    )}

stp_instr:
| STP
  { (fun v r1 r2 r3 kr -> A.I_STP (A.TT,v,r1,r2,r3,kr)) }
| STNP
  { (fun v r1 r2 r3 kr -> A.I_STP (A.NT,v,r1,r2,r3,kr)) }

stp_simd_instr:
| STP
  { ( fun v r1 r2 r3 k0 k0' ->
      match k0' with
      | Some post ->
        if k0 = A.K MetaConst.zero then A.I_STP_P_SIMD (A.TT,v,r1,r2,r3,post)
        else assert false
      | None -> A.I_STP_SIMD (A.TT,v,r1,r2,r3,k0)
    )}
| STNP
  { ( fun v r1 r2 r3 k0 k0' ->
      match k0' with
      | None -> A.I_STP_SIMD (A.NT,v,r1,r2,r3,k0)
      | Some _ -> assert false
    )}

cond:
| EQ { A.EQ }
| NE { A.NE }
| GE { A.GE }
| GT { A.GT }
| LE { A.LE }
| LT { A.LT }

label_addr:
| NAME      { $1 }

instr:
| NOP { A.I_NOP }
| HINT NUM { A.I_NOP }
| HLT NUM { A.I_NOP }
/* Branch */
| B label_addr { A.I_B $2 }
| BR xreg { A.I_BR $2 }
| BL label_addr { A.I_BL $2 }
| BLR xreg { A.I_BLR $2 }
| RET  { A.I_RET None }
| RET xreg { A.I_RET (Some $2) }
| ERET { A.I_ERET }
| BEQ label_addr { A.I_BC (A.EQ,$2) }
| BNE label_addr { A.I_BC (A.NE,$2) }
| BLE NAME { A.I_BC (A.LE,$2) }
| BLT NAME { A.I_BC (A.LT,$2) }
| BGE NAME { A.I_BC (A.GE,$2) }
| BGT NAME { A.I_BC (A.GT,$2) }
| CBZ reg COMMA label_addr { let v,r = $2 in A.I_CBZ (v,r,$4) }
| CBNZ reg COMMA label_addr { let v,r = $2 in A.I_CBNZ (v,r,$4) }
| TBNZ reg COMMA NUM COMMA label_addr
  { let v,r = $2 in A.I_TBNZ (v,r,MetaConst.Int $4,$6) }
| TBZ reg COMMA NUM COMMA label_addr
  { let v,r = $2 in A.I_TBZ (v,r,MetaConst.Int $4,$6) }
/* Memory */
/* must differentiate between regular and post-indexed load */
| LDR reg COMMA LBRK cxreg kr0 RBRK k0
  { let v,r    = $2 in
    let kr, os = $6 in
    match $8 with
    | Some post when kr = A.K MetaConst.zero ->
      A.I_LDR_P (v,r,$5,post)
    | _ ->
      A.I_LDR (v,r,$5,kr,os) }
| LDUR reg COMMA LBRK cxreg k0 RBRK
  { let v,r = $2 in A.I_LDUR (v,r,$5,$6)}
| ldp_instr wreg COMMA wreg COMMA LBRK cxreg kr0_no_shift RBRK
  { $1 A.V32 $2 $4 $7 $8 }
| ldp_instr xreg COMMA xreg COMMA LBRK cxreg kr0_no_shift RBRK
  { $1 A.V64 $2 $4 $7 $8 }
| LDPSW xreg COMMA xreg COMMA LBRK cxreg kr0_no_shift RBRK
  { A.I_LDPSW ($2,$4,$7,$8) }
| LDXP wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_LDXP (A.V32,A.XP,$2,$4,$7) }
| LDXP xreg COMMA xreg COMMA LBRK cxreg RBRK
  { A.I_LDXP (A.V64,A.XP,$2,$4,$7) }
| LDAXP wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_LDXP (A.V32,A.AXP,$2,$4,$7) }
| LDAXP xreg COMMA xreg COMMA LBRK cxreg RBRK
  { A.I_LDXP (A.V64,A.AXP,$2,$4,$7) }
| stp_instr wreg COMMA wreg COMMA LBRK cxreg kr0_no_shift RBRK
  { $1 A.V32 $2 $4 $7 $8 }
| stp_instr xreg COMMA xreg COMMA LBRK cxreg kr0_no_shift RBRK
  { $1 A.V64 $2 $4 $7 $8 }
| STXP wreg COMMA wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_STXP (A.V32,A.YY,$2,$4,$6,$9) }
| STXP wreg COMMA xreg COMMA xreg COMMA LBRK cxreg RBRK
  { A.I_STXP (A.V64,A.YY,$2,$4,$6,$9) }
| STLXP wreg COMMA wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_STXP (A.V32,A.LY,$2,$4,$6,$9) }
| STLXP wreg COMMA xreg COMMA xreg COMMA LBRK cxreg RBRK
  { A.I_STXP (A.V64,A.LY,$2,$4,$6,$9) }
| LDRB wreg COMMA LBRK cxreg kr0 RBRK
  { let (kr, s) = $6 in A.I_LDRBH (A.B,$2,$5,kr,s) }
| LDRH wreg COMMA LBRK cxreg kr0 RBRK
  { let (kr, s) = $6 in A.I_LDRBH (A.H,$2,$5,kr,s) }
| LDRSB reg COMMA LBRK cxreg RBRK
  { let (v, s) = $2 in A.I_LDRS (v,A.B,s,$5) }
| LDRSH reg COMMA LBRK cxreg RBRK
  { let (v, s) = $2 in A.I_LDRS (v,A.H,s,$5) }
| LDAR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in A.I_LDAR (v,A.AA,r,$5) }
| LDARB wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.B,A.AA,$2,$5) }
| LDARH wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.H,A.AA,$2,$5) }
| LDXR reg COMMA LBRK cxreg RBRK
    { let v,r = $2 in A.I_LDAR (v,A.XX,r,$5) }
| LDXRB wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.B,A.XX,$2,$5) }
| LDXRH wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.H,A.XX,$2,$5) }
| LDAXR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in A.I_LDAR (v,A.AX,r,$5) }
| LDAXRB wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.B,A.AX,$2,$5) }
| LDAXRH wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.H,A.AX,$2,$5) }
| LDAPR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in A.I_LDAR (v,A.AQ,r,$5) }
| LDAPRB wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.B,A.AQ,$2,$5) }
| LDAPRH wreg COMMA LBRK cxreg RBRK
  { A.I_LDARBH (A.H,A.AQ,$2,$5) }
| STR reg COMMA LBRK cxreg kr0 RBRK
  { let (v,r)   = $2 in
    let (kr,os) = $6 in A.I_STR (v,r,$5,kr,os) }
| STRB wreg COMMA LBRK cxreg kr0 RBRK
  { let (kr,os) = $6 in A.I_STRBH (A.B,$2,$5,kr,os) }
| STRH wreg COMMA LBRK cxreg kr0 RBRK
  { let (kr, os) = $6 in A.I_STRBH (A.H,$2,$5,kr,os) }
| STLR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in A.I_STLR (v,r,$5) }
| STLRB wreg COMMA LBRK cxreg RBRK
  { A.I_STLRBH (A.B,$2,$5) }
| STLRH wreg COMMA LBRK cxreg RBRK
  { A.I_STLRBH (A.H,$2,$5) }
| STXR wreg COMMA reg COMMA LBRK cxreg RBRK
  { let v,r = $4 in A.I_STXR (v,A.YY,$2,r,$7) }
| STXRB wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_STXRBH (A.B,A.YY,$2,$4,$7) }
| STXRH wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_STXRBH (A.H,A.YY,$2,$4,$7) }
| STLXR wreg COMMA reg COMMA LBRK cxreg RBRK
  { let v,r = $4 in A.I_STXR (v,A.LY,$2,r,$7) }
| STLXRB wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_STXRBH (A.B,A.LY,$2,$4,$7) }
| STLXRH wreg COMMA wreg COMMA LBRK cxreg RBRK
  { A.I_STXRBH (A.H,A.LY,$2,$4,$7) }
   /* Neon extension Memory */
| LD1 vregs1 INDEX COMMA LBRK xreg RBRK kx0_no_shift
  { A.I_LD1 ($2, $3, $6, $8) }
| LD1 vregs COMMA LBRK xreg RBRK kx0_no_shift
  { A.I_LD1M ($2, $5, $7) }
| LD1R vregs1 COMMA LBRK xreg RBRK kx0_no_shift
  { A.I_LD1R ($2, $5, $7) }
| LD2 vregs2 INDEX COMMA LBRK xreg RBRK kx0_no_shift
  { A.I_LD2 ($2, $3, $6, $8) }
| LD2 vregs2 COMMA LBRK xreg RBRK kx0_no_shift
  { A.I_LD2M ($2, $5, $7) }
| LD2R vregs2 COMMA LBRK xreg RBRK kx0_no_shift
  { A.I_LD2R ($2, $5, $7) }
| LD3 vregs3 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_LD3 ($2, $3, $6, $8) }
| LD3 vregs3 COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_LD3M ($2, $5, $7)}
| LD3R vregs3 COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_LD3R ($2, $5, $7) }
| LD4 vregs4 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_LD4 ($2, $3, $6, $8) }
| LD4 vregs4 COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_LD4M ($2, $5, $7) }
| LD4R vregs4 COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_LD4R ($2, $5, $7) }
| ST1 vregs1 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST1 ($2, $3, $6, $8) }
| ST1 vregs COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST1M ($2, $5, $7) }
| ST2 vregs2 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST2 ($2, $3, $6, $8) }
| ST2 vregs2 COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST2M ($2, $5, $7) }
| ST3 vregs3 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST3 ($2, $3, $6, $8) }
| ST3 vregs3 COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST3M ($2, $5, $7) }
| ST4 vregs4 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST4 ($2, $3, $6, $8) }
| ST4 vregs4 COMMA LBRK xreg RBRK kx0_no_shift
   { A.I_ST4M ($2, $5, $7) }
| ldp_simd_instr sreg COMMA sreg COMMA LBRK xreg k0_no_shift RBRK k0
  { $1 A.VSIMD32 $2 $4 $7 $8 $10 }
| ldp_simd_instr dreg COMMA dreg COMMA LBRK xreg k0_no_shift RBRK k0
  { $1 A.VSIMD64 $2 $4 $7 $8 $10 }
| ldp_simd_instr qreg COMMA qreg COMMA LBRK xreg k0_no_shift RBRK k0
  { $1 A.VSIMD128 $2 $4 $7 $8 $10 }
| stp_simd_instr sreg COMMA sreg COMMA LBRK xreg k0_no_shift RBRK k0
  { $1 A.VSIMD32 $2 $4 $7 $8 $10 }
| stp_simd_instr dreg COMMA dreg COMMA LBRK xreg k0_no_shift RBRK k0
  { $1 A.VSIMD64 $2 $4 $7 $8 $10 }
| stp_simd_instr qreg COMMA qreg COMMA LBRK xreg k0_no_shift RBRK k0
  { $1 A.VSIMD128 $2 $4 $7 $8 $10 }
| LDR scalar_regs COMMA LBRK xreg kr0 RBRK k0
  { let v,r    = $2 in
    let kr, os = $6 in
    match $8 with
    | Some post when kr = A.K MetaConst.zero ->
      A.I_LDR_P_SIMD (v,r,$5,post)
    | _ ->
      A.I_LDR_SIMD (v,r,$5,kr,os) }
| LDUR scalar_regs COMMA LBRK xreg k0 RBRK
  { let v,r = $2 in
    A.I_LDUR_SIMD (v, r, $5, $6) }
| STR scalar_regs COMMA LBRK xreg kr0 RBRK k0
  { let v,r    = $2 in
    let kr, os = $6 in
    match $8 with
    | Some post when kr = A.K MetaConst.zero ->
      A.I_STR_P_SIMD (v,r,$5,post)
    | _ ->
      A.I_STR_SIMD (v,r,$5,kr,os) }
| STUR scalar_regs COMMA LBRK xreg k0 RBRK
  { let v,r = $2 in
    A.I_STUR_SIMD (v, r, $5, $6) }
| MOV vreg INDEX COMMA vreg INDEX
  { A.I_MOV_VE ($2, $3, $5, $6) }
| MOV vreg INDEX COMMA xwr
  { let v,r = $5 in
    A.I_MOV_FG ($2, $3, v, r) }
| MOV xreg COMMA vreg INDEX
  { A.I_MOV_TG (A.V64, $2, $4, $5) }
| MOV wreg COMMA vreg INDEX
  { A.I_MOV_TG (A.V32, $2, $4, $5) }
| MOV vreg COMMA vreg
  { A.I_MOV_V ($2, $4) }
| MOV bhsdregs COMMA vreg INDEX
  { let v,r = $2 in
    A.I_MOV_S (v, r, $4 ,$5) }
| MOVI vreg COMMA k
  { A.I_MOVI_V ($2, $4, A.S_NOEXT) }
| MOVI vreg COMMA k COMMA shift
  { A.I_MOVI_V ($2, $4, $6) }
| MOVI dreg COMMA k
  { A.I_MOVI_S ( A.VSIMD64, $2, $4) }
| OP vreg COMMA vreg COMMA vreg
  { match $1 with
    | A.EOR -> A.I_EOR_SIMD ($2,$4,$6)
    | _ -> assert false}
| ADD vreg COMMA vreg COMMA vreg
  { A.I_ADD_SIMD ($2,$4,$6) }
| ADD dreg COMMA dreg COMMA dreg
  { A.I_ADD_SIMD_S ($2,$4,$6)}
    /* Compare and swap */
| CAS wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_P,$2,$4,$7) }
| CAS xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_P,$2,$4,$7) }
| CAS creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V128,A.RMW_P,$2,$4,$7) }
| CASA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_A,$2,$4,$7) }
| CASA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_A,$2,$4,$7) }
| CASA creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V128,A.RMW_A,$2,$4,$7) }
| CASL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_L,$2,$4,$7) }
| CASL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_L,$2,$4,$7) }
| CASL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V128,A.RMW_L,$2,$4,$7) }
| CASAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V32,A.RMW_AL,$2,$4,$7) }
| CASAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V64,A.RMW_AL,$2,$4,$7) }
| CASAL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CAS (A.V128,A.RMW_AL,$2,$4,$7) }
| CASB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_P,$2,$4,$7) }
| CASAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_A,$2,$4,$7) }
| CASLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_L,$2,$4,$7) }
| CASALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.B,A.RMW_AL,$2,$4,$7) }
| CASH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_P,$2,$4,$7) }
| CASAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_A,$2,$4,$7) }
| CASLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_L,$2,$4,$7) }
| CASALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_CASBH (A.H,A.RMW_AL,$2,$4,$7) }
/* Swap */
| SWP wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_P,$2,$4,$7) }
| SWP xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_P,$2,$4,$7) }
| SWP creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V128,A.RMW_P,$2,$4,$7) }
| SWPA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_A,$2,$4,$7) }
| SWPA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_A,$2,$4,$7) }
| SWPA creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V128,A.RMW_A,$2,$4,$7) }
| SWPL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_L,$2,$4,$7) }
| SWPL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_L,$2,$4,$7) }
| SWPL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V128,A.RMW_L,$2,$4,$7) }
| SWPAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V32,A.RMW_AL,$2,$4,$7) }
| SWPAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V64,A.RMW_AL,$2,$4,$7) }
| SWPAL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWP (A.V128,A.RMW_AL,$2,$4,$7) }
| SWPB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_P,$2,$4,$7) }
| SWPAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_A,$2,$4,$7) }
| SWPLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_L,$2,$4,$7) }
| SWPALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.B,A.RMW_AL,$2,$4,$7) }
| SWPH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_P,$2,$4,$7) }
| SWPAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_A,$2,$4,$7) }
| SWPLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_L,$2,$4,$7) }
| SWPALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { A.I_SWPBH (A.H,A.RMW_AL,$2,$4,$7) }
/* Memory Tagging */
| STG xreg COMMA LBRK xreg kr0_no_shift RBRK
  { A.I_STG ($2,$5,$6) }
| STZG xreg COMMA LBRK xreg kr0_no_shift RBRK
  { A.I_STZG ($2,$5,$6) }
| LDG xreg COMMA LBRK xreg kr0_no_shift RBRK
  { A.I_LDG ($2,$5,$6) }

/* Fetch and ADD */
| LDADD wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_P,$2,$4,$7) }
| LDADD xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_P,$2,$4,$7) }
| LDADDA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_A,$2,$4,$7) }
| LDADDA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_A,$2,$4,$7) }
| LDADDL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_L,$2,$4,$7) }
| LDADDL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_L,$2,$4,$7) }
| LDADDAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V32,A.RMW_AL,$2,$4,$7) }
| LDADDAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_ADD,A.V64,A.RMW_AL,$2,$4,$7) }
| LDADDH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_P,$2,$4,$7) }
| LDADDAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_A,$2,$4,$7) }
| LDADDLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_L,$2,$4,$7) }
| LDADDALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.H,A.RMW_AL,$2,$4,$7) }
| LDADDB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_P,$2,$4,$7) }
| LDADDAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_A,$2,$4,$7) }
| LDADDLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_L,$2,$4,$7) }
| LDADDALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_ADD,A.B,A.RMW_AL,$2,$4,$7) }
| STADD wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V32,A.W_P,$2,$5) }
| STADD xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V64,A.W_P,$2,$5) }
| STADDL wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V32,A.W_L,$2,$5) }
| STADDL xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_ADD,A.V64,A.W_L,$2,$5) }
| STADDH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.H,A.W_P,$2,$5) }
| STADDLH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.H,A.W_L,$2,$5) }
| STADDB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.B,A.W_P,$2,$5) }
| STADDLB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_ADD,A.B,A.W_L,$2,$5) }
/* Fetch and Xor */
| LDEOR wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_P,$2,$4,$7) }
| LDEOR xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_P,$2,$4,$7) }
| LDEORA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_A,$2,$4,$7) }
| LDEORA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_A,$2,$4,$7) }
| LDEORL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_L,$2,$4,$7) }
| LDEORL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_L,$2,$4,$7) }
| LDEORAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V32,A.RMW_AL,$2,$4,$7) }
| LDEORAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_EOR,A.V64,A.RMW_AL,$2,$4,$7) }
| LDEORH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_P,$2,$4,$7) }
| LDEORAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_A,$2,$4,$7) }
| LDEORLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_L,$2,$4,$7) }
| LDEORALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.H,A.RMW_AL,$2,$4,$7) }
| LDEORB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_P,$2,$4,$7) }
| LDEORAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_A,$2,$4,$7) }
| LDEORLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_L,$2,$4,$7) }
| LDEORALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_EOR,A.B,A.RMW_AL,$2,$4,$7) }
| STEOR wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V32,A.W_P,$2,$5) }
| STEOR xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V64,A.W_P,$2,$5) }
| STEORL wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V32,A.W_L,$2,$5) }
| STEORL xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_EOR,A.V64,A.W_L,$2,$5) }
| STEORH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.H,A.W_P,$2,$5) }
| STEORLH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.H,A.W_L,$2,$5) }
| STEORB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.B,A.W_P,$2,$5) }
| STEORLB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_EOR,A.B,A.W_L,$2,$5) }
/* Fetch and Or */
| LDSET wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_P,$2,$4,$7) }
| LDSET xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_P,$2,$4,$7) }
| LDSETA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_A,$2,$4,$7) }
| LDSETA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_A,$2,$4,$7) }
| LDSETL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_L,$2,$4,$7) }
| LDSETL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_L,$2,$4,$7) }
| LDSETAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V32,A.RMW_AL,$2,$4,$7) }
| LDSETAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SET,A.V64,A.RMW_AL,$2,$4,$7) }
| LDSETH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_P,$2,$4,$7) }
| LDSETAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_A,$2,$4,$7) }
| LDSETLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_L,$2,$4,$7) }
| LDSETALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.H,A.RMW_AL,$2,$4,$7) }
| LDSETB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_P,$2,$4,$7) }
| LDSETAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_A,$2,$4,$7) }
| LDSETLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_L,$2,$4,$7) }
| LDSETALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SET,A.B,A.RMW_AL,$2,$4,$7) }
| STSET wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V32,A.W_P,$2,$5) }
| STSET xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V64,A.W_P,$2,$5) }
| STSETL wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V32,A.W_L,$2,$5) }
| STSETL xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SET,A.V64,A.W_L,$2,$5) }
| STSETH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.H,A.W_P,$2,$5) }
| STSETLH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.H,A.W_L,$2,$5) }
| STSETB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.B,A.W_P,$2,$5) }
| STSETLB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SET,A.B,A.W_L,$2,$5) }
/* Fetch and AndNot2 */
| LDCLR wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_P,$2,$4,$7) }
| LDCLR xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_P,$2,$4,$7) }
| LDCLRA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_A,$2,$4,$7) }
| LDCLRA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_A,$2,$4,$7) }
| LDCLRL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_L,$2,$4,$7) }
| LDCLRL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_L,$2,$4,$7) }
| LDCLRAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V32,A.RMW_AL,$2,$4,$7) }
| LDCLRAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_CLR,A.V64,A.RMW_AL,$2,$4,$7) }
| LDCLRH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_P,$2,$4,$7) }
| LDCLRAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_A,$2,$4,$7) }
| LDCLRLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_L,$2,$4,$7) }
| LDCLRALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.H,A.RMW_AL,$2,$4,$7) }
| LDCLRB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_P,$2,$4,$7) }
| LDCLRAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_A,$2,$4,$7) }
| LDCLRLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_L,$2,$4,$7) }
| LDCLRALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_CLR,A.B,A.RMW_AL,$2,$4,$7) }
| STCLR wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V32,A.W_P,$2,$5) }
| STCLR xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V64,A.W_P,$2,$5) }
| STCLRL wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V32,A.W_L,$2,$5) }
| STCLRL xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_CLR,A.V64,A.W_L,$2,$5) }
| STCLRH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.H,A.W_P,$2,$5) }
| STCLRLH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.H,A.W_L,$2,$5) }
| STCLRB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.B,A.W_P,$2,$5) }
| STCLRLB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_CLR,A.B,A.W_L,$2,$5) }
/* Fetch and Max, Signed */
| LDSMAX wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_P,$2,$4,$7) }
| LDSMAX xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_P,$2,$4,$7) }
| LDSMAXA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_A,$2,$4,$7) }
| LDSMAXA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_A,$2,$4,$7) }
| LDSMAXL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_L,$2,$4,$7) }
| LDSMAXL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_L,$2,$4,$7) }
| LDSMAXAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V32,A.RMW_AL,$2,$4,$7) }
| LDSMAXAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMAX,A.V64,A.RMW_AL,$2,$4,$7) }
| LDSMAXH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_P,$2,$4,$7) }
| LDSMAXAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_A,$2,$4,$7) }
| LDSMAXLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_L,$2,$4,$7) }
| LDSMAXALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.H,A.RMW_AL,$2,$4,$7) }
| LDSMAXB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_P,$2,$4,$7) }
| LDSMAXAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_A,$2,$4,$7) }
| LDSMAXLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_L,$2,$4,$7) }
| LDSMAXALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMAX,A.B,A.RMW_AL,$2,$4,$7) }
| STSMAX wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V32,A.W_P,$2,$5) }
| STSMAX xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V64,A.W_P,$2,$5) }
| STSMAXL wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V32,A.W_L,$2,$5) }
| STSMAXL xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMAX,A.V64,A.W_L,$2,$5) }
| STSMAXH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.H,A.W_P,$2,$5) }
| STSMAXLH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.H,A.W_L,$2,$5) }
| STSMAXB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.B,A.W_P,$2,$5) }
| STSMAXLB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMAX,A.B,A.W_L,$2,$5) }
/* Fetch and Min, Signed */
| LDSMIN wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_P,$2,$4,$7) }
| LDSMIN xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_P,$2,$4,$7) }
| LDSMINA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_A,$2,$4,$7) }
| LDSMINA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_A,$2,$4,$7) }
| LDSMINL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_L,$2,$4,$7) }
| LDSMINL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_L,$2,$4,$7) }
| LDSMINAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V32,A.RMW_AL,$2,$4,$7) }
| LDSMINAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOP (A.A_SMIN,A.V64,A.RMW_AL,$2,$4,$7) }
| LDSMINH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_P,$2,$4,$7) }
| LDSMINAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_A,$2,$4,$7) }
| LDSMINLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_L,$2,$4,$7) }
| LDSMINALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.H,A.RMW_AL,$2,$4,$7) }
| LDSMINB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_P,$2,$4,$7) }
| LDSMINAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_A,$2,$4,$7) }
| LDSMINLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_L,$2,$4,$7) }
| LDSMINALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { A.I_LDOPBH (A.A_SMIN,A.B,A.RMW_AL,$2,$4,$7) }
| STSMIN wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V32,A.W_P,$2,$5) }
| STSMIN xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V64,A.W_P,$2,$5) }
| STSMINL wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V32,A.W_L,$2,$5) }
| STSMINL xreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOP (A.A_SMIN,A.V64,A.W_L,$2,$5) }
| STSMINH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.H,A.W_P,$2,$5) }
| STSMINLH wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.H,A.W_L,$2,$5) }
| STSMINB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.B,A.W_P,$2,$5) }
| STSMINLB wreg COMMA LBRK cxreg zeroopt RBRK
   { A.I_STOPBH (A.A_SMIN,A.B,A.W_L,$2,$5) }
/* Operations */
| MOV xreg COMMA kr
  { A.I_MOV (A.V64,$2,$4) }
| MOV wreg COMMA kwr
  { A.I_MOV (A.V32,$2,$4) }
| MOV creg COMMA creg
  { A.I_MOV (A.V128,$2,A.RV (A.V128,$4)) }
| CPY creg COMMA creg
  { A.I_MOV (A.V128,$2,A.RV (A.V128,$4)) }
| MOVZ xreg COMMA k
  { A.I_MOVZ (A.V64,$2, $4, A.S_NOEXT) }
| MOVZ xreg COMMA k COMMA LSL k
  { A.I_MOVZ (A.V64,$2, $4, A.S_LSL $7) }
| MOVZ wreg COMMA k
  { A.I_MOVZ (A.V32,$2,$4, A.S_NOEXT) }
| MOVZ wreg COMMA k COMMA LSL k
  { A.I_MOVZ (A.V32,$2,$4, A.S_LSL $7) }
| MOVK xreg COMMA k
  { A.I_MOVK (A.V64,$2, $4, A.S_NOEXT) }
| MOVK xreg COMMA k COMMA LSL k
  { A.I_MOVK (A.V64,$2, $4, A.S_LSL $7) }
| MOVK wreg COMMA k
  { A.I_MOVK (A.V32,$2,$4, A.S_NOEXT) }
| MOVK wreg COMMA k COMMA LSL k
  { A.I_MOVK (A.V32,$2,$4, A.S_LSL $7) }
| ADR xreg COMMA NAME
  { A.I_ADR ($2,$4) }
| SXTW xreg COMMA wreg
  { A.I_SXTW ($2,$4) }
/* Special handling for ASR/LSL/LSR operation */
| ASR xreg COMMA xreg COMMA kr
  { A.I_OP3 (A.V64, AArch64Base.ASR, $2, $4, $6, A.S_NOEXT) }
| LSL xreg COMMA xreg COMMA kr
  { A.I_OP3 (A.V64, AArch64Base.LSL, $2, $4, $6, A.S_NOEXT) }
| LSR xreg COMMA xreg COMMA kr
  { A.I_OP3 (A.V64, AArch64Base.LSR, $2, $4, $6, A.S_NOEXT) }
| OP xreg COMMA xreg COMMA kxr
  { check_op3 $1 $6 ; A.I_OP3 (A.V64,$1,$2,$4,$6, A.S_NOEXT) }
| OP xreg COMMA xreg COMMA kr COMMA shift
  { check_op3 $1 $6 ; A.I_OP3 (A.V64,$1,$2,$4,$6, $8) }
| OP wreg COMMA wreg COMMA kwr
  { check_op3 $1 $6 ; A.I_OP3 (A.V32,$1,$2,$4,$6, A.S_NOEXT) }
| ADD xreg COMMA xreg COMMA kxr
  { A.I_OP3 (A.V64,A.ADD,$2,$4,$6, A.S_NOEXT) }
| ADD xreg COMMA xreg COMMA kr COMMA shift
  { A.I_OP3 (A.V64,A.ADD,$2,$4,$6, $8) }
| ADD wreg COMMA wreg COMMA kwr
  { A.I_OP3 (A.V32,A.ADD,$2,$4,$6, A.S_NOEXT) }
| ADD wreg COMMA wreg COMMA kwr COMMA shift
  { A.I_OP3 (A.V32,A.ADD,$2,$4,$6, $8) }
| ADD creg COMMA creg COMMA kxr
  { A.I_OP3 (A.V128,A.ADD,$2,$4,$6, A.S_NOEXT) }
| SUB xreg COMMA xreg COMMA kxr
  { A.I_OP3 (A.V64,A.SUB,$2,$4,$6, A.S_NOEXT) }
| SUB xreg COMMA xreg COMMA kr COMMA shift
  { A.I_OP3 (A.V64,A.SUB,$2,$4,$6, $8) }
| SUB wreg COMMA wreg COMMA kwr
    { A.I_OP3 (A.V32,A.SUB,$2,$4,$6, A.S_NOEXT) }
| SUB wreg COMMA wreg COMMA kwr COMMA shift
    { A.I_OP3 (A.V32,A.SUB,$2,$4,$6, $8) }
| SUB creg COMMA creg COMMA k
  { A.I_OP3 (A.V128,A.SUB,$2,$4,A.K $6, A.S_NOEXT) }
| SUBS xreg COMMA xreg COMMA kxr
  { A.I_OP3 (A.V64,A.SUBS,$2,$4,$6, A.S_NOEXT) }
| SUBS xreg COMMA xreg COMMA kr COMMA shift
  { A.I_OP3 (A.V64,A.SUBS,$2,$4,$6, $8) }
| SUBS wreg COMMA wreg COMMA kwr
  { A.I_OP3 (A.V32,A.SUBS,$2,$4,$6, A.S_NOEXT) }
| SUBS wreg COMMA wreg COMMA kwr COMMA shift
  { A.I_OP3 (A.V32,A.SUBS,$2,$4,$6, $8) }
| SUBS xreg COMMA creg COMMA creg
  { A.I_OP3 (A.V128,A.SUBS,$2,$4,A.RV (A.V128,$6), A.S_NOEXT) }
| OP wreg COMMA wreg COMMA kwr COMMA shift
  { check_op3 $1 $6 ; A.I_OP3 (A.V32,$1,$2,$4,$6,$8) }
| CMP wreg COMMA kr_shift
  { let (reg,shift) = $4 in
    A.I_OP3 (A.V32,A.SUBS,A.ZR,$2,reg,shift) }
| CMP xreg COMMA kr_shift
  { let (reg,shift) = $4 in
    A.I_OP3 (A.V64,A.SUBS,A.ZR,$2,reg,shift) }
| TST wreg COMMA k
  { A.I_OP3 (A.V32,A.ANDS,A.ZR,$2,A.K $4, A.S_NOEXT) }
| TST xreg COMMA k
  { A.I_OP3 (A.V64,A.ANDS,A.ZR,$2,A.K $4, A.S_NOEXT) }
| RBIT wreg COMMA wreg
  { A.I_RBIT (A.V32,$2,$4) }
| RBIT xreg COMMA xreg
  { A.I_RBIT (A.V64,$2,$4) }
/* Morello */
| ALIGND creg COMMA creg COMMA k
  { A.I_ALIGND ($2,$4,A.K $6) }
| ALIGNU creg COMMA creg COMMA k
  { A.I_ALIGNU ($2,$4,A.K $6) }
| BUILD creg COMMA creg COMMA creg
  { A.I_BUILD ($2,$4,$6) }
| CHKEQ creg COMMA creg
  { A.I_CHKEQ ($2,$4) }
| CHKSLD creg
  { A.I_CHKSLD ($2) }
| CHKTGD creg
  { A.I_CHKTGD ($2) }
| CLRTAG creg COMMA creg
  { A.I_CLRTAG ($2,$4) }
| CPYTYPE creg COMMA creg COMMA creg
  { A.I_CPYTYPE ($2,$4,$6) }
| CPYVALUE creg COMMA creg COMMA creg
  { A.I_CPYVALUE ($2,$4,$6) }
| CSEAL creg COMMA creg COMMA creg
  { A.I_CSEAL ($2,$4,$6) }
| GC xreg COMMA creg
  { A.I_GC ($1,$2,$4) }
| LDCT xreg COMMA LBRK cxreg RBRK
  { A.I_LDCT ($2,$5) }
| SC creg COMMA creg COMMA xreg
  { A.I_SC ($1,$2,$4,$6) }
| SEAL creg COMMA creg COMMA creg
  { A.I_SEAL ($2,$4,$6) }
| STCT xreg COMMA LBRK cxreg RBRK
  { A.I_STCT ($2,$5) }
| UNSEAL creg COMMA creg COMMA creg
  { A.I_UNSEAL ($2,$4,$6) }
/* Misc */
| CSEL xreg COMMA  xreg COMMA  xreg COMMA cond
  { A.I_CSEL (A.V64,$2,$4,$6,$8,A.Cpy) }
| CSEL wreg COMMA  wreg COMMA  wreg COMMA cond
  { A.I_CSEL (A.V32,$2,$4,$6,$8,A.Cpy) }
| CSEL creg COMMA  creg COMMA  creg COMMA cond
  { A.I_CSEL (A.V128,$2,$4,$6,$8,A.Cpy) }
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
| IC IC_OP
  { A.I_IC ($2,A.ZR) }
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
| MSR SYSREG COMMA xreg
  { A.I_MSR ($2,$4) }

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
