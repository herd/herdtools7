%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open AArch64Base

(* No constant third argument for those *)
let check_op3 op e =
  match op,e with
  |(BIC|BICS),OpExt.Imm _ -> raise Parsing.Parse_error
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

%token SEMI COMMA PIPE COLON DOT BANG LCRL RCRL LBRK RBRK LPAR RPAR SCOPES LEVELS REGIONS

/*  Extension */
%token TOK_SXTB TOK_SXTH TOK_SXTW TOK_SXTX
%token TOK_UXTB TOK_UXTH TOK_UXTW TOK_UXTX

%token SBFM UBFM

/* Inline Barrel Shift Operands */
%token TOK_LSL TOK_LSR TOK_ASR TOK_MSL TOK_ROR

/* Variable shift's */
%token TOK_LSLV TOK_LSRV TOK_ASRV TOK_RORV

/* Instructions */
%token NOP HINT HLT
%token TOK_B BR CBZ CBNZ TBZ TBNZ
%token TOK_EQ TOK_NE TOK_GE TOK_GT TOK_LE TOK_LT
%token TOK_CS TOK_CC TOK_MI TOK_PL TOK_VS TOK_VC TOK_HI TOK_LS TOK_AL
%token BEQ BNE BGE BGT BLE BLT BCS BCC BMI BPL BVS BVC BHI BLS BAL
%token BL BLR RET ERET
%token LDR LDRSW LDP LDNP LDPSW LDIAPP STP STNP STILP
%token LDRB LDRH LDUR STR STRB STRH STLR STLRB STLRH
%token LDRSB LDRSH
%token LD1 LD1R LDAP1 LD2 LD2R LD3 LD3R LD4 LD4R STL1 ST1 ST2 ST3 ST4 STUR /* Neon load/store */
%token ADDV DUP FMOV LDAPUR STLUR
%token CMP MOV MOVZ MOVN MOVK MOVI ADR MVN
%token  LDAR LDARB LDARH LDAPR LDAPRB LDAPRH  LDXR LDXRB LDXRH LDAXR LDAXRB LDAXRH LDXP LDAXP
%token STXR STXRB STXRH STLXR STLXRB STLXRH STXP STLXP
%token <AArch64Base.op> OP
%token <AArch64Base.sc> SC
%token <AArch64Base.gc> GC
%token TOK_ADD TOK_ADDS TOK_SUB TOK_SUBS
%token TOK_NEG TOK_NEGS
%token <AArch64Base.MOPLExt.sop> MOPLZ
%token <AArch64Base.MOPLExt.sop> MOPL
%token CSEL CSINC CSINV CSNEG CSET CSETM CINC
%token TOK_DMB TOK_DSB TOK_ISB
%token TOK_SY TOK_ST TOK_LD
%token TOK_OSH TOK_OSHST TOK_OSHLD
%token TOK_ISH TOK_ISHST TOK_ISHLD
%token TOK_NSH TOK_NSHST TOK_NSHLD
%token CAS CASA CASL CASAL CASB CASAB CASLB CASALB CASH CASAH CASLH CASALH
%token CASP CASPA CASPL CASPAL
%token SWP SWPA SWPL SWPAL SWPB SWPAB SWPLB SWPALB SWPH SWPAH SWPLH SWPALH
%token <AArch64Base.atomic_op * AArch64Base.rmw_type> LDOP
%token <AArch64Base.atomic_op * AArch64Base.w_type> STOP
%token <AArch64Base.bh * AArch64Base.atomic_op * AArch64Base.rmw_type> LDOPBH
%token <AArch64Base.bh * AArch64Base.atomic_op * AArch64Base.w_type> STOPBH
%token UDF
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
%token MRS MSR TST RBIT ABS
%token REV16 REV32 REV REV64
%token EXTR
%token STG STZG STZ2G LDG
%token ALIGND ALIGNU BUILD CHKEQ CHKSLD CHKTGD CLRTAG CPY CPYTYPE CPYVALUE CSEAL
%token LDCT SEAL STCT UNSEAL
%type <MiscParser.proc list * (AArch64Base.parsedPseudo) list list * MiscParser.extra_data> main

%start  main
%type <AArch64Base.parsedPseudo list> instr_option_seq

%start instr_option_seq
%type  <AArch64Base.parsedInstruction> instr

%start one_instr
%type  <AArch64Base.pins> one_instr


%%
main:
| semi_opt proc_list iol_list scopes_and_memory_map EOF
   { $2,$3,[MiscParser.BellExtra $4] }

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
|            { Nop }
| NAME COLON instr_option {Label ($1,$3) }
| CODEVAR    { Symbolic $1 }
| instr      { Instruction $1}

reg:
| SYMB_CREG { V128,Symbolic_reg $1 }
| ARCH_CREG { V128,$1 }
| SYMB_XREG { V64,Symbolic_reg $1 }
| ARCH_XREG { V64,$1 }
| SYMB_WREG { V32,Symbolic_reg $1 }
| ARCH_WREG { V32,$1 }

creg:
| SYMB_CREG { Symbolic_reg $1 }
| ARCH_CREG { $1 }

xreg:
| SYMB_XREG { Symbolic_reg $1 }
| ARCH_XREG { $1 }

cxreg:
| SYMB_CREG { Symbolic_reg $1 }
| SYMB_XREG { Symbolic_reg $1 }
| ARCH_XREG { $1 }
| ARCH_CREG { $1 }


wreg:
| SYMB_WREG { Symbolic_reg $1 }
| ARCH_WREG { $1 }

%inline wxreg:
| xreg { V64,$1 }
| wreg { V32,$1 }

vreg:
| ARCH_VREG { $1 }

vregs:
| vregs1 { $1 }
| vregs2 { $1 }
| vregs3 { $1 }
| vregs4 { $1 }

vregs1:
| LCRL vreg RCRL { [$2] }

vregs2:
| LCRL vreg COMMA vreg RCRL { [$2;$4] }

vregs3:
| LCRL vreg COMMA vreg COMMA vreg RCRL { [$2;$4;$6] }

vregs4:
| LCRL vreg COMMA vreg COMMA vreg COMMA vreg RCRL { [$2;$4;$6;$8] }

scalar_regs:
| breg { VSIMD8,$1 }
| hreg { VSIMD16,$1 }
| sreg { VSIMD32,$1 }
| dreg { VSIMD64,$1 }
| qreg { VSIMD128,$1 }

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
| breg { VSIMD8,$1 }
| hreg { VSIMD16,$1 }
| sreg { VSIMD32,$1 }
| dreg { VSIMD64,$1 }

k:
| NUM  { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

k0_opt:
| { None }
| COMMA k { Some $2}

k0:
| { MetaConst.zero }
| COMMA k=k { k }

kr:
| k { K $1 }
| xreg { RV (V64,$1) }
| wreg  { RV (V32,$1) }

kx0_no_shift:
| { K (MetaConst.zero) }
| COMMA k { K $2 }
| COMMA xreg { RV (V64,$2) }

%inline op_ext_shift:
| TOK_LSL k { OpExt.LSL $2 }
| TOK_LSR k { OpExt.LSR $2}
| TOK_ASR k { OpExt.ASR $2}
| TOK_ROR k { OpExt.ROR $2}

%inline op_ext_shift0:
| { OpExt.LSL MetaConst.zero }
| COMMA op_ext_shift { $2 }

op_ext_imm:
| k
    { OpExt.Imm ($1,MetaConst.zero) }
| k COMMA TOK_LSL k
    { OpExt.Imm ($1,$4) }

%inline op_ext_reg_w:
| wreg op_ext_shift0
    { OpExt.Reg ($1,$2) }

%inline op_ext_w:
| op_ext_imm { $1 }
| op_ext_reg_w { $1 }

%inline op_ext_reg_x:
| xreg op_ext_shift0
    { OpExt.Reg ($1,$2) }

%inline op_ext_x:
| op_ext_imm { $1 }
| op_ext_reg_x { $1 }

op_ext_c:
| op_ext_imm { $1 }
| creg op_ext_shift0
    { OpExt.Reg ($1,$2) }

%inline add_sub_sext:
| TOK_SXTB { Ext.SXTB }
| TOK_SXTH { Ext.SXTH }
| TOK_SXTW { Ext.SXTW }
| TOK_SXTX { Ext.SXTX }
| TOK_UXTB { Ext.UXTB }
| TOK_UXTH { Ext.UXTH }
| TOK_UXTW { Ext.UXTW }
| TOK_UXTX { Ext.UXTX }


%inline add_sub_ext:
| add_sub_sext { $1,None }
| add_sub_sext k { $1,Some $2 }
/*
kr0_no_shift_opt:
| { None }
| COMMA k { Some (K $2) }
| COMMA xreg { Some (RV (V64,$2)) }
| COMMA wreg { Some (RV (V32,$2)) }
| COMMA wreg COMMA TOK_SXTW { Some (RV (V32,$2)) }
*/

/* Beware: for w-indexed accesses SXTW is considered always present.
   Far from ideal, one simple to get correct assembly output for
   the litmus tool. */
kwr:
| k { K $1 }
| wreg { RV (V32,$1) }

mem_sext:
| TOK_UXTW { MemExt.UXTW }
| TOK_LSL { MemExt.LSL }
| TOK_SXTW { MemExt.SXTW }
| TOK_SXTX { MemExt.SXTX }

mem_idx:
| LBRK cxreg RBRK
    { $2,(MetaConst.zero,Idx)  }
| LBRK cxreg COMMA k RBRK
    { $2,($4,Idx) }
| LBRK cxreg RBRK COMMA k
    { $2,($5,PostIdx) }
| LBRK cxreg COMMA k RBRK BANG
    { $2,($4,PreIdx) }

mem_ea:
| mem_idx
    {
      let r,idx = $1 in
      r,MemExt.Imm idx
    }
| LBRK cxreg COMMA wxreg RBRK
    {
      let open MemExt in
      let v,r = $4 in
      $2,Reg (v,r,LSL,MetaConst.zero)
    }
| LBRK cxreg COMMA wxreg COMMA mem_sext RBRK
    {
      let open MemExt in
      let v,r = $4 in
      $2,Reg (v,r,$6,MetaConst.zero)
    }
| LBRK cxreg COMMA wxreg COMMA mem_sext k RBRK
    {
      let open MemExt in
      let v,r = $4 in
      $2,Reg (v,r,$6,$7)
    }

shift:
| TOK_LSL NUM  { S_LSL(MetaConst.Int $2)  }
| TOK_LSR NUM  { S_LSR(MetaConst.Int $2)  }
| TOK_ASR NUM  { S_ASR(MetaConst.Int $2)  }
| TOK_MSL NUM  { S_MSL(MetaConst.Int $2)  }

zeroopt:
| { () }
| COMMA NUM { if $2 <> 0 then raise Parsing.Parse_error }

ldp_instr:
| LDP
  { (fun v r1 r2 (r3,idx) -> I_LDP (Pa,v,r1,r2,r3,idx)) }
| LDNP
  { (fun v r1 r2 (r3,idx) -> I_LDP (PaN,v,r1,r2,r3,idx)) }
| LDIAPP
  {
   (fun v r1 r2 (r3,idx) ->
     match v,idx with
     | (_,(MetaConst.Int 0,Idx))
     | (V32,(MetaConst.Int 8,PostIdx))
     | (V64,(MetaConst.Int 16,PostIdx))
          ->
            I_LDP (PaI,v,r1,r2,r3,idx)
      | _,_ -> raise Parsing.Parse_error)
  }

ldp_simd_instr:
| LDP
  { ( fun v r1 r2 (r3,idx) -> I_LDP_SIMD (TT,v,r1,r2,r3,idx)) }
| LDNP
  { ( fun v r1 r2 (r3,idx) -> I_LDP_SIMD (NT,v,r1,r2,r3,idx)) }

stp_instr:
| STP
  { (fun v r1 r2 (r3,idx) -> I_STP (Pa,v,r1,r2,r3,idx)) }
| STNP
  { (fun v r1 r2 (r3,idx) -> I_STP (PaN,v,r1,r2,r3,idx)) }
| STILP
    {
     (fun v r1 r2 (r3,idx) ->
      match v,idx with
      | (_,(MetaConst.Int 0,Idx))
      | (V32,(MetaConst.Int (-8),PreIdx))
      | (V64,(MetaConst.Int (-16),PreIdx))
          ->
            I_STP (PaI,v,r1,r2,r3,idx)
      | _, _ -> raise Parsing.Parse_error)
    }

stp_simd_instr:
| STP
  { ( fun v r1 r2 (r3,idx) -> I_STP_SIMD (TT,v,r1,r2,r3,idx)) }
| STNP
  { ( fun v r1 r2 (r3,idx) -> I_STP_SIMD (NT,v,r1,r2,r3,idx)) }

cond:
| TOK_EQ { EQ }
| TOK_NE { NE }
| TOK_GE { GE }
| TOK_GT { GT }
| TOK_LE { LE }
| TOK_LT { LT }
| TOK_CS { CS }
| TOK_CC { CC }
| TOK_MI { MI }
| TOK_PL { PL }
| TOK_VS { VS }
| TOK_VC { VC }
| TOK_HI { HI }
| TOK_LS { LS }
| TOK_AL { AL }

%inline tok_instr_shift:
| TOK_ASR { ASR }
| TOK_LSL { LSL }
| TOK_LSR { LSR}
| TOK_ROR { ROR }

%inline tok_instr_shiftv:
| TOK_ASRV { ASR }
| TOK_LSLV { LSL }
| TOK_LSRV { LSR }
| TOK_RORV { ROR }

%inline tok_add_sub:
| TOK_ADD  { ADD }
| TOK_ADDS { ADDS }
| TOK_SUB  { SUB }
| TOK_SUBS { SUBS }

%inline tok_add_sub_ext:
| TOK_ADD  { Ext.ADD }
| TOK_ADDS { Ext.ADDS }
| TOK_SUB  { Ext.SUB }
| TOK_SUBS { Ext.SUBS }

label_addr:
| NAME      { BranchTarget.Lbl $1 }
| DOT NUM   { BranchTarget.Offset $2 }

one_instr:
| i=instr EOF { i }

instr:
| NOP { I_NOP }
| HINT NUM { I_NOP }
| HLT NUM { I_NOP }
/* Branch */
| TOK_B label_addr { I_B $2 }
| BR xreg { I_BR $2 }
| BL label_addr { I_BL $2 }
| BLR xreg { I_BLR $2 }
| RET  { I_RET None }
| RET xreg { I_RET (Some $2) }
| ERET { I_ERET }
| BEQ label_addr { I_BC (EQ, $2) }
| BNE label_addr { I_BC (NE, $2) }
| BLE label_addr { I_BC (LE, $2) }
| BLT label_addr { I_BC (LT, $2) }
| BGE label_addr { I_BC (GE, $2) }
| BGT label_addr { I_BC (GT, $2) }
| BCS label_addr { I_BC (CS, $2) }
| BCC label_addr { I_BC (CC, $2) }
| BMI label_addr { I_BC (MI, $2) }
| BPL label_addr { I_BC (PL, $2) }
| BVS label_addr { I_BC (VS, $2) }
| BVC label_addr { I_BC (VC, $2) }
| BHI label_addr { I_BC (HI, $2) }
| BLS label_addr { I_BC (LS, $2) }
| BAL label_addr { I_BC (AL, $2) }
| CBZ reg COMMA label_addr { let v,r = $2 in I_CBZ (v,r,$4) }
| CBNZ reg COMMA label_addr { let v,r = $2 in I_CBNZ (v,r,$4) }
| TBNZ reg COMMA NUM COMMA label_addr
  { let v,r = $2 in I_TBNZ (v,r,MetaConst.Int $4,$6) }
| TBZ reg COMMA NUM COMMA label_addr
  { let v,r = $2 in I_TBZ (v,r,MetaConst.Int $4,$6) }
/* Memory */
| LDR wxreg COMMA mem_ea
  { let (v,r)   = $2 and (ra,ext) = $4 in I_LDR (v,r,ra,ext) }
| LDRSW xreg COMMA mem_ea
  { let r = $2 and (ra,ext) = $4 in I_LDRSW (r,ra,ext) }
| LDRSB reg COMMA mem_ea
  { let (v, s) = $2 and (ra,ext) = $4 in I_LDRS ((v,B),s,ra,ext) }
| LDRSH reg COMMA mem_ea
  { let (v, s) = $2 and (ra,ext) = $4 in I_LDRS ((v,H),s,ra,ext) }
| LDUR reg COMMA LBRK cxreg k0_opt RBRK
  { let v,r = $2 in I_LDUR (v,r,$5,$6)}

| instr=ldp_instr r1=wreg COMMA r2=wreg COMMA ea=mem_idx
  { instr V32 r1 r2 ea }
| instr=ldp_instr r1=xreg COMMA r2=xreg COMMA ea=mem_idx
  { instr V64 r1 r2 ea }
| instr=stp_instr r1=wreg COMMA r2=wreg COMMA ea=mem_idx
  { instr V32 r1 r2 ea }
| instr=stp_instr r1=xreg COMMA r2=xreg COMMA ea=mem_idx
  { instr V64 r1 r2 ea }
| LDPSW  r1=xreg COMMA r2=xreg COMMA  ea=mem_idx
    {
      let ra,idx = ea in
      I_LDPSW (r1,r2,ra,idx)
    }
| LDXP wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_LDXP (V32,XP,$2,$4,$7) }
| LDXP xreg COMMA xreg COMMA LBRK cxreg RBRK
  { I_LDXP (V64,XP,$2,$4,$7) }
| LDAXP wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_LDXP (V32,AXP,$2,$4,$7) }
| LDAXP xreg COMMA xreg COMMA LBRK cxreg RBRK
  { I_LDXP (V64,AXP,$2,$4,$7) }
| STXP wreg COMMA wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_STXP (V32,YY,$2,$4,$6,$9) }
| STXP wreg COMMA xreg COMMA xreg COMMA LBRK cxreg RBRK
  { I_STXP (V64,YY,$2,$4,$6,$9) }
| STLXP wreg COMMA wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_STXP (V32,LY,$2,$4,$6,$9) }
| STLXP wreg COMMA xreg COMMA xreg COMMA LBRK cxreg RBRK
  { I_STXP (V64,LY,$2,$4,$6,$9) }
| LDRB wreg COMMA mem_ea
  { let (ra,idx) = $4 in I_LDRBH (B,$2,ra,idx) }
| LDRH wreg COMMA mem_ea
  { let ra,idx = $4 in I_LDRBH (H,$2,ra,idx) }
| LDAR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in I_LDAR (v,AA,r,$5) }
| LDARB wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (B,AA,$2,$5) }
| LDARH wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (H,AA,$2,$5) }
| LDXR reg COMMA LBRK cxreg RBRK
    { let v,r = $2 in I_LDAR (v,XX,r,$5) }
| LDXRB wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (B,XX,$2,$5) }
| LDXRH wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (H,XX,$2,$5) }
| LDAXR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in I_LDAR (v,AX,r,$5) }
| LDAXRB wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (B,AX,$2,$5) }
| LDAXRH wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (H,AX,$2,$5) }
| LDAPR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in I_LDAR (v,AQ,r,$5) }
| LDAPRB wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (B,AQ,$2,$5) }
| LDAPRH wreg COMMA LBRK cxreg RBRK
  { I_LDARBH (H,AQ,$2,$5) }
| STR wxreg COMMA mem_ea
  { let (v,r)   = $2 and (ra,ext) = $4 in I_STR (v,r,ra,ext) }
| STRB wreg COMMA mem_ea
  { let (ra,idx) = $4 in I_STRBH (B,$2,ra,idx) }
| STRH wreg COMMA mem_ea
  { let (ra,idx) = $4 in I_STRBH (H,$2,ra,idx) }
| STLR reg COMMA LBRK cxreg RBRK
  { let v,r = $2 in I_STLR (v,r,$5) }
| STLRB wreg COMMA LBRK cxreg RBRK
  { I_STLRBH (B,$2,$5) }
| STLRH wreg COMMA LBRK cxreg RBRK
  { I_STLRBH (H,$2,$5) }
| STXR wreg COMMA reg COMMA LBRK cxreg RBRK
  { let v,r = $4 in I_STXR (v,YY,$2,r,$7) }
| STXRB wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_STXRBH (B,YY,$2,$4,$7) }
| STXRH wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_STXRBH (H,YY,$2,$4,$7) }
| STLXR wreg COMMA reg COMMA LBRK cxreg RBRK
  { let v,r = $4 in I_STXR (v,LY,$2,r,$7) }
| STLXRB wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_STXRBH (B,LY,$2,$4,$7) }
| STLXRH wreg COMMA wreg COMMA LBRK cxreg RBRK
  { I_STXRBH (H,LY,$2,$4,$7) }
   /* Neon extension Memory */
| LD1 vregs1 INDEX COMMA LBRK xreg RBRK kx0_no_shift
  { I_LD1 ($2, $3, $6, $8) }
| LDAP1 vregs1 INDEX COMMA LBRK xreg RBRK
  { match List.hd $2 with
    | Vreg(_,(0,64)) -> I_LDAP1 ($2, $3, $6, K (MetaConst.zero))
    | _ -> assert false
  }
| LD1 vregs COMMA LBRK xreg RBRK kx0_no_shift
  { I_LD1M ($2, $5, $7) }
| LD1R vregs1 COMMA LBRK xreg RBRK kx0_no_shift
  { I_LD1R ($2, $5, $7) }
| LD2 vregs2 INDEX COMMA LBRK xreg RBRK kx0_no_shift
  { I_LD2 ($2, $3, $6, $8) }
| LD2 vregs2 COMMA LBRK xreg RBRK kx0_no_shift
  { I_LD2M ($2, $5, $7) }
| LD2R vregs2 COMMA LBRK xreg RBRK kx0_no_shift
  { I_LD2R ($2, $5, $7) }
| LD3 vregs3 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { I_LD3 ($2, $3, $6, $8) }
| LD3 vregs3 COMMA LBRK xreg RBRK kx0_no_shift
   { I_LD3M ($2, $5, $7)}
| LD3R vregs3 COMMA LBRK xreg RBRK kx0_no_shift
   { I_LD3R ($2, $5, $7) }
| LD4 vregs4 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { I_LD4 ($2, $3, $6, $8) }
| LD4 vregs4 COMMA LBRK xreg RBRK kx0_no_shift
   { I_LD4M ($2, $5, $7) }
| LD4R vregs4 COMMA LBRK xreg RBRK kx0_no_shift
   { I_LD4R ($2, $5, $7) }
| ST1 vregs1 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST1 ($2, $3, $6, $8) }
| STL1 vregs1 INDEX COMMA LBRK xreg RBRK
  { match List.hd $2 with
    | Vreg(_,(0,64)) -> I_STL1 ($2, $3, $6,  K (MetaConst.zero))
    | _ -> assert false
  }
| ST1 vregs COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST1M ($2, $5, $7) }
| ST2 vregs2 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST2 ($2, $3, $6, $8) }
| ST2 vregs2 COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST2M ($2, $5, $7) }
| ST3 vregs3 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST3 ($2, $3, $6, $8) }
| ST3 vregs3 COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST3M ($2, $5, $7) }
| ST4 vregs4 INDEX COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST4 ($2, $3, $6, $8) }
| ST4 vregs4 COMMA LBRK xreg RBRK kx0_no_shift
   { I_ST4M ($2, $5, $7) }
| ldp_simd_instr sreg COMMA sreg COMMA mem_idx
  { $1 VSIMD32 $2 $4 $6 }
| ldp_simd_instr dreg COMMA dreg COMMA mem_idx
  { $1 VSIMD64 $2 $4 $6 }
| ldp_simd_instr qreg COMMA qreg COMMA mem_idx
  { $1 VSIMD128 $2 $4 $6 }
| stp_simd_instr sreg COMMA sreg COMMA mem_idx
  { $1 VSIMD32 $2 $4 $6 }
| stp_simd_instr dreg COMMA dreg COMMA mem_idx
  { $1 VSIMD64 $2 $4 $6 }
| stp_simd_instr qreg COMMA qreg COMMA mem_idx
  { $1 VSIMD128 $2 $4 $6 }
| LDR scalar_regs COMMA mem_ea
  { let (v,r)   = $2 and (ra,ext) = $4 in I_LDR_SIMD (v,r,ra,ext) }
| LDUR scalar_regs COMMA LBRK xreg k0 RBRK
  { let v,r = $2 in
    I_LDUR_SIMD (v, r, $5, $6) }
| LDAPUR scalar_regs COMMA LBRK xreg k0 RBRK
  { let v,r = $2 in
    I_LDAPUR_SIMD (v, r, $5, $6) }
| STR scalar_regs COMMA mem_ea
  { let (v,r)   = $2 and (ra,ext) = $4 in I_STR_SIMD (v,r,ra,ext) }
| STUR scalar_regs COMMA LBRK xreg k0 RBRK
  { let v,r = $2 in
    I_STUR_SIMD (v, r, $5, $6) }
| STLUR scalar_regs COMMA LBRK xreg k0 RBRK
  { let v,r = $2 in
    I_STLUR_SIMD (v, r, $5, $6) }
| ADDV breg COMMA vreg
  {  I_ADDV (VSIMD8, $2, $4) }
| ADDV hreg COMMA vreg
  {  I_ADDV (VSIMD16, $2, $4) }
| ADDV sreg COMMA vreg
  {  I_ADDV (VSIMD32, $2, $4) }
| DUP vreg COMMA wxreg
  { let v,r = $4 in
    I_DUP ($2 , v, r) }
| FMOV wreg COMMA hreg
   { I_FMOV_TG (V32, $2, VSIMD16, $4) }
| FMOV xreg COMMA hreg
   { I_FMOV_TG (V64, $2, VSIMD16, $4) }
| FMOV wreg COMMA sreg
   { I_FMOV_TG (V32, $2, VSIMD32, $4) }
| FMOV xreg COMMA dreg
   { I_FMOV_TG (V64, $2, VSIMD64, $4) }
| MOV vreg INDEX COMMA vreg INDEX
  { I_MOV_VE ($2, $3, $5, $6) }
| MOV vreg INDEX COMMA wxreg
  { let v,r = $5 in
    I_MOV_FG ($2, $3, v, r) }
| MOV xreg COMMA vreg INDEX
  { I_MOV_TG (V64, $2, $4, $5) }
| MOV wreg COMMA vreg INDEX
  { I_MOV_TG (V32, $2, $4, $5) }
| MOV vreg COMMA vreg
  { I_MOV_V ($2, $4) }
| MOV bhsdregs COMMA vreg INDEX
  { let v,r = $2 in
    I_MOV_S (v, r, $4 ,$5) }
| MOVI vreg COMMA k
  { I_MOVI_V ($2, $4, S_NOEXT) }
| MOVI vreg COMMA k COMMA shift
  { I_MOVI_V ($2, $4, $6) }
| MOVI dreg COMMA k
  { I_MOVI_S ( VSIMD64, $2, $4) }
| OP vreg COMMA vreg COMMA vreg
  { match $1 with
    | EOR -> I_EOR_SIMD ($2,$4,$6)
    | _ -> assert false}
| TOK_ADD vreg COMMA vreg COMMA vreg
  { I_ADD_SIMD ($2,$4,$6) }
| TOK_ADD dreg COMMA dreg COMMA dreg
  { I_ADD_SIMD_S ($2,$4,$6)}
    /* Compare and swap */
| CAS wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V32,RMW_P,$2,$4,$7) }
| CAS xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V64,RMW_P,$2,$4,$7) }
| CAS creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V128,RMW_P,$2,$4,$7) }
| CASA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V32,RMW_A,$2,$4,$7) }
| CASA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V64,RMW_A,$2,$4,$7) }
| CASA creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V128,RMW_A,$2,$4,$7) }
| CASL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V32,RMW_L,$2,$4,$7) }
| CASL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V64,RMW_L,$2,$4,$7) }
| CASL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V128,RMW_L,$2,$4,$7) }
| CASAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V32,RMW_AL,$2,$4,$7) }
| CASAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V64,RMW_AL,$2,$4,$7) }
| CASAL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_CAS (V128,RMW_AL,$2,$4,$7) }
| CASB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (B,RMW_P,$2,$4,$7) }
| CASAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (B,RMW_A,$2,$4,$7) }
| CASLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (B,RMW_L,$2,$4,$7) }
| CASALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (B,RMW_AL,$2,$4,$7) }
| CASH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (H,RMW_P,$2,$4,$7) }
| CASAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (H,RMW_A,$2,$4,$7) }
| CASLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (H,RMW_L,$2,$4,$7) }
| CASALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_CASBH (H,RMW_AL,$2,$4,$7) }
| CASP wreg COMMA wreg COMMA wreg COMMA wreg COMMA LBRK xreg RBRK
  { I_CASP (V32,RMW_P,$2,$4,$6,$8,$11) }
| CASP xreg COMMA xreg COMMA xreg COMMA xreg COMMA LBRK cxreg zeroopt RBRK
  { I_CASP (V64,RMW_P,$2,$4,$6,$8,$11) }
| CASPA wreg COMMA wreg COMMA wreg COMMA wreg COMMA LBRK cxreg zeroopt RBRK
  { I_CASP (V32,RMW_A,$2,$4,$6,$8,$11) }
| CASPA xreg COMMA xreg COMMA xreg COMMA xreg COMMA LBRK cxreg zeroopt RBRK
  { I_CASP (V64,RMW_A,$2,$4,$6,$8,$11) }
| CASPL wreg COMMA wreg COMMA wreg COMMA wreg COMMA LBRK cxreg zeroopt RBRK
  { I_CASP (V32,RMW_L,$2,$4,$6,$8,$11) }
| CASPL xreg COMMA xreg COMMA xreg COMMA xreg COMMA LBRK cxreg zeroopt RBRK
  { I_CASP (V64,RMW_L,$2,$4,$6,$8,$11) }
| CASPAL wreg COMMA wreg COMMA wreg COMMA wreg COMMA LBRK cxreg zeroopt RBRK
  { I_CASP (V32,RMW_AL,$2,$4,$6,$8,$11) }
| CASPAL xreg COMMA xreg COMMA xreg COMMA xreg COMMA LBRK cxreg zeroopt RBRK
  { I_CASP (V64,RMW_AL,$2,$4,$6,$8,$11) }
/* Swap */
| SWP wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V32,RMW_P,$2,$4,$7) }
| SWP xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V64,RMW_P,$2,$4,$7) }
| SWP creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V128,RMW_P,$2,$4,$7) }
| SWPA wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V32,RMW_A,$2,$4,$7) }
| SWPA xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V64,RMW_A,$2,$4,$7) }
| SWPA creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V128,RMW_A,$2,$4,$7) }
| SWPL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V32,RMW_L,$2,$4,$7) }
| SWPL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V64,RMW_L,$2,$4,$7) }
| SWPL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V128,RMW_L,$2,$4,$7) }
| SWPAL wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V32,RMW_AL,$2,$4,$7) }
| SWPAL xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V64,RMW_AL,$2,$4,$7) }
| SWPAL creg COMMA creg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWP (V128,RMW_AL,$2,$4,$7) }
| SWPB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (B,RMW_P,$2,$4,$7) }
| SWPAB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (B,RMW_A,$2,$4,$7) }
| SWPLB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (B,RMW_L,$2,$4,$7) }
| SWPALB wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (B,RMW_AL,$2,$4,$7) }
| SWPH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (H,RMW_P,$2,$4,$7) }
| SWPAH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (H,RMW_A,$2,$4,$7) }
| SWPLH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (H,RMW_L,$2,$4,$7) }
| SWPALH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
  { I_SWPBH (H,RMW_AL,$2,$4,$7) }
/* Memory Tagging */
| STG xreg COMMA mem_idx
    {
      let r,idx = $4 in
      I_STG ($2,r,idx)
    }
| STZG xreg COMMA mem_idx
    {
      let r,idx = $4 in
      I_STZG ($2,r,idx)
    }
| STZ2G xreg COMMA mem_idx
    {
      let r,idx = $4 in
      I_STZ2G ($2,r,idx)
    }
| LDG xreg COMMA LBRK xreg k0 RBRK
   { I_LDG ($2,$5,$6) }

/* Fetch and OP */
| LDOP wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { let op,rmw = $1 in I_LDOP (op,V32,rmw,$2,$4,$7) }
| LDOP xreg COMMA xreg COMMA  LBRK cxreg zeroopt RBRK
   { let op,rmw = $1 in I_LDOP (op,V64,rmw,$2,$4,$7) }
| LDOPBH wreg COMMA wreg COMMA  LBRK cxreg zeroopt RBRK
   { let v,op,rmw = $1 in I_LDOPBH (op,v,rmw,$2,$4,$7) }
| STOP wreg COMMA LBRK cxreg zeroopt RBRK
   { let op,w = $1 in I_STOP (op,V32,w,$2,$5) }
| STOPBH wreg COMMA LBRK cxreg zeroopt RBRK
   { let v,op,w = $1 in I_STOPBH (op,v,w,$2,$5) }
/* Operations */
| MOV xreg COMMA kr
  { I_MOV (V64,$2,$4) }
| MOV wreg COMMA kwr
  { I_MOV (V32,$2,$4) }
| MOV creg COMMA creg
  { I_MOV (V128,$2,RV (V128,$4)) }
| CPY creg COMMA creg
  { I_MOV (V128,$2,RV (V128,$4)) }

| MOVZ xreg COMMA k
  { I_MOVZ (V64,$2, $4, S_NOEXT) }
| MOVZ xreg COMMA k COMMA TOK_LSL k
  { I_MOVZ (V64,$2, $4, S_LSL $7) }
| MOVZ wreg COMMA k
  { I_MOVZ (V32,$2,$4, S_NOEXT) }
| MOVZ wreg COMMA k COMMA TOK_LSL k
  { I_MOVZ (V32,$2,$4, S_LSL $7) }
| MOVN xreg COMMA k
  { I_MOVN (V64,$2, $4, S_NOEXT) }
| MOVN xreg COMMA k COMMA TOK_LSL k
  { I_MOVN (V64,$2, $4, S_LSL $7) }
| MOVN wreg COMMA k
  { I_MOVN (V32,$2,$4, S_NOEXT) }
| MOVN wreg COMMA k COMMA TOK_LSL k
  { I_MOVN (V32,$2,$4, S_LSL $7) }
| MOVK xreg COMMA k
  { I_MOVK (V64,$2, $4, S_NOEXT) }
| MOVK xreg COMMA k COMMA TOK_LSL k
  { I_MOVK (V64,$2, $4, S_LSL $7) }
| MOVK wreg COMMA k
  { I_MOVK (V32,$2,$4, S_NOEXT) }
| MOVK wreg COMMA k COMMA TOK_LSL k
  { I_MOVK (V32,$2,$4, S_LSL $7) }
| ADR xreg COMMA label_addr
  { I_ADR ($2,$4) }
| TOK_SXTW xreg COMMA wreg
  { I_SXTW ($2,$4) }
| MVN wreg COMMA wreg
  { I_OP3 (V32,ORN,$2,ZR,OpExt.Reg ($4,OpExt.LSL MetaConst.zero)) }
| MVN xreg COMMA xreg
  { I_OP3 (V64,ORN,$2,ZR,OpExt.Reg ($4,OpExt.LSL MetaConst.zero)) }
/* Special handling for ASR/LSL/LSR operation */
| tok_instr_shiftv xreg COMMA xreg COMMA op_ext_reg_x
  { I_OP3 (V64, $1, $2, $4, $6) }
| tok_instr_shiftv wreg COMMA wreg COMMA op_ext_reg_w
  { I_OP3 (V32, $1, $2, $4, $6) }
| tok_instr_shift xreg COMMA xreg COMMA op_ext_x
  { I_OP3 (V64, $1, $2, $4, $6) }
| tok_instr_shift wreg COMMA wreg COMMA op_ext_w
  { I_OP3 (V32, $1, $2, $4, $6) }

| SBFM xreg COMMA xreg COMMA k COMMA k
  { I_SBFM (V64,$2,$4,$6,$8) }
| SBFM wreg COMMA wreg COMMA k COMMA k
  { I_SBFM (V32,$2,$4,$6,$8) }
| UBFM xreg COMMA xreg COMMA k COMMA k
  { I_UBFM (V64,$2,$4,$6,$8) }
| UBFM wreg COMMA wreg COMMA k COMMA k
  { I_UBFM (V32,$2,$4,$6,$8) }
(* Generic OP3 *)
| OP xreg COMMA xreg COMMA op_ext_x
  { check_op3 $1 $6 ; I_OP3 (V64,$1,$2,$4,$6) }
| OP wreg COMMA wreg COMMA op_ext_w
  { check_op3 $1 $6 ; I_OP3 (V32,$1,$2,$4,$6) }

(* Addition, also consider extended register *)
| tok_add_sub xreg COMMA xreg COMMA op_ext_x
  { I_OP3 (V64, $1, $2, $4, $6) }
| tok_add_sub wreg COMMA wreg COMMA op_ext_w
  { I_OP3 (V32, $1, $2, $4, $6) }
| tok_add_sub creg COMMA creg COMMA op_ext_c
  { I_OP3 (V128, $1, $2, $4, $6) }
| tok_add_sub_ext creg COMMA creg COMMA xreg COMMA add_sub_ext
  { I_ADDSUBEXT (V128, $1, $2, $4, (V64, $6), $8) }
| tok_add_sub_ext xreg COMMA xreg COMMA wxreg COMMA add_sub_ext
  { I_ADDSUBEXT (V64,$1,$2,$4,$6,$8) }
| tok_add_sub_ext wreg COMMA wreg COMMA wreg COMMA add_sub_ext
  { I_ADDSUBEXT (V32, $1, $2, $4, (V32, $6), $8) }

(* Multiplication, special forms *)
| MOPLZ xreg COMMA wreg COMMA wreg
    { I_MOPL ($1,$2,$4,$6,ZR) }
| MOPL xreg COMMA wreg COMMA wreg COMMA xreg
    { I_MOPL ($1,$2,$4,$6,$8) }

(* Aliases of SUB *)
| CMP wreg COMMA op_ext_w
  { I_OP3 (V32,SUBS,ZR,$2,$4) }
| CMP xreg COMMA op_ext_x
  { I_OP3 (V64,SUBS,ZR,$2,$4) }
| CMP wreg COMMA wreg COMMA add_sub_ext
  { I_ADDSUBEXT (V32,Ext.SUBS,ZR,$2,(V32,$4),$6) }
| CMP xreg COMMA wxreg COMMA add_sub_ext
  { I_ADDSUBEXT (V64,Ext.SUBS,ZR,$2,$4,$6) }
| TOK_NEG wreg COMMA  op_ext_reg_w
  { I_OP3 (V32,SUB,$2,ZR,$4) }
| TOK_NEG xreg COMMA  op_ext_reg_x
  { I_OP3 (V64,SUB,$2,ZR,$4) }
| TOK_NEGS wreg COMMA  op_ext_reg_w
  { I_OP3 (V32,SUBS,$2,ZR,$4) }
| TOK_NEGS xreg COMMA  op_ext_reg_x
  { I_OP3 (V64,SUBS,$2,ZR,$4) }

| TST wreg COMMA op_ext_w
  { I_OP3 (V32,ANDS,ZR,$2,$4) }
| TST xreg COMMA op_ext_x
  { I_OP3 (V64,ANDS,ZR,$2,$4) }

| RBIT wreg COMMA wreg
  { I_RBIT (V32,$2,$4) }
| RBIT xreg COMMA xreg
  { I_RBIT (V64,$2,$4) }

| REV16 wreg COMMA wreg
  { I_REV (RV16 AArch64Base.V32,$2,$4) }
| REV16 xreg COMMA xreg
  { I_REV (RV16 AArch64Base.V64,$2,$4) }
| REV32 xreg COMMA xreg
  { I_REV (RV32,$2,$4) }
| REV64 xreg COMMA xreg
  { I_REV (RV64 AArch64Base.V64 ,$2,$4) }
| REV wreg COMMA wreg
  { I_REV (RV64 AArch64Base.V32,$2,$4) }
| REV xreg COMMA xreg
  { I_REV (RV64 AArch64Base.V64,$2,$4) }

| EXTR xreg COMMA xreg COMMA xreg COMMA  k
   { I_EXTR (V64,$2,$4,$6,$8) }
| EXTR wreg COMMA wreg COMMA wreg COMMA  k
   { I_EXTR (V32,$2,$4,$6,$8) }

| ABS wreg COMMA wreg
  { I_ABS (V32,$2,$4) }
| ABS xreg COMMA xreg
  { I_ABS (V64,$2,$4) }

/* Morello */
| ALIGND creg COMMA creg COMMA k
  { I_ALIGND ($2,$4,$6) }
| ALIGNU creg COMMA creg COMMA k
  { I_ALIGNU ($2,$4,$6) }
| BUILD creg COMMA creg COMMA creg
  { I_BUILD ($2,$4,$6) }
| CHKEQ creg COMMA creg
  { I_CHKEQ ($2,$4) }
| CHKSLD creg
  { I_CHKSLD ($2) }
| CHKTGD creg
  { I_CHKTGD ($2) }
| CLRTAG creg COMMA creg
  { I_CLRTAG ($2,$4) }
| CPYTYPE creg COMMA creg COMMA creg
  { I_CPYTYPE ($2,$4,$6) }
| CPYVALUE creg COMMA creg COMMA creg
  { I_CPYVALUE ($2,$4,$6) }
| CSEAL creg COMMA creg COMMA creg
  { I_CSEAL ($2,$4,$6) }
| GC xreg COMMA creg
  { I_GC ($1,$2,$4) }
| LDCT xreg COMMA LBRK cxreg RBRK
  { I_LDCT ($2,$5) }
| SC creg COMMA creg COMMA xreg
  { I_SC ($1,$2,$4,$6) }
| SEAL creg COMMA creg COMMA creg
  { I_SEAL ($2,$4,$6) }
| STCT xreg COMMA LBRK cxreg RBRK
  { I_STCT ($2,$5) }
| UNSEAL creg COMMA creg COMMA creg
  { I_UNSEAL ($2,$4,$6) }
/* Misc */
| CSEL xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Cpy) }
| CSEL wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Cpy) }
| CSEL creg COMMA  creg COMMA  creg COMMA cond
  { I_CSEL (V128,$2,$4,$6,$8,Cpy) }
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
| CSETM wreg COMMA cond
  { I_CSEL (V32,$2,ZR,ZR,inverse_cond $4,Inv) }
| CSETM xreg COMMA cond
  { I_CSEL (V64,$2,ZR,ZR,inverse_cond $4,Inv) }
| CINC wreg COMMA wreg COMMA cond
  { I_CSEL (V32,$2,$4,$4,inverse_cond $6,Inc) }
| CINC xreg COMMA xreg COMMA cond
  { I_CSEL (V64,$2,$4,$4,inverse_cond $6,Inc) }
/* Fences */
| TOK_DMB fenceopt
  { let d,t = $2 in I_FENCE (DMB (d,t)) }
| TOK_DSB fenceopt
  { let d,t = $2 in I_FENCE (DSB (d,t)) }
| TOK_ISB
  { I_FENCE ISB }
/* Cache Maintenance */
| IC IC_OP
  { I_IC ($2,ZR) }
| IC IC_OP COMMA xreg
  { I_IC ($2,$4) }
| IC IVAU COMMA xreg
  { I_IC (IC.({ funct=I; typ=VA; point=U; domain=NO; }),$4) }
| DC IVAU COMMA xreg
  { I_DC (DC.({ funct=I; typ=VA; point=U; }),$4) }
| DC DC_OP COMMA xreg
  { I_DC ($2,$4) }
| TLBI TLBI_OP
  { I_TLBI ($2, ZR) }
| TLBI TLBI_OP COMMA xreg
  { I_TLBI ($2, $4) }

/* System register */
| MRS xreg COMMA SYSREG
  { I_MRS ($2,$4) }
| MSR SYSREG COMMA xreg
  { I_MSR ($2,$4) }
| UDF NUM
  { I_UDF (MetaConst.Int $2) }

fenceopt:
| TOK_SY
  { SY,FULL }
| TOK_ST
  { SY,ST }
| TOK_LD
  { SY,LD }
| TOK_OSH
  { OSH,FULL }
| TOK_OSHST
  { OSH,ST }
| TOK_OSHLD
  { OSH,LD }
| TOK_ISH
  { ISH,FULL }
| TOK_ISHST
  { ISH,ST }
| TOK_ISHLD
  { ISH,LD }
| TOK_NSH
  { NSH,FULL }
| TOK_NSHST
  { NSH,ST }
| TOK_NSHLD
  { NSH,LD}
