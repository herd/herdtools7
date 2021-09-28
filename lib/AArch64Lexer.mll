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

{

module type Config = sig
 include LexUtils.Config
 val is_morello : bool
end

module Make(O:Config) = struct
open Lexing
open LexMisc
open AArch64Parser
module A = AArch64Base
module LU = LexUtils.Make(O)

let parse_creg = if O.is_morello then A.parse_creg  else fun _ -> None

let check_name name =
if O.debug then Printf.eprintf "Check: '%s'\n"  name ;
match name with
| "nop"|"NOP" -> NOP
(* Hints are NOPS in AArch64 *)
| "hint"|"HINT" -> HINT
(* Halt instructions are used by Debug mode, not needed here - NOP *)
| "hlt" | "HLT" -> HLT
(* Branch *)
| "b"  | "B"  -> TOK_B
| "br"  | "BR"  -> BR
| "bl"  | "BL"  -> BL
| "blr"  | "BLR"  -> BLR
| "ret"  | "RET" -> RET
| "eret"  | "ERET" -> ERET
| "ne"  | "NE"  -> TOK_NE
| "eq"  | "EQ"  -> TOK_EQ
| "ge"  | "GE"  -> TOK_GE
| "gt"  | "GT"  -> TOK_GT
| "le"  | "LE"  -> TOK_LE
| "lt"  | "LT"  -> TOK_LT
| "cs"  | "CS"  -> TOK_CS
| "cc"  | "CC"  -> TOK_CC
| "mi"  | "MI"  -> TOK_MI
| "pl"  | "PL"  -> TOK_PL
| "vs"  | "VS"  -> TOK_VS
| "vc"  | "VC"  -> TOK_VC
| "hi"  | "HI"  -> TOK_HI
| "ls"  | "LS"  -> TOK_LS
| "al"  | "AL"  -> TOK_AL
| "b.eq" | "B.EQ" -> BEQ
| "b.ne" | "B.NE" -> BNE
| "b.ge" | "B.GE" -> BGE
| "b.gt" | "B.GT" -> BGT
| "b.le" | "B.LE" -> BLE
| "b.lt" | "B.LT" -> BLT
| "b.cs" | "B.CS" -> BCS
| "b.cc" | "B.CC" -> BCC
| "b.mi" | "B.MI" -> BMI
| "b.pl" | "B.PL" -> BPL
| "b.vs" | "B.VS" -> BVS
| "b.vc" | "B.VC" -> BVC
| "b.hi" | "B.HI" -> BHI
| "b.ls" | "B.LS" -> BLS
| "b.al" | "B.AL" -> BAL
| "cbz"  | "CBZ" -> CBZ
| "cbnz"  | "CBNZ" -> CBNZ
| "tbnz" | "TBNZ" -> TBNZ
| "tbz" | "TBZ" -> TBZ
(* Memory *)
| "ldr"|"LDR" -> LDR
| "ldur"|"LDUR" -> LDUR
| "ldp"|"LDP" -> LDP
| "ldpsw"|"LDPSW" -> LDPSW
| "ldnp"|"LDNP" -> LDNP
| "stp"|"STP" -> STP
| "stnp"|"STNP" -> STNP
| "ldrb"|"LDRB" -> LDRB
| "ldrh"|"LDRH" -> LDRH
| "ldrsb"|"LDRSB" -> LDRSB
| "ldrsh"|"LDRSH" -> LDRSH
| "ldar"|"LDAR" -> LDAR
| "ldarb"|"LDARB" -> LDARB
| "ldarh"|"LDARH" -> LDARH
| "ldapr"|"LDAPR" -> LDAPR
| "ldaprb"|"LDAPRB" -> LDAPRB
| "ldaprh"|"LDAPRH" -> LDAPRH
| "ldxr"|"LDXR" -> LDXR
| "ldxrb"|"LDXRB" -> LDXRB
| "ldxrh"|"LDXRH" -> LDXRH
| "ldaxr"|"LDAXR" -> LDAXR
| "ldaxrb"|"LDAXRB" -> LDAXRB
| "ldaxrh"|"LDAXRH" -> LDAXRH
| "ldxp"|"LDXP" -> LDXP
| "ldaxp"|"LDAXP" -> LDAXP
| "str"|"STR" -> STR
| "stlr"|"STLR" -> STLR
| "stxr"|"STXR" -> STXR
| "stlxr"|"STLXR" -> STLXR
| "strb"|"STRB" -> STRB
| "strh"|"STRH" -> STRH
| "stlrb"|"STLRB" -> STLRB
| "stlrh"|"STLRH" -> STLRH
| "stxrb"|"STXRB" -> STXRB
| "stlxrb"|"STLXRB" -> STLXRB
| "stxrh"|"STXRH" -> STXRH
| "stlxrh"|"STLXRH" -> STLXRH
| "stxp"| "STXP" -> STXP
| "stlxp"| "STLXP" -> STLXP
(* Neon Extension Memory *)
| "ld1" | "LD1" -> LD1
| "ld1r" | "LD1R" -> LD1R
| "ld2" | "LD2" -> LD2
| "ld2r" | "LD2R" -> LD2R
| "ld3" | "LD3" -> LD3
| "ld3r" | "LD3R" -> LD3R
| "ld4" | "LD4" -> LD4
| "ld4r" | "LD4R" -> LD4R
| "stur" | "STUR" -> STUR
| "st1" | "ST1" -> ST1
| "st2" | "ST2" -> ST2
| "st3" | "ST3" -> ST3
| "st4" | "ST4" -> ST4
| "movi" | "MOVI" -> MOVI
| "mvn" | "MVN" -> MVN
(* Compare and swap *)
| "cas"|"CAS" -> CAS
| "casa"|"CASA" -> CASA
| "casl"|"CASL" -> CASL
| "casal"|"CASAL" -> CASAL
| "cash"|"CASH" -> CASH
| "casah"|"CASAH" -> CASAH
| "caslh"|"CASLH" -> CASLH
| "casalh"|"CASALH" -> CASALH
| "casb"|"CASB" -> CASB
| "casab"|"CASAB" -> CASAB
| "caslb"|"CASLB" -> CASLB
| "casalb"|"CASALB" -> CASALB
(* Swap *)
| "swp"|"SWP" -> SWP
| "swpa"|"SWPA" -> SWPA
| "swpl"|"SWPL" -> SWPL
| "swpal"|"SWPAL" -> SWPAL
| "swph"|"SWPH" -> SWPH
| "swpah"|"SWPAH" -> SWPAH
| "swplh"|"SWPLH" -> SWPLH
| "swpalh"|"SWPALH" -> SWPALH
| "swpb"|"SWPB" -> SWPB
| "swpab"|"SWPAB" -> SWPAB
| "swplb"|"SWPLB" -> SWPLB
| "swpalb"|"SWPALB" -> SWPALB
(* Fetch and ADD *)
| "ldadd"|"LDADD" -> LDADD
| "ldadda"|"LDADDA" -> LDADDA
| "ldaddl"|"LDADDL" -> LDADDL
| "ldaddal"|"LDADDAL" -> LDADDAL
| "ldaddh"|"LDADDH" -> LDADDH
| "ldaddah"|"LDADDAH" -> LDADDAH
| "ldaddlh"|"LDADDLH" -> LDADDLH
| "ldaddalh"|"LDADDALH" -> LDADDALH
| "ldaddb"|"LDADDB" -> LDADDB
| "ldaddab"|"LDADDAB" -> LDADDAB
| "ldaddlb"|"LDADDLB" -> LDADDLB
| "ldaddalb"|"LDADDALB" -> LDADDALB
| "stadd"|"STADD" -> STADD
| "staddl"|"STADDL" -> STADDL
| "staddh"|"STADDH" -> STADDH
| "staddlh"|"STADDLH" -> STADDLH
| "staddb"|"STADDB" -> STADDB
| "staddlb"|"STADDLB" -> STADDLB
(* Fetch and Xor *)
| "ldeor"|"LDEOR" -> LDEOR
| "ldeora"|"LDEORA" -> LDEORA
| "ldeorl"|"LDEORL" -> LDEORL
| "ldeoral"|"LDEORAL" -> LDEORAL
| "ldeorh"|"LDEORH" -> LDEORH
| "ldeorah"|"LDEORAH" -> LDEORAH
| "ldeorlh"|"LDEORLH" -> LDEORLH
| "ldeoralh"|"LDEORALH" -> LDEORALH
| "ldeorb"|"LDEORB" -> LDEORB
| "ldeorab"|"LDEORAB" -> LDEORAB
| "ldeorlb"|"LDEORLB" -> LDEORLB
| "ldeoralb"|"LDEORALB" -> LDEORALB
| "steor"|"STEOR" -> STEOR
| "steorl"|"STEORL" -> STEORL
| "steorh"|"STEORH" -> STEORH
| "steorlh"|"STEORLH" -> STEORLH
| "steorb"|"STEORB" -> STEORB
| "steorlb"|"STEORLB" -> STEORLB
(* Fetch and Or *)
| "ldset"|"LDSET" -> LDSET
| "ldseta"|"LDSETA" -> LDSETA
| "ldsetl"|"LDSETL" -> LDSETL
| "ldsetal"|"LDSETAL" -> LDSETAL
| "ldseth"|"LDSETH" -> LDSETH
| "ldsetah"|"LDSETAH" -> LDSETA
| "ldsetlh"|"LDSETLH" -> LDSETLH
| "ldsetalh"|"LDSETALH" -> LDSETALH
| "ldsetb"|"LDSETB" -> LDSETB
| "ldsetab"|"LDSETAB" -> LDSETAB
| "ldsetlb"|"LDSETLB" -> LDSETLB
| "ldsetalb"|"LDSETALB" -> LDSETALB
| "stset"|"STSET" -> STSET
| "stsetl"|"STSETL" -> STSETL
| "stseth"|"STSETH" -> STSETH
| "stsetlh"|"STSETLH" -> STSETLH
| "stsetb"|"STSETB" -> STSETB
| "stsetlb"|"STSETLB" -> STSETLB
(* Fetch and b1.b2. b1 & ~b2 *)
| "ldclr"|"LDCLR" -> LDCLR
| "ldclra"|"LDCLRA" -> LDCLRA
| "ldclrl"|"LDCLRL" -> LDCLRL
| "ldclral"|"LDCLRAL" -> LDCLRAL
| "ldclrh"|"LDCLRH" -> LDCLRH
| "ldclrah"|"LDCLRAH" -> LDCLRAH
| "ldclrlh"|"LDCLRLH" -> LDCLRLH
| "ldclralh"|"LDCLRALH" -> LDCLRALH
| "ldclrb"|"LDCLRB" -> LDCLRB
| "ldclrab"|"LDCLRAB" -> LDCLRAB
| "ldclrlb"|"LDCLRLB" -> LDCLRLB
| "ldclralb"|"LDCLRALB" -> LDCLRALB
| "stclr"|"STCLR" -> STCLR
| "stclrl"|"STCLRL" -> STCLRL
| "stclrh"|"STCLRH" -> STCLRH
| "stclrlh"|"STCLRLH" -> STCLRLH
| "stclrb"|"STCLRB" -> STCLRB
| "stclrlb"|"STCLRLB" -> STCLRLB
(* Fetch and Max, Signed *)
| "ldsmax"|"LDSMAX" -> LDSMAX
| "ldsmaxa"|"LDSMAXA" -> LDSMAXA
| "ldsmaxl"|"LDSMAXL" -> LDSMAXL
| "ldsmaxal"|"LDSMAXAL" -> LDSMAXAL
| "ldsmaxh"|"LDSMAXH" -> LDSMAXH
| "ldsmaxah"|"LDSMAXAH" -> LDSMAXAH
| "ldsmaxlh"|"LDSMAXLH" -> LDSMAXLH
| "ldsmaxalh"|"LDSMAXALH" -> LDSMAXALH
| "ldsmaxb"|"LDSMAXB" -> LDSMAXB
| "ldsmaxab"|"LDSMAXAB" -> LDSMAXAB
| "ldsmaxlb"|"LDSMAXLB" -> LDSMAXLB
| "ldsmaxalb"|"LDSMAXALB" -> LDSMAXALB
| "stsmax"|"STSMAX" -> STSMAX
| "stsmaxl"|"STSMAXL" -> STSMAXL
| "stsmaxh"|"STSMAXH" -> STSMAXH
| "stsmaxlh"|"STSMAXLH" -> STSMAXLH
| "stsmaxb"|"STSMAXB" -> STSMAXB
| "stsmaxlb"|"STSMAXLB" -> STSMAXLB
(* Fetch and Min, Signed *)
| "ldsmin"|"LDSMIN" -> LDSMIN
| "ldsmina"|"LDSMINA" -> LDSMINA
| "ldsminl"|"LDSMINL" -> LDSMINL
| "ldsminal"|"LDSMINAL" -> LDSMINAL
| "ldsminh"|"LDSMINH" -> LDSMINH
| "ldsminah"|"LDSMINAH" -> LDSMINAH
| "ldsminlh"|"LDSMINLH" -> LDSMINLH
| "ldsminalh"|"LDSMINALH" -> LDSMINALH
| "ldsminb"|"LDSMINB" -> LDSMINB
| "ldsminab"|"LDSMINAB" -> LDSMINAB
| "ldsminlb"|"LDSMINLB" -> LDSMINLB
| "ldsminalb"|"LDSMINALB" -> LDSMINALB
| "stsmin"|"STSMIN" -> STSMIN
| "stsminl"|"STSMINL" -> STSMINL
| "stsminh"|"STSMINH" -> STSMINH
| "stsminlh"|"STSMINLH" -> STSMINLH
| "stsminb"|"STSMINB" -> STSMINB
| "stsminlb"|"STSMINLB" -> STSMINLB
(* Fetch and Max, Unsigned *)
(*
| "ldumax"|"LDUMAX" -> LDUMAX
| "ldumaxa"|"LDUMAXA" -> LDUMAXA
| "ldumaxl"|"LDUMAXL" -> LDUMAXL
| "ldumaxal"|"LDUMAXAL" -> LDUMAXAL
| "ldumaxh"|"LDUMAXH" -> LDUMAXH
| "ldumaxah"|"LDUMAXAH" -> LDUMAXAH
| "ldumaxlh"|"LDUMAXLH" -> LDUMAXLH
| "ldumaxalh"|"LDUMAXALH" -> LDUMAXALH
| "ldumaxb"|"LDUMAXB" -> LDUMAXB
| "ldumaxab"|"LDUMAXAB" -> LDUMAXAB
| "ldumaxlb"|"LDUMAXLB" -> LDUMAXLB
| "ldumaxalb"|"LDUMAXALB" -> LDUMAXALB
| "stumax"|"STUMAX" -> STUMAX
| "stumaxl"|"STUMAXL" -> STUMAXL
| "stumaxh"|"STUMAXH" -> STUMAXH
| "stumaxlh"|"STUMAXLH" -> STUMAXLH
| "stumaxb"|"STUMAXB" -> STUMAXB
| "stumaxlb"|"STUMAXLB" -> STUMAXLB
(* Fetch and Min, Unsigned *)
| "ldumin"|"LDUMIN" -> LDUMIN
| "ldumina"|"LDUMINA" -> LDUMINA
| "lduminl"|"LDUMINL" -> LDUMINL
| "lduminal"|"LDUMINAL" -> LDUMINAL
| "lduminh"|"LDUMINH" -> LDUMINH
| "lduminah"|"LDUMINAH" -> LDUMINAH
| "lduminlh"|"LDUMINLH" -> LDUMINLH
| "lduminalh"|"LDUMINALH" -> LDUMINALH
| "lduminb"|"LDUMINB" -> LDUMINB
| "lduminab"|"LDUMINAB" -> LDUMINAB
| "lduminlb"|"LDUMINLB" -> LDUMINLB
| "lduminalb"|"LDUMINALB" -> LDUMINALB
| "stumin"|"STUMIN" -> STUMIN
| "stuminl"|"STUMINL" -> STUMINL
| "stuminh"|"STUMINH" -> STUMINH
| "stuminlh"|"STUMINLH" -> STUMINLH
| "stuminb"|"STUMINB" -> STUMINB
| "stuminlb"|"STUMINLB" -> STUMINLB
*)
(* Memory Tagging *)
| "stg"|"STG" -> STG
| "stzg"|"STZG" -> STZG
| "ldg"|"LDG" -> LDG
(* Operations *)
| "sxtw"|"SXTW" -> TOK_SXTW
| "uxtw"|"UXTW" -> TOK_UXTW
| "mov"|"MOV" -> MOV
| "movz"|"MOVZ" -> MOVZ
| "movk"|"MOVK" -> MOVK
| "adr"|"ADR" -> ADR
| "rbit"|"RBIT" -> RBIT
| "cmp"|"CMP" -> CMP
| "tst"|"TST" -> TST
(* Three argument opcodes factorized *)
| "adds"|"ADDS" -> OP A.ADDS
| "eor"|"EOR" -> OP A.EOR
| "orr"|"ORR" -> OP A.ORR
| "orn"|"ORN" -> OP A.ORN
| "and"|"AND" -> OP A.AND
| "ands"|"ANDS" -> OP A.ANDS
| "bic"|"BIC" -> OP A.BIC
| "bics"|"BICS" -> OP A.BICS
(* Some arithmetic instruction have their own lexeme,
   for parser to handle then in special ways *)
(* Also used as barrel shift *)
| "asr" | "ASR" -> TOK_ASR
| "lsl" | "LSL" -> TOK_LSL
| "lsr" | "LSR" -> TOK_LSR
(* SUB, SUBS, ADD have 128 bits semantics*)
| "sub"|"SUB" -> TOK_SUB
| "subs"|"SUBS" -> TOK_SUBS
| "add"|"ADD" -> TOK_ADD
(* Morello *)
| "alignd"|"ALIGND" -> ALIGND
| "alignu"|"ALIGNU" -> ALIGNU
| "build"|"BUILD" -> BUILD
| "chkeq"|"CHKEQ" -> CHKEQ
| "chksld"|"CHKSLD" -> CHKSLD
| "chktgd"|"CHKTGD" -> CHKTGD
| "clrtag"|"CLRTAG" -> CLRTAG
| "cpy"|"CPY" -> CPY
| "cpytype"|"CPYTYPE" -> CPYTYPE
| "cpyvalue"|"CPYVALUE" -> CPYVALUE
| "cseal"|"CSEAL" -> CSEAL
| "cthi"|"CTHI" -> SC A.CTHI
| "gcflgs"|"GCFLGS" -> GC A.GCFLGS
| "gcperm"|"GCPERM" -> GC A.GCPERM
| "gcseal"|"GCSEAL" -> GC A.GCSEAL
| "gctag"|"GCTAG" -> GC A.GCTAG
| "gctype"|"GCTYPE" -> GC A.GCTYPE
| "gcvalue"|"GCVALUE" -> GC A.GCVALUE
| "ldct"|"LDCT" -> LDCT
| "scflgs"|"SCFLGS" -> SC A.SCFLGS
| "sctag"|"SCTAG" -> SC A.SCTAG
| "scvalue"|"SCVALUE" -> SC A.SCVALUE
| "seal"|"SEAL" -> SEAL
| "stct"|"STCT" -> STCT
| "unseal"|"UNSEAL" -> UNSEAL
(* Misc *)
| "csel"|"CSEL" -> CSEL
| "csinc"|"CSINC" -> CSINC
| "csinv"|"CSINV" -> CSINV
| "csneg"|"CSNEG" -> CSNEG
| "cset"|"CSET" -> CSET
(* Fences *)
| "dmb"|"DMB" -> TOK_DMB
| "dsb"|"DSB" -> TOK_DSB
| "isb"|"ISB" -> TOK_ISB
(* Fence Operands *)
| "sy"|"SY" -> TOK_SY
| "st"|"ST" -> TOK_ST
| "ld"|"LD" -> TOK_LD
| "osh"|"OSH" -> TOK_OSH
| "oshst"|"OSHST" -> TOK_OSHST
| "oshld"|"OSHLD" -> TOK_OSHLD
| "ish"|"ISH" -> TOK_ISH
| "ishst"|"ISHST" -> TOK_ISHST
| "ishld"|"ISHLD" -> TOK_ISHLD
| "nsh"|"NSH" -> TOK_NSH
| "nshst"|"NSHST" -> TOK_NSHST
| "nshld"|"NSHLD" -> TOK_NSHLD
(* inline barrel shift operands *)
| "msl" | "MSL" -> TOK_MSL
(* Cache maintenance *)
| "ic"|"IC" -> IC
| "dc"|"DC" -> DC
| "ialluis"|"IALLUIS" -> A.IC.(IC_OP { funct=I; typ=ALL; point=U; domain=IS; })
| "ivauis"|"IVAUIS" -> A.IC.(IC_OP { funct=I; typ=VA; point=U; domain=IS; })
| "iallu"|"IALLU" -> A.IC.(IC_OP { funct=I; typ=ALL; point=U; domain=NO; })
| "ivau"|"IVAU" -> IVAU
| "ivac"|"IVAC" -> A.DC.(DC_OP { funct=I; typ=VA; point=CO; })
| "cvac"|"CVAC" -> A.DC.(DC_OP { funct=C; typ=VA; point=CO; })
| "civac"|"CIVAC" -> A.DC.(DC_OP { funct=CI; typ=VA; point=CO; })
| "zvac"|"ZVAC" -> A.DC.(DC_OP { funct=Z; typ=VA; point=CO; })
| "iswc"|"ISWC" -> A.DC.(DC_OP { funct=I; typ=SW; point=CO; })
| "cswc"|"CSWC" -> A.DC.(DC_OP { funct=C; typ=SW; point=CO; })
| "ciswc"|"CISWC" -> A.DC.(DC_OP { funct=CI; typ=SW; point=CO; })
| "zswc"|"ZSWC" -> A.DC.(DC_OP { funct=Z; typ=SW; point=CO; })
| "cvau"|"CVAU" -> A.DC.(DC_OP { funct=C; typ=VA; point=U; })
| "civau"|"CIVAU" -> A.DC.(DC_OP { funct=CI; typ=VA; point=U; })
| "zvau"|"ZVAU" -> A.DC.(DC_OP { funct=Z; typ=VA; point=U; })
| "iswu"|"ISWU" -> A.DC.(DC_OP { funct=I; typ=SW; point=U; })
| "cswu"|"CSWU" -> A.DC.(DC_OP { funct=C; typ=SW; point=U; })
| "ciswu"|"CISWU" -> A.DC.(DC_OP { funct=CI; typ=SW; point=U; })
| "zswu"|"ZSWU" -> A.DC.(DC_OP { funct=Z; typ=SW; point=U; })
(* Idem, tlb *)
| "tlbi"|"TLBI"-> TLBI
(* Arguments, and there are many... *)
| "ipas2e1is"|"IPAS2E1IS" ->
    A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=IS; })
| "ipas2le1is"|"IPAS2LE1IS" ->
    A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=IS; })
| "ipas2e1"|"IPAS2E1" -> A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=No; })
| "ipas2le1"|"IPAS2LE1" -> A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=No; })
|  "vmalle1is"|"VMALLE1IS" ->
    A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=IS; })
|  "vmalle1"|"VMALLE1" ->
    A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=No; })
| "alle1is"|"ALLE1IS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=IS; })
| "alle2is"|"ALLE2IS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=IS; })
| "alle3is"|"ALLE3IS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=IS; })
| "alle1"|"ALLE1" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=No; })
| "alle2"|"ALLE2" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=No; })
| "alle3"|"ALLE3" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=No; })
| "vae1is"|"VAE1IS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=IS; })
| "vae2is"|"VAE2IS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=IS; })
| "vae3is"|"VAE3IS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=IS; })
| "vae1"|"VAE1" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=No; })
| "vae2"|"VAE2" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=No; })
| "vae3"|"VAE3" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=No; })
| "aside1is"|"ASIDE1IS" -> A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=IS; })
| "aside1"|"ASIDE1" -> A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=No; })
| "vaae1is"|"VAAE1IS" -> A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=IS; })
| "vaae1"|"VAAE1" -> A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=No; })
| "vale1is"|"VALE1IS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=IS; })
| "vale2is"|"VALE2IS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=IS; })
| "vale3is"|"VALE3IS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=IS; })
| "vale1"|"VALE1" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=No; })
| "vale2"|"VALE2" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=No; })
| "vale3"|"VALE3" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=No; })
| "vaale1is"|"VAALE1IS" -> A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=IS; })
| "vaale1"|"VAALE1" -> A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=No; })
| "vmalls12e1is"|"VMALLS12E1IS" ->
    A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=IS; })
| "vmalls12e1"|"VMALLS12E1" ->
    A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=No; })
(* System registers *)
| "mrs"|"MRS" -> MRS
| "msr"|"MSR" -> MSR
| _ ->
    begin match A.parse_wreg name with
    | Some r -> ARCH_WREG r
    | None ->
        begin match A.parse_xreg name with
        | Some r -> ARCH_XREG r
        | None ->
            begin match parse_creg name with
            | Some r -> ARCH_CREG r
            | None ->
                begin match A.parse_vreg name with
                | Some r -> ARCH_VREG r
                | None ->
                    begin match A.parse_simd_reg name with
                    | Some r ->
                        begin match (Char.uppercase_ascii name.[0]) with
                        | 'B' -> ARCH_BREG r
                        | 'H' -> ARCH_HREG r
                        | 'S' -> ARCH_SREG r
                        | 'D' -> ARCH_DREG r
                        | 'Q' -> ARCH_QREG r
                        | _ -> assert false
                        end
                    | None ->
                        begin match A.parse_sysreg name with
                        | Some r -> SYSREG r
                        | None -> NAME name
                        end
                    end
                end
            end
        end
    end
}
let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '#'? (('-'|'+') ? num as x) { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| 'P' (num as x) ".F"
    { PROCFH (int_of_string x) }
| ['w''W']'%' (name as name) { SYMB_WREG name }
| ['x''X']?'%' (name as name) { SYMB_XREG name }
| ['c''C']?'%' (name as name) { SYMB_CREG name }
| '[' (num as i) ']' { INDEX (int_of_string i) }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '{' { LCRL }
| '}' { RCRL }
| '[' { LBRK }
| ']' { RBRK }
| '(' { LPAR }
| ')' { RPAR }
| ':' { COLON }
| '.' { DOT }
| "scopes"  { SCOPES  }
| "levels"  { LEVELS  }
| "regions" { REGIONS }
| '&' (name as x) { META x }
| "codevar:" (name as x) { CODEVAR x }
| name as x  { check_name x }
| eof { EOF }
| ""  { error "AArch64 lexer" lexbuf }

{
let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf)
   end ;
   tok
end
}
