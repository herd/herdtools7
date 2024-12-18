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
| "b.eq" | "B.EQ" | "b.none" | "B.NONE" -> BEQ
| "b.ne" | "B.NE" | "b.any" | "B.ANY" -> BNE
| "b.ge" | "B.GE" | "b.tcont" | "B.TCONT" -> BGE
| "b.gt" | "B.GT" -> BGT
| "b.le" | "B.LE" -> BLE
| "b.lt" | "B.LT" | "b.tstop" | "B.TSTOP" -> BLT
| "b.cs" | "B.CS" | "b.nlast" | "B.NLAST" -> BCS
| "b.cc" | "B.CC" | "b.last" | "B.LAST" -> BCC
| "b.mi" | "B.MI" | "b.first" | "B.FIRST" -> BMI
| "b.pl" | "B.PL" | "b.nfirst" | "B.NFIRST" -> BPL
| "b.vs" | "B.VS" -> BVS
| "b.vc" | "B.VC" -> BVC
| "b.hi" | "B.HI" -> BHI
| "b.ls" | "B.LS" | "b.plast" | "B.PLAST" -> BLS
| "b.al" | "B.AL" -> BAL
| "cbz"  | "CBZ" -> CBZ
| "cbnz"  | "CBNZ" -> CBNZ
| "tbnz" | "TBNZ" -> TBNZ
| "tbz" | "TBZ" -> TBZ
(* Memory *)
| "ldr"|"LDR" -> LDR
| "ldrsw"|"LDRSW" -> LDRSW
| "ldur"|"LDUR" -> LDUR
| "ldapur"|"LDAPUR" -> LDAPUR
| "ldp"|"LDP" -> LDP
| "ldpsw"|"LDPSW" -> LDPSW
| "ldnp"|"LDNP" -> LDNP
| "ldiapp"|"LDIAPP" -> LDIAPP
| "stp"|"STP" -> STP
| "stnp"|"STNP" -> STNP
| "stilp"|"STILP" -> STILP
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
| "ldap1" | "LDAP1" -> LDAP1
| "ld1r" | "LD1R" -> LD1R
| "ld2" | "LD2" -> LD2
| "ld2r" | "LD2R" -> LD2R
| "ld3" | "LD3" -> LD3
| "ld3r" | "LD3R" -> LD3R
| "ld4" | "LD4" -> LD4
| "ld4r" | "LD4R" -> LD4R
| "stur" | "STUR" -> STUR
| "stlur" | "STLUR" -> STLUR
| "stl1" | "STL1" -> STL1
| "st1" | "ST1" -> ST1
| "st2" | "ST2" -> ST2
| "st3" | "ST3" -> ST3
| "st4" | "ST4" -> ST4
| "addv" | "ADDV" -> ADDV
| "dup" | "DUP" -> DUP
| "movi" | "MOVI" -> MOVI
| "mvn" | "MVN" -> MVN
| "fmov" | "FMOV" -> FMOV
(* Pointer Authentication Code *)
| "PACIA" | "pacia" -> PACIA
| "PACIA1716" | "pacia1716" -> PACIA1716
| "PACIAZ" | "paciaz" -> PACIAZ
| "PACIZA" | "paciza" -> PACIZA
| "PACIASP" | "paciasp" -> PACIASP
| "PACIB" | "pacib" -> PACIB
| "PACIB1716" | "pacib1716" -> PACIB1716
| "PACIBZ" | "pacibz" -> PACIBZ
| "PACIZB" | "pacizb" -> PACIZB
| "PACIBSP" | "pacibsp" -> PACIBSP
| "PACDA" | "pacda" -> PACDA
| "PACDZA" | "pacdza" -> PACDZA
| "PACDB" | "pacdb" -> PACDB
| "PACDZB" | "pacdzb" -> PACDZB
| "AUTIA" | "autia" -> AUTIA
| "AUTIA1716" | "autia1716" -> AUTIA1716
| "AUTIAZ" | "autiaz" -> AUTIAZ
| "AUTIZA" | "autiza" -> AUTIZA
| "AUTIASP" | "autiasp" -> AUTIASP
| "AUTIB" | "autib" -> AUTIB
| "AUTIB1716" | "autib1716" -> AUTIB1716
| "AUTIBZ" | "autibz" -> AUTIBZ
| "AUTIZB" | "autizb" -> AUTIZB
| "AUTIBSP" | "autibsp" -> AUTIBSP
| "AUTDA" | "autda" -> AUTDA
| "AUTDZA" | "autdza" -> AUTDZA
| "AUTDB" | "autdb" -> AUTDB
| "AUTDZB" | "autdzb" -> AUTDZB
| "XPACI" | "xpaci" -> XPACI
| "XPACD" | "xpacd" -> XPACD
(* Scalabel Vector Extension *)
| "whilelt" | "WHILELT" -> WHILELT
| "whilele" | "WHILELE" -> WHILELE
| "whilelo" | "WHILELO" -> WHILELO
| "whilels" | "WHILELS" -> WHILELS
| "uaddv" | "UADDV" -> UADDV
| "ld1b" | "LD1B" -> LD1B
| "ld1h" | "LD1H" -> LD1H
| "ld1w" | "LD1W" -> LD1W
| "ld1d" | "LD1D" -> LD1D
| "ld1q" | "LD1Q" -> LD1Q
| "ld2b" | "LD2B" -> LD2B
| "ld2h" | "LD2H" -> LD2H
| "ld2w" | "LD2W" -> LD2W
| "ld2d" | "LD2D" -> LD2D
| "ld3b" | "LD3B" -> LD3B
| "ld3h" | "LD3H" -> LD3H
| "ld3w" | "LD3W" -> LD3W
| "ld3d" | "LD3D" -> LD3D
| "ld4b" | "LD4B" -> LD4B
| "ld4h" | "LD4H" -> LD4H
| "ld4w" | "LD4W" -> LD4W
| "ld4d" | "LD4D" -> LD4D
| "st1b" | "ST1B" -> ST1B
| "st1h" | "ST1H" -> ST1H
| "st1w" | "ST1W" -> ST1W
| "st1d" | "ST1D" -> ST1D
| "st1q" | "ST1Q" -> ST1Q
| "st2b" | "ST2B" -> ST2B
| "st2h" | "ST2H" -> ST2H
| "st2w" | "ST2W" -> ST2W
| "st2d" | "ST2D" -> ST2D
| "st3b" | "ST3B" -> ST3B
| "st3h" | "ST3H" -> ST3H
| "st3w" | "ST3W" -> ST3W
| "st3d" | "ST3D" -> ST3D
| "st4b" | "ST4B" -> ST4B
| "st4h" | "ST4H" -> ST4H
| "st4w" | "ST4W" -> ST4W
| "st4d" | "ST4D" -> ST4D
| "index" | "INDEX" -> TOK_INDEX
| "rdvl" | "RDVL" -> RDVL
| "addvl" | "ADDVL" -> ADDVL
| "cntb" | "CNTB" -> let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD8)
| "cnth" | "CNTH" -> let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD16)
| "cntw" | "CNTW" -> let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD32)
| "cntd" | "CNTD" -> let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD64)
| "incb" | "INCB" -> let open AArch64Base in  CNT_INC_SVE (INC,VSIMD8)
| "inch" | "INCH" -> let open AArch64Base in  CNT_INC_SVE (INC,VSIMD16)
| "incw" | "INCW" -> let open AArch64Base in  CNT_INC_SVE (INC,VSIMD32)
| "incd" | "INCD" -> let open AArch64Base in  CNT_INC_SVE (INC,VSIMD64)
| "mul" | "MUL" -> TOK_MUL
| "vl" | "VL" -> TOK_VL
| "ptrue" | "PTRUE" -> PTRUE
| "pow2" | "POW2" -> TOK_POW2
| "vl1" | "VL1" -> TOK_VL1
| "vl2" | "VL2" -> TOK_VL2
| "vl3" | "VL3" -> TOK_VL3
| "vl4" | "VL4" -> TOK_VL4
| "vl5" | "VL5" -> TOK_VL5
| "vl6" | "VL6" -> TOK_VL6
| "vl7" | "VL7" -> TOK_VL7
| "vl8" | "VL8" -> TOK_VL8
| "vl16" | "VL16" -> TOK_VL16
| "vl32" | "VL32" -> TOK_VL32
| "vl64" | "VL64" -> TOK_VL64
| "vl128" | "VL128" -> TOK_VL128
| "vl256" | "VL256" -> TOK_VL256
| "mul4" | "MUL4" -> TOK_MUL4
| "mul3" | "MUL3" -> TOK_MUL3
| "all" | "ALL" -> TOK_ALL
| "movprfx" | "MOVPRFX" -> MOVPRFX
| "ctermeq"|"CTERMEQ" -> CTERM AArch64Base.CTERM.EQ
| "ctermne"|"CTERMNE" -> CTERM AArch64Base.CTERM.NE
(* Scalable Matrix Extension *)
| "addva" | "ADDVA" -> ADDA (AArch64Base.Vertical)
| "addha" | "ADDHA" -> ADDA (AArch64Base.Horizontal)
| "mova" | "MOVA" -> MOVA
| "smstart" | "SMSTART" -> SMSTART
| "smstop" | "SMSTOP" -> SMSTOP
| "sm" | "SM" -> TOK_SM
| "za" | "ZA" -> TOK_ZA
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
| "casp" | "CASP" -> CASP
| "caspa" | "CASPA" -> CASPA
| "caspl" | "CASPL" -> CASPL
| "caspal" | "CASPAL" -> CASPAL
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
| "ldadd"|"LDADD" -> LDOP (A.A_ADD,A.RMW_P)
| "ldadda"|"LDADDA" -> LDOP (A.A_ADD,A.RMW_A)
| "ldaddl"|"LDADDL" -> LDOP (A.A_ADD,A.RMW_L)
| "ldaddal"|"LDADDAL" -> LDOP (A.A_ADD,A.RMW_AL)
| "ldaddh"|"LDADDH" -> LDOPBH (A.H,A.A_ADD,A.RMW_P)
| "ldaddah"|"LDADDAH" -> LDOPBH (A.H,A.A_ADD,A.RMW_A)
| "ldaddlh"|"LDADDLH" -> LDOPBH (A.H,A.A_ADD,A.RMW_L)
| "ldaddalh"|"LDADDALH" -> LDOPBH (A.H,A.A_ADD,A.RMW_AL)
| "ldaddb"|"LDADDB" ->  LDOPBH (A.B,A.A_ADD,A.RMW_P)
| "ldaddab"|"LDADDAB" ->  LDOPBH (A.B,A.A_ADD,A.RMW_A)
| "ldaddlb"|"LDADDLB" ->  LDOPBH (A.B,A.A_ADD,A.RMW_L)
| "ldaddalb"|"LDADDALB" ->  LDOPBH (A.B,A.A_ADD,A.RMW_AL)
| "stadd"|"STADD" -> STOP (A.A_ADD,A.W_P)
| "staddl"|"STADDL" -> STOP (A.A_ADD,A.W_L)
| "staddh"|"STADDH" -> STOPBH (A.H,A.A_ADD,A.W_P)
| "staddlh"|"STADDLH" -> STOPBH (A.H,A.A_ADD,A.W_L)
| "staddb"|"STADDB" -> STOPBH (A.B,A.A_ADD,A.W_P)
| "staddlb"|"STADDLB" -> STOPBH (A.B,A.A_ADD,A.W_L)
(* Fetch and exclusive or, EOR *)
| "ldeor"|"LDEOR" -> LDOP (A.A_EOR,A.RMW_P)
| "ldeora"|"LDEORA" -> LDOP (A.A_EOR,A.RMW_A)
| "ldeorl"|"LDEORL" -> LDOP (A.A_EOR,A.RMW_L)
| "ldeoral"|"LDEORAL" -> LDOP (A.A_EOR,A.RMW_AL)
| "ldeorh"|"LDEORH" -> LDOPBH (A.H,A.A_EOR,A.RMW_P)
| "ldeorah"|"LDEORAH" -> LDOPBH (A.H,A.A_EOR,A.RMW_A)
| "ldeorlh"|"LDEORLH" -> LDOPBH (A.H,A.A_EOR,A.RMW_L)
| "ldeoralh"|"LDEORALH" -> LDOPBH (A.H,A.A_EOR,A.RMW_AL)
| "ldeorb"|"LDEORB" ->  LDOPBH (A.B,A.A_EOR,A.RMW_P)
| "ldeorab"|"LDEORAB" ->  LDOPBH (A.B,A.A_EOR,A.RMW_A)
| "ldeorlb"|"LDEORLB" ->  LDOPBH (A.B,A.A_EOR,A.RMW_L)
| "ldeoralb"|"LDEORALB" ->  LDOPBH (A.B,A.A_EOR,A.RMW_AL)
| "steor"|"STEOR" -> STOP (A.A_EOR,A.W_P)
| "steorl"|"STEORL" -> STOP (A.A_EOR,A.W_L)
| "steorh"|"STEORH" -> STOPBH (A.H,A.A_EOR,A.W_P)
| "steorlh"|"STEORLH" -> STOPBH (A.H,A.A_EOR,A.W_L)
| "steorb"|"STEORB" -> STOPBH (A.B,A.A_EOR,A.W_P)
| "steorlb"|"STEORLB" -> STOPBH (A.B,A.A_EOR,A.W_L)
(* Fetch and SET bit mask *)
| "ldset"|"LDSET" -> LDOP (A.A_SET,A.RMW_P)
| "ldseta"|"LDSETA" -> LDOP (A.A_SET,A.RMW_A)
| "ldsetl"|"LDSETL" -> LDOP (A.A_SET,A.RMW_L)
| "ldsetal"|"LDSETAL" -> LDOP (A.A_SET,A.RMW_AL)
| "ldseth"|"LDSETH" -> LDOPBH (A.H,A.A_SET,A.RMW_P)
| "ldsetah"|"LDSETAH" -> LDOPBH (A.H,A.A_SET,A.RMW_A)
| "ldsetlh"|"LDSETLH" -> LDOPBH (A.H,A.A_SET,A.RMW_L)
| "ldsetalh"|"LDSETALH" -> LDOPBH (A.H,A.A_SET,A.RMW_AL)
| "ldsetb"|"LDSETB" ->  LDOPBH (A.B,A.A_SET,A.RMW_P)
| "ldsetab"|"LDSETAB" ->  LDOPBH (A.B,A.A_SET,A.RMW_A)
| "ldsetlb"|"LDSETLB" ->  LDOPBH (A.B,A.A_SET,A.RMW_L)
| "ldsetalb"|"LDSETALB" ->  LDOPBH (A.B,A.A_SET,A.RMW_AL)
| "stset"|"STSET" -> STOP (A.A_SET,A.W_P)
| "stsetl"|"STSETL" -> STOP (A.A_SET,A.W_L)
| "stseth"|"STSETH" -> STOPBH (A.H,A.A_SET,A.W_P)
| "stsetlh"|"STSETLH" -> STOPBH (A.H,A.A_SET,A.W_L)
| "stsetb"|"STSETB" -> STOPBH (A.B,A.A_SET,A.W_P)
| "stsetlb"|"STSETLB" -> STOPBH (A.B,A.A_SET,A.W_L)
(* Fetch and clear bit mask *)
| "ldclr"|"LDCLR" -> LDOP (A.A_CLR,A.RMW_P)
| "ldclra"|"LDCLRA" -> LDOP (A.A_CLR,A.RMW_A)
| "ldclrl"|"LDCLRL" -> LDOP (A.A_CLR,A.RMW_L)
| "ldclral"|"LDCLRAL" -> LDOP (A.A_CLR,A.RMW_AL)
| "ldclrh"|"LDCLRH" -> LDOPBH (A.H,A.A_CLR,A.RMW_P)
| "ldclrah"|"LDCLRAH" -> LDOPBH (A.H,A.A_CLR,A.RMW_A)
| "ldclrlh"|"LDCLRLH" -> LDOPBH (A.H,A.A_CLR,A.RMW_L)
| "ldclralh"|"LDCLRALH" -> LDOPBH (A.H,A.A_CLR,A.RMW_AL)
| "ldclrb"|"LDCLRB" ->  LDOPBH (A.B,A.A_CLR,A.RMW_P)
| "ldclrab"|"LDCLRAB" ->  LDOPBH (A.B,A.A_CLR,A.RMW_A)
| "ldclrlb"|"LDCLRLB" ->  LDOPBH (A.B,A.A_CLR,A.RMW_L)
| "ldclralb"|"LDCLRALB" ->  LDOPBH (A.B,A.A_CLR,A.RMW_AL)
| "stclr"|"STCLR" -> STOP (A.A_CLR,A.W_P)
| "stclrl"|"STCLRL" -> STOP (A.A_CLR,A.W_L)
| "stclrh"|"STCLRH" -> STOPBH (A.H,A.A_CLR,A.W_P)
| "stclrlh"|"STCLRLH" -> STOPBH (A.H,A.A_CLR,A.W_L)
| "stclrb"|"STCLRB" -> STOPBH (A.B,A.A_CLR,A.W_P)
| "stclrlb"|"STCLRLB" -> STOPBH (A.B,A.A_CLR,A.W_L)
(* Fetch and signed max *)
| "ldsmax"|"LDSMAX" -> LDOP (A.A_SMAX,A.RMW_P)
| "ldsmaxa"|"LDSMAXA" -> LDOP (A.A_SMAX,A.RMW_A)
| "ldsmaxl"|"LDSMAXL" -> LDOP (A.A_SMAX,A.RMW_L)
| "ldsmaxal"|"LDSMAXAL" -> LDOP (A.A_SMAX,A.RMW_AL)
| "ldsmaxh"|"LDSMAXH" -> LDOPBH (A.H,A.A_SMAX,A.RMW_P)
| "ldsmaxah"|"LDSMAXAH" -> LDOPBH (A.H,A.A_SMAX,A.RMW_A)
| "ldsmaxlh"|"LDSMAXLH" -> LDOPBH (A.H,A.A_SMAX,A.RMW_L)
| "ldsmaxalh"|"LDSMAXALH" -> LDOPBH (A.H,A.A_SMAX,A.RMW_AL)
| "ldsmaxb"|"LDSMAXB" ->  LDOPBH (A.B,A.A_SMAX,A.RMW_P)
| "ldsmaxab"|"LDSMAXAB" ->  LDOPBH (A.B,A.A_SMAX,A.RMW_A)
| "ldsmaxlb"|"LDSMAXLB" ->  LDOPBH (A.B,A.A_SMAX,A.RMW_L)
| "ldsmaxalb"|"LDSMAXALB" ->  LDOPBH (A.B,A.A_SMAX,A.RMW_AL)
| "stsmax"|"STSMAX" -> STOP (A.A_SMAX,A.W_P)
| "stsmaxl"|"STSMAXL" -> STOP (A.A_SMAX,A.W_L)
| "stsmaxh"|"STSMAXH" -> STOPBH (A.H,A.A_SMAX,A.W_P)
| "stsmaxlh"|"STSMAXLH" -> STOPBH (A.H,A.A_SMAX,A.W_L)
| "stsmaxb"|"STSMAXB" -> STOPBH (A.B,A.A_SMAX,A.W_P)
| "stsmaxlb"|"STSMAXLB" -> STOPBH (A.B,A.A_SMAX,A.W_L)
(* Fetch and signed min *)
| "ldsmin"|"LDSMIN" -> LDOP (A.A_SMIN,A.RMW_P)
| "ldsmina"|"LDSMINA" -> LDOP (A.A_SMIN,A.RMW_A)
| "ldsminl"|"LDSMINL" -> LDOP (A.A_SMIN,A.RMW_L)
| "ldsminal"|"LDSMINAL" -> LDOP (A.A_SMIN,A.RMW_AL)
| "ldsminh"|"LDSMINH" -> LDOPBH (A.H,A.A_SMIN,A.RMW_P)
| "ldsminah"|"LDSMINAH" -> LDOPBH (A.H,A.A_SMIN,A.RMW_A)
| "ldsminlh"|"LDSMINLH" -> LDOPBH (A.H,A.A_SMIN,A.RMW_L)
| "ldsminalh"|"LDSMINALH" -> LDOPBH (A.H,A.A_SMIN,A.RMW_AL)
| "ldsminb"|"LDSMINB" ->  LDOPBH (A.B,A.A_SMIN,A.RMW_P)
| "ldsminab"|"LDSMINAB" ->  LDOPBH (A.B,A.A_SMIN,A.RMW_A)
| "ldsminlb"|"LDSMINLB" ->  LDOPBH (A.B,A.A_SMIN,A.RMW_L)
| "ldsminalb"|"LDSMINALB" ->  LDOPBH (A.B,A.A_SMIN,A.RMW_AL)
| "stsmin"|"STSMIN" -> STOP (A.A_SMIN,A.W_P)
| "stsminl"|"STSMINL" -> STOP (A.A_SMIN,A.W_L)
| "stsminh"|"STSMINH" -> STOPBH (A.H,A.A_SMIN,A.W_P)
| "stsminlh"|"STSMINLH" -> STOPBH (A.H,A.A_SMIN,A.W_L)
| "stsminb"|"STSMINB" -> STOPBH (A.B,A.A_SMIN,A.W_P)
| "stsminlb"|"STSMINLB" -> STOPBH (A.B,A.A_SMIN,A.W_L)
(* Fetch and unsigned max *)
| "ldumax"|"LDUMAX" -> LDOP (A.A_UMAX,A.RMW_P)
| "ldumaxa"|"LDUMAXA" -> LDOP (A.A_UMAX,A.RMW_A)
| "ldumaxl"|"LDUMAXL" -> LDOP (A.A_UMAX,A.RMW_L)
| "ldumaxal"|"LDUMAXAL" -> LDOP (A.A_UMAX,A.RMW_AL)
| "ldumaxh"|"LDUMAXH" -> LDOPBH (A.H,A.A_UMAX,A.RMW_P)
| "ldumaxah"|"LDUMAXAH" -> LDOPBH (A.H,A.A_UMAX,A.RMW_A)
| "ldumaxlh"|"LDUMAXLH" -> LDOPBH (A.H,A.A_UMAX,A.RMW_L)
| "ldumaxalh"|"LDUMAXALH" -> LDOPBH (A.H,A.A_UMAX,A.RMW_AL)
| "ldumaxb"|"LDUMAXB" ->  LDOPBH (A.B,A.A_UMAX,A.RMW_P)
| "ldumaxab"|"LDUMAXAB" ->  LDOPBH (A.B,A.A_UMAX,A.RMW_A)
| "ldumaxlb"|"LDUMAXLB" ->  LDOPBH (A.B,A.A_UMAX,A.RMW_L)
| "ldumaxalb"|"LDUMAXALB" ->  LDOPBH (A.B,A.A_UMAX,A.RMW_AL)
| "stumax"|"STUMAX" -> STOP (A.A_UMAX,A.W_P)
| "stumaxl"|"STUMAXL" -> STOP (A.A_UMAX,A.W_L)
| "stumaxh"|"STUMAXH" -> STOPBH (A.H,A.A_UMAX,A.W_P)
| "stumaxlh"|"STUMAXLH" -> STOPBH (A.H,A.A_UMAX,A.W_L)
| "stumaxb"|"STUMAXB" -> STOPBH (A.B,A.A_UMAX,A.W_P)
| "stumaxlb"|"STUMAXLB" -> STOPBH (A.B,A.A_UMAX,A.W_L)
(* Fetch and unsigned min *)
| "ldumin"|"LDUMIN" -> LDOP (A.A_UMIN,A.RMW_P)
| "ldumina"|"LDUMINA" -> LDOP (A.A_UMIN,A.RMW_A)
| "lduminl"|"LDUMINL" -> LDOP (A.A_UMIN,A.RMW_L)
| "lduminal"|"LDUMINAL" -> LDOP (A.A_UMIN,A.RMW_AL)
| "lduminh"|"LDUMINH" -> LDOPBH (A.H,A.A_UMIN,A.RMW_P)
| "lduminah"|"LDUMINAH" -> LDOPBH (A.H,A.A_UMIN,A.RMW_A)
| "lduminlh"|"LDUMINLH" -> LDOPBH (A.H,A.A_UMIN,A.RMW_L)
| "lduminalh"|"LDUMINALH" -> LDOPBH (A.H,A.A_UMIN,A.RMW_AL)
| "lduminb"|"LDUMINB" ->  LDOPBH (A.B,A.A_UMIN,A.RMW_P)
| "lduminab"|"LDUMINAB" ->  LDOPBH (A.B,A.A_UMIN,A.RMW_A)
| "lduminlb"|"LDUMINLB" ->  LDOPBH (A.B,A.A_UMIN,A.RMW_L)
| "lduminalb"|"LDUMINALB" ->  LDOPBH (A.B,A.A_UMIN,A.RMW_AL)
| "stumin"|"STUMIN" -> STOP (A.A_UMIN,A.W_P)
| "stuminl"|"STUMINL" -> STOP (A.A_UMIN,A.W_L)
| "stuminh"|"STUMINH" -> STOPBH (A.H,A.A_UMIN,A.W_P)
| "stuminlh"|"STUMINLH" -> STOPBH (A.H,A.A_UMIN,A.W_L)
| "stuminb"|"STUMINB" -> STOPBH (A.B,A.A_UMIN,A.W_P)
| "stuminlb"|"STUMINLB" -> STOPBH (A.B,A.A_UMIN,A.W_L)
(* SupervisorCall *)
| "svc"|"SVC" -> SVC
(* Undefined *)
| "udf"|"UDF" -> UDF
(* Memory Tagging *)
| "stg"|"STG" -> STG
| "st2g"|"ST2G" -> ST2G
| "stzg"|"STZG" -> STZG
| "stz2g"|"STZ2G" -> STZ2G
| "ldg"|"LDG" -> LDG
(* Operations *)
| "ubfm"|"UBFM" -> UBFM
| "sbfm"|"SBFM" -> SBFM
| "mov"|"MOV" -> MOV
| "movz"|"MOVZ" -> MOVZ
| "movn"|"MOVN" -> MOVN
| "movk"|"MOVK" -> MOVK
| "adr"|"ADR" -> ADR
| "rev16"|"REV16" -> REV16
| "rev32"|"REV32" -> REV32
| "rev64"|"REV64" -> REV64
| "rev"|"REV" -> REV
| "extr"|"EXTR" -> EXTR
| "rbit"|"RBIT" -> RBIT
| "abs"|"ABS" -> ABS
| "cmp"|"CMP" -> CMP
| "tst"|"TST" -> TST
(* Those operations are factorized *)
| "eor"|"EOR" -> OP A.EOR
| "eon"|"EON" -> OP A.EOR
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
| "ror" | "ROR" -> TOK_ROR
| "asrv" | "ASRV" -> TOK_ASRV
| "lslv" | "LSLV" -> TOK_LSLV
| "lsrv" | "LSRV" -> TOK_LSRV
| "rorv" | "RORV" -> TOK_RORV
(* extensions *)
| "uxtb"|"UXTB" -> TOK_UXTB
| "uxth"|"UXTH" -> TOK_UXTH
| "uxtw"|"UXTW" -> TOK_UXTW
| "uxtx"|"UXTX" -> TOK_UXTX
| "sxtb"|"SXTB" -> TOK_SXTB
| "sxth"|"SXTH" -> TOK_SXTH
| "sxtw"|"SXTW" -> TOK_SXTW
| "sxtx"|"SXTX" -> TOK_SXTX
(* SUB, SUBS, ADD have 128 bits semantics*)
| "sub"|"SUB" -> TOK_SUB
| "subs"|"SUBS" -> TOK_SUBS
| "add"|"ADD" -> TOK_ADD
| "adds"|"ADDS" -> TOK_ADDS
| "neg"|"NEG" -> TOK_NEG
| "negs"|"NEGS" -> TOK_NEGS
| "smaddl"|"SMADDL" -> MOPL AArch64Base.MOPLExt.(Signed,ADD)
| "smsubl"|"SMSUBL" -> MOPL AArch64Base.MOPLExt.(Signed,SUB)
| "umaddl"|"UMADDL" -> MOPL AArch64Base.MOPLExt.(Unsigned,ADD)
| "umsubl"|"UMSUBL" -> MOPL AArch64Base.MOPLExt.(Unsigned,SUB)
| "smull"|"SMULL" -> MOPLZ AArch64Base.MOPLExt.(Signed,ADD)
| "smnegl"|"SMNEGL" -> MOPLZ AArch64Base.MOPLExt.(Signed,SUB)
| "umull"|"UMULL" -> MOPLZ AArch64Base.MOPLExt.(Unsigned,ADD)
| "umnegl"|"UMNEGL" -> MOPLZ AArch64Base.MOPLExt.(Unsigned,SUB)
| "madd"|"MADD" -> MOP AArch64Base.MOPExt.(ADD)
| "msub"|"MSUB" -> MOP AArch64Base.MOPExt.(SUB)
| "mneg"|"MNEG" -> MOPZ AArch64Base.MOPExt.(SUB)
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
| "csetm"|"CSETM" -> CSETM
| "cinc"|"CINC" -> CINC
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
    A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=IS; nXS=false; })
| "ipas2le1is"|"IPAS2LE1IS" ->
    A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=IS; nXS=false; })
| "ipas2e1"|"IPAS2E1" -> A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=No; nXS=false; })
| "ipas2le1"|"IPAS2LE1" -> A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=No; nXS=false; })
|  "vmalle1is"|"VMALLE1IS" ->
    A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=IS; nXS=false; })
|  "vmalle1"|"VMALLE1" ->
    A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=No; nXS=false; })
| "alle1is"|"ALLE1IS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=IS; nXS=false; })
| "alle2is"|"ALLE2IS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=IS; nXS=false; })
| "alle3is"|"ALLE3IS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=IS; nXS=false; })
| "alle1"|"ALLE1" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=No; nXS=false; })
| "alle2"|"ALLE2" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=No; nXS=false; })
| "alle3"|"ALLE3" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=No; nXS=false; })
| "vae1is"|"VAE1IS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=IS; nXS=false; })
| "vae2is"|"VAE2IS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=IS; nXS=false; })
| "vae3is"|"VAE3IS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=IS; nXS=false; })
| "vae1"|"VAE1" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=No; nXS=false; })
| "vae2"|"VAE2" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=No; nXS=false; })
| "vae3"|"VAE3" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=No; nXS=false; })
| "aside1is"|"ASIDE1IS" -> A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=IS; nXS=false; })
| "aside1"|"ASIDE1" -> A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=No; nXS=false; })
| "vaae1is"|"VAAE1IS" -> A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=IS; nXS=false; })
| "vaae1"|"VAAE1" -> A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=No; nXS=false; })
| "vale1is"|"VALE1IS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=IS; nXS=false; })
| "vale2is"|"VALE2IS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=IS; nXS=false; })
| "vale3is"|"VALE3IS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=IS; nXS=false; })
| "vale1"|"VALE1" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=No; nXS=false; })
| "vale2"|"VALE2" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=No; nXS=false; })
| "vale3"|"VALE3" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=No; nXS=false; })
| "vaale1is"|"VAALE1IS" -> A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=IS; nXS=false; })
| "vaale1"|"VAALE1" -> A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=No; nXS=false; })
| "vmalls12e1is"|"VMALLS12E1IS" ->
    A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=IS; nXS=false; })
| "vmalls12e1"|"VMALLS12E1" ->
    A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=No; nXS=false; })
(* nXS version of the above *)
| "ipas2e1isnxs"|"IPAS2E1ISNXS" ->
    A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=IS; nXS=true; })
| "ipas2le1isnxs"|"IPAS2LE1ISNXS" ->
    A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=IS; nXS=true; })
| "ipas2e1nxs"|"IPAS2E1NXS" -> A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=No; nXS=true; })
| "ipas2le1nxs"|"IPAS2LE1NXS" -> A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=No; nXS=true; })
|  "vmalle1isnxs"|"VMALLE1ISNXS" ->
    A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=IS; nXS=true; })
|  "vmalle1nxs"|"VMALLE1NXS" ->
    A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=No; nXS=true; })
| "alle1isnxs"|"ALLE1ISNXS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=IS; nXS=true; })
| "alle2isnxs"|"ALLE2ISNXS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=IS; nXS=true; })
| "alle3isnxs"|"ALLE3ISNXS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=IS; nXS=true; })
| "alle1nxs"|"ALLE1NXS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=No; nXS=true; })
| "alle2nxs"|"ALLE2NXS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=No; nXS=true; })
| "alle3nxs"|"ALLE3NXS" -> A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=No; nXS=true; })
| "vae1isnxs"|"VAE1ISNXS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=IS; nXS=true; })
| "vae2isnxs"|"VAE2ISNXS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=IS; nXS=true; })
| "vae3isnxs"|"VAE3ISNXS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=IS; nXS=true; })
| "vae1nxs"|"VAE1NXS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=No; nXS=true; })
| "vae2nxs"|"VAE2NXS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=No; nXS=true; })
| "vae3nxs"|"VAE3NXS" -> A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=No; nXS=true; })
| "aside1isnxs"|"ASIDE1ISNXS" -> A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=IS; nXS=true; })
| "aside1nxs"|"ASIDE1NXS" -> A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=No; nXS=true; })
| "vaae1isnxs"|"VAAE1ISNXS" -> A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=IS; nXS=true; })
| "vaae1nxs"|"VAAE1NXS" -> A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=No; nXS=true; })
| "vale1isnxs"|"VALE1ISNXS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=IS; nXS=true; })
| "vale2isnxs"|"VALE2ISNXS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=IS; nXS=true; })
| "vale3isnxs"|"VALE3ISNXS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=IS; nXS=true; })
| "vale1nxs"|"VALE1NXS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=No; nXS=true; })
| "vale2nxs"|"VALE2NXS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=No; nXS=true; })
| "vale3nxs"|"VALE3NXS" -> A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=No; nXS=true; })
| "vaale1isnxs"|"VAALE1ISNXS" -> A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=IS; nXS=true; })
| "vaale1nxs"|"VAALE1NXS" -> A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=No; nXS=true; })
| "vmalls12e1isnxs"|"VMALLS12E1ISNXS" ->
    A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=IS; nXS=true; })
| "vmalls12e1nxs"|"VMALLS12E1NXS" ->
    A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=No; nXS=true; })
(* Address translation and its operands *)
(* Restricted to stage 1 only for EL1 and EL0; excludes <pan> and <ignore> fields *)
| "at"|"AT" -> AT
| "s1e0r" | "S1E0R" -> A.AT.(AT_OP {stages=S1; level=A.E0; rw=R; })
| "s1e0w" | "S1E0W" -> A.AT.(AT_OP {stages=S1; level=A.E0; rw=W; })
| "s1e1r" | "S1E1R" -> A.AT.(AT_OP {stages=S1; level=A.E1; rw=R; })
| "s1e1w" | "S1E1W" -> A.AT.(AT_OP {stages=S1; level=A.E1; rw=W; })
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
                        begin match A.parse_zreg name with
                        | Some r ->
                            begin match r with
                            | A.Zreg(_,8) -> ARCH_ZBREG r
                            | A.Zreg(_,16) -> ARCH_ZHREG r
                            | A.Zreg(_,32) -> ARCH_ZSREG r
                            | A.Zreg(_,64) -> ARCH_ZDREG r
                            | A.Zreg(_,128) -> ARCH_ZQREG r
                            | _ -> assert false
                            end
                        | None ->
                            begin match A.parse_pmreg name with
                            | Some r ->
                                begin match r with
                                | A.PMreg (_,A.Zero) -> ARCH_PMREG_Z r
                                | A.PMreg (_,A.Merge) -> ARCH_PMREG_M r
                                | _ -> assert false
                                end
                            | None ->
                                begin match A.parse_preg name with
                                | Some r -> ARCH_PREG r
                                | None ->
                                    begin match A.parse_zareg name with
                                    | Some r ->
                                        begin match r with
                                        | A.ZAreg(_,_,8) -> ARCH_ZABREG r
                                        | A.ZAreg(_,_,16) -> ARCH_ZAHREG r
                                        | A.ZAreg(_,_,32) -> ARCH_ZASREG r
                                        | A.ZAreg(_,_,64) -> ARCH_ZADREG r
                                        | A.ZAreg(_,_,128) -> ARCH_ZAQREG r
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
                end
            end
        end
    end
}
let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = (alpha|'_'|'.'|'$') (alpha|digit|'_'|'/'|'.'|'$')*
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
| '!' { BANG }
| "scopes"  { SCOPES  }
| "levels"  { LEVELS  }
| "regions" { REGIONS }
| '&' (name as x) { META x }
| "codevar:" (name as x) { CODEVAR x }
| ".p2align" { DOTP2ALIGN }
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
