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

open AArch64Parser
module A = AArch64Base

(* This is intentionally a hash table instead of a large string pattern match.
 * A large string ppatern match is known to cause issues with some versions of
 * js_of_ocaml. *)
let keyword_list = [
  "nop", NOP;
  (* Hints are NOPS in AArch64 *)
  "hint", HINT;
  (* Halt instructions are used by Debug mode, not needed here - NOP *)
  "hlt", HLT;
  (* Event Register *)
  "wfe", WFE;
  "sev", SEV;
  "sevl", SEVL;
  (* Branch *)
  "b", TOK_B;
  "br", BR;
  "bl", BL;
  "blr", BLR;
  "ret", RET;
  "eret", ERET;
  "ne", TOK_NE;
  "eq", TOK_EQ;
  "ge", TOK_GE;
  "gt", TOK_GT;
  "le", TOK_LE;
  "lt", TOK_LT;
  "cs", TOK_CS;
  "cc", TOK_CC;
  "mi", TOK_MI;
  "pl", TOK_PL;
  "vs", TOK_VS;
  "vc", TOK_VC;
  "hi", TOK_HI;
  "ls", TOK_LS;
  "al", TOK_AL;
  "b.eq", BEQ;
  "b.none", BEQ;
  "b.ne", BNE;
  "b.any", BNE;
  "b.ge", BGE;
  "b.tcont", BGE;
  "b.gt", BGT;
  "b.le", BLE;
  "b.lt", BLT;
  "b.tstop", BLT;
  "b.cs", BCS;
  "b.nlast", BCS;
  "b.cc", BCC;
  "b.last", BCC;
  "b.mi", BMI;
  "b.first", BMI;
  "b.pl", BPL;
  "b.nfirst", BPL;
  "b.vs", BVS;
  "b.vc", BVC;
  "b.hi", BHI;
  "b.ls", BLS;
  "b.plast", BLS;
  "b.al", BAL;
  "cbz", CBZ;
  "cbnz", CBNZ;
  "tbnz", TBNZ;
  "tbz", TBZ;
  (* Memory *)
  "ldr", LDR;
  "ldrsw", LDRSW;
  "ldur", LDUR;
  "ldapur", LDAPUR;
  "ldp", LDP;
  "ldpsw", LDPSW;
  "ldnp", LDNP;
  "ldiapp", LDIAPP;
  "ldap", LDAP;
  "stp", STP;
  "stnp", STNP;
  "stilp", STILP;
  "stlp", STLP;
  "ldrb", LDRB;
  "ldrh", LDRH;
  "ldrsb", LDRSB;
  "ldrsh", LDRSH;
  "ldar", LDAR;
  "ldarb", LDARB;
  "ldarh", LDARH;
  "ldapr", LDAPR;
  "ldaprb", LDAPRB;
  "ldaprh", LDAPRH;
  "ldxr", LDXR;
  "ldxrb", LDXRB;
  "ldxrh", LDXRH;
  "ldaxr", LDAXR;
  "ldaxrb", LDAXRB;
  "ldaxrh", LDAXRH;
  "ldxp", LDXP;
  "ldaxp", LDAXP;
  "str", STR;
  "stlr", STLR;
  "stxr", STXR;
  "stlxr", STLXR;
  "strb", STRB;
  "strh", STRH;
  "stlrb", STLRB;
  "stlrh", STLRH;
  "stxrb", STXRB;
  "stlxrb", STLXRB;
  "stxrh", STXRH;
  "stlxrh", STLXRH;
  "stxp", STXP;
  "stlxp", STLXP;
  (* Neon Extension Memory *)
  "ld1", LD1;
  "ldap1", LDAP1;
  "ld1r", LD1R;
  "ld2", LD2;
  "ld2r", LD2R;
  "ld3", LD3;
  "ld3r", LD3R;
  "ld4", LD4;
  "ld4r", LD4R;
  "stur", STUR;
  "stlur", STLUR;
  "stl1", STL1;
  "st1", ST1;
  "st2", ST2;
  "st3", ST3;
  "st4", ST4;
  "addv", ADDV;
  "dup", DUP;
  "movi", MOVI;
  "mvn", MVN;
  "fmov", FMOV;
  (* Pointer Authentication Code *)
  "pacia", PACIA;
  "pacia1716", PACIA1716;
  "paciaz", PACIAZ;
  "paciza", PACIZA;
  "paciasp", PACIASP;
  "pacib", PACIB;
  "pacib1716", PACIB1716;
  "pacibz", PACIBZ;
  "pacizb", PACIZB;
  "pacibsp", PACIBSP;
  "pacda", PACDA;
  "pacdza", PACDZA;
  "pacdb", PACDB;
  "pacdzb", PACDZB;
  "autia", AUTIA;
  "autia1716", AUTIA1716;
  "autiaz", AUTIAZ;
  "autiza", AUTIZA;
  "autiasp", AUTIASP;
  "autib", AUTIB;
  "autib1716", AUTIB1716;
  "autibz", AUTIBZ;
  "autizb", AUTIZB;
  "autibsp", AUTIBSP;
  "autda", AUTDA;
  "autdza", AUTDZA;
  "autdb", AUTDB;
  "autdzb", AUTDZB;
  "xpaci", XPACI;
  "xpacd", XPACD;
  (* Scalabel Vector Extension *)
  "whilelt", WHILELT;
  "whilele", WHILELE;
  "whilelo", WHILELO;
  "whilels", WHILELS;
  "uaddv", UADDV;
  "ld1b", LD1B;
  "ld1h", LD1H;
  "ld1w", LD1W;
  "ld1d", LD1D;
  "ld1q", LD1Q;
  "ld2b", LD2B;
  "ld2h", LD2H;
  "ld2w", LD2W;
  "ld2d", LD2D;
  "ld3b", LD3B;
  "ld3h", LD3H;
  "ld3w", LD3W;
  "ld3d", LD3D;
  "ld4b", LD4B;
  "ld4h", LD4H;
  "ld4w", LD4W;
  "ld4d", LD4D;
  "st1b", ST1B;
  "st1h", ST1H;
  "st1w", ST1W;
  "st1d", ST1D;
  "st1q", ST1Q;
  "st2b", ST2B;
  "st2h", ST2H;
  "st2w", ST2W;
  "st2d", ST2D;
  "st3b", ST3B;
  "st3h", ST3H;
  "st3w", ST3W;
  "st3d", ST3D;
  "st4b", ST4B;
  "st4h", ST4H;
  "st4w", ST4W;
  "st4d", ST4D;
  "index", TOK_INDEX;
  "rdvl", RDVL;
  "addvl", ADDVL;
  "cntb", (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD8));
  "cnth", (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD16));
  "cntw", (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD32));
  "cntd", (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD64));
  "incb", (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD8));
  "inch", (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD16));
  "incw", (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD32));
  "incd", (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD64));
  "mul", TOK_MUL;
  "vl", TOK_VL;
  "ptrue", PTRUE;
  "pow2", TOK_POW2;
  "vl1", TOK_VL1;
  "vl2", TOK_VL2;
  "vl3", TOK_VL3;
  "vl4", TOK_VL4;
  "vl5", TOK_VL5;
  "vl6", TOK_VL6;
  "vl7", TOK_VL7;
  "vl8", TOK_VL8;
  "vl16", TOK_VL16;
  "vl32", TOK_VL32;
  "vl64", TOK_VL64;
  "vl128", TOK_VL128;
  "vl256", TOK_VL256;
  "mul4", TOK_MUL4;
  "mul3", TOK_MUL3;
  "all", TOK_ALL;
  "movprfx", MOVPRFX;
  "ctermeq", CTERM AArch64Base.CTERM.EQ;
  "ctermne", CTERM AArch64Base.CTERM.NE;
  (* Scalable Matrix Extension *)
  "addva", ADDA AArch64Base.Vertical;
  "addha", ADDA AArch64Base.Horizontal;
  "mova", MOVA;
  "smstart", SMSTART;
  "smstop", SMSTOP;
  "sm", TOK_SM;
  "za", TOK_ZA;
  (* Compare and swap *)
  "cas", CAS;
  "casa", CASA;
  "casl", CASL;
  "casal", CASAL;
  "cash", CASH;
  "casah", CASAH;
  "caslh", CASLH;
  "casalh", CASALH;
  "casb", CASB;
  "casab", CASAB;
  "caslb", CASLB;
  "casalb", CASALB;
  "casp", CASP;
  "caspa", CASPA;
  "caspl", CASPL;
  "caspal", CASPAL;
  (* Swap *)
  "swp", SWP;
  "swpa", SWPA;
  "swpl", SWPL;
  "swpal", SWPAL;
  "swph", SWPH;
  "swpah", SWPAH;
  "swplh", SWPLH;
  "swpalh", SWPALH;
  "swpb", SWPB;
  "swpab", SWPAB;
  "swplb", SWPLB;
  "swpalb", SWPALB;
  (* Fetch and ADD *)
  "ldadd", LDOP (A.A_ADD,A.RMW_P);
  "ldadda", LDOP (A.A_ADD,A.RMW_A);
  "ldaddl", LDOP (A.A_ADD,A.RMW_L);
  "ldaddal", LDOP (A.A_ADD,A.RMW_AL);
  "ldaddh", LDOPBH (A.H,A.A_ADD,A.RMW_P);
  "ldaddah", LDOPBH (A.H,A.A_ADD,A.RMW_A);
  "ldaddlh", LDOPBH (A.H,A.A_ADD,A.RMW_L);
  "ldaddalh", LDOPBH (A.H,A.A_ADD,A.RMW_AL);
  "ldaddb", LDOPBH (A.B,A.A_ADD,A.RMW_P);
  "ldaddab", LDOPBH (A.B,A.A_ADD,A.RMW_A);
  "ldaddlb", LDOPBH (A.B,A.A_ADD,A.RMW_L);
  "ldaddalb", LDOPBH (A.B,A.A_ADD,A.RMW_AL);
  "stadd", STOP (A.A_ADD,A.W_P);
  "staddl", STOP (A.A_ADD,A.W_L);
  "staddh", STOPBH (A.H,A.A_ADD,A.W_P);
  "staddlh", STOPBH (A.H,A.A_ADD,A.W_L);
  "staddb", STOPBH (A.B,A.A_ADD,A.W_P);
  "staddlb", STOPBH (A.B,A.A_ADD,A.W_L);
  (* Fetch and exclusive or, EOR *)
  "ldeor", LDOP (A.A_EOR,A.RMW_P);
  "ldeora", LDOP (A.A_EOR,A.RMW_A);
  "ldeorl", LDOP (A.A_EOR,A.RMW_L);
  "ldeoral", LDOP (A.A_EOR,A.RMW_AL);
  "ldeorh", LDOPBH (A.H,A.A_EOR,A.RMW_P);
  "ldeorah", LDOPBH (A.H,A.A_EOR,A.RMW_A);
  "ldeorlh", LDOPBH (A.H,A.A_EOR,A.RMW_L);
  "ldeoralh", LDOPBH (A.H,A.A_EOR,A.RMW_AL);
  "ldeorb", LDOPBH (A.B,A.A_EOR,A.RMW_P);
  "ldeorab", LDOPBH (A.B,A.A_EOR,A.RMW_A);
  "ldeorlb", LDOPBH (A.B,A.A_EOR,A.RMW_L);
  "ldeoralb", LDOPBH (A.B,A.A_EOR,A.RMW_AL);
  "steor", STOP (A.A_EOR,A.W_P);
  "steorl", STOP (A.A_EOR,A.W_L);
  "steorh", STOPBH (A.H,A.A_EOR,A.W_P);
  "steorlh", STOPBH (A.H,A.A_EOR,A.W_L);
  "steorb", STOPBH (A.B,A.A_EOR,A.W_P);
  "steorlb", STOPBH (A.B,A.A_EOR,A.W_L);
  (* Fetch and SET bit mask *)
  "ldset", LDOP (A.A_SET,A.RMW_P);
  "ldseta", LDOP (A.A_SET,A.RMW_A);
  "ldsetl", LDOP (A.A_SET,A.RMW_L);
  "ldsetal", LDOP (A.A_SET,A.RMW_AL);
  "ldseth", LDOPBH (A.H,A.A_SET,A.RMW_P);
  "ldsetah", LDOPBH (A.H,A.A_SET,A.RMW_A);
  "ldsetlh", LDOPBH (A.H,A.A_SET,A.RMW_L);
  "ldsetalh", LDOPBH (A.H,A.A_SET,A.RMW_AL);
  "ldsetb", LDOPBH (A.B,A.A_SET,A.RMW_P);
  "ldsetab", LDOPBH (A.B,A.A_SET,A.RMW_A);
  "ldsetlb", LDOPBH (A.B,A.A_SET,A.RMW_L);
  "ldsetalb", LDOPBH (A.B,A.A_SET,A.RMW_AL);
  "stset", STOP (A.A_SET,A.W_P);
  "stsetl", STOP (A.A_SET,A.W_L);
  "stseth", STOPBH (A.H,A.A_SET,A.W_P);
  "stsetlh", STOPBH (A.H,A.A_SET,A.W_L);
  "stsetb", STOPBH (A.B,A.A_SET,A.W_P);
  "stsetlb", STOPBH (A.B,A.A_SET,A.W_L);
  (* Fetch and clear bit mask *)
  "ldclr", LDOP (A.A_CLR,A.RMW_P);
  "ldclra", LDOP (A.A_CLR,A.RMW_A);
  "ldclrl", LDOP (A.A_CLR,A.RMW_L);
  "ldclral", LDOP (A.A_CLR,A.RMW_AL);
  "ldclrh", LDOPBH (A.H,A.A_CLR,A.RMW_P);
  "ldclrah", LDOPBH (A.H,A.A_CLR,A.RMW_A);
  "ldclrlh", LDOPBH (A.H,A.A_CLR,A.RMW_L);
  "ldclralh", LDOPBH (A.H,A.A_CLR,A.RMW_AL);
  "ldclrb", LDOPBH (A.B,A.A_CLR,A.RMW_P);
  "ldclrab", LDOPBH (A.B,A.A_CLR,A.RMW_A);
  "ldclrlb", LDOPBH (A.B,A.A_CLR,A.RMW_L);
  "ldclralb", LDOPBH (A.B,A.A_CLR,A.RMW_AL);
  "stclr", STOP (A.A_CLR,A.W_P);
  "stclrl", STOP (A.A_CLR,A.W_L);
  "stclrh", STOPBH (A.H,A.A_CLR,A.W_P);
  "stclrlh", STOPBH (A.H,A.A_CLR,A.W_L);
  "stclrb", STOPBH (A.B,A.A_CLR,A.W_P);
  "stclrlb", STOPBH (A.B,A.A_CLR,A.W_L);
  (* Fetch and signed max *)
  "ldsmax", LDOP (A.A_SMAX,A.RMW_P);
  "ldsmaxa", LDOP (A.A_SMAX,A.RMW_A);
  "ldsmaxl", LDOP (A.A_SMAX,A.RMW_L);
  "ldsmaxal", LDOP (A.A_SMAX,A.RMW_AL);
  "ldsmaxh", LDOPBH (A.H,A.A_SMAX,A.RMW_P);
  "ldsmaxah", LDOPBH (A.H,A.A_SMAX,A.RMW_A);
  "ldsmaxlh", LDOPBH (A.H,A.A_SMAX,A.RMW_L);
  "ldsmaxalh", LDOPBH (A.H,A.A_SMAX,A.RMW_AL);
  "ldsmaxb", LDOPBH (A.B,A.A_SMAX,A.RMW_P);
  "ldsmaxab", LDOPBH (A.B,A.A_SMAX,A.RMW_A);
  "ldsmaxlb", LDOPBH (A.B,A.A_SMAX,A.RMW_L);
  "ldsmaxalb", LDOPBH (A.B,A.A_SMAX,A.RMW_AL);
  "stsmax", STOP (A.A_SMAX,A.W_P);
  "stsmaxl", STOP (A.A_SMAX,A.W_L);
  "stsmaxh", STOPBH (A.H,A.A_SMAX,A.W_P);
  "stsmaxlh", STOPBH (A.H,A.A_SMAX,A.W_L);
  "stsmaxb", STOPBH (A.B,A.A_SMAX,A.W_P);
  "stsmaxlb", STOPBH (A.B,A.A_SMAX,A.W_L);
  (* Fetch and signed min *)
  "ldsmin", LDOP (A.A_SMIN,A.RMW_P);
  "ldsmina", LDOP (A.A_SMIN,A.RMW_A);
  "ldsminl", LDOP (A.A_SMIN,A.RMW_L);
  "ldsminal", LDOP (A.A_SMIN,A.RMW_AL);
  "ldsminh", LDOPBH (A.H,A.A_SMIN,A.RMW_P);
  "ldsminah", LDOPBH (A.H,A.A_SMIN,A.RMW_A);
  "ldsminlh", LDOPBH (A.H,A.A_SMIN,A.RMW_L);
  "ldsminalh", LDOPBH (A.H,A.A_SMIN,A.RMW_AL);
  "ldsminb", LDOPBH (A.B,A.A_SMIN,A.RMW_P);
  "ldsminab", LDOPBH (A.B,A.A_SMIN,A.RMW_A);
  "ldsminlb", LDOPBH (A.B,A.A_SMIN,A.RMW_L);
  "ldsminalb", LDOPBH (A.B,A.A_SMIN,A.RMW_AL);
  "stsmin", STOP (A.A_SMIN,A.W_P);
  "stsminl", STOP (A.A_SMIN,A.W_L);
  "stsminh", STOPBH (A.H,A.A_SMIN,A.W_P);
  "stsminlh", STOPBH (A.H,A.A_SMIN,A.W_L);
  "stsminb", STOPBH (A.B,A.A_SMIN,A.W_P);
  "stsminlb", STOPBH (A.B,A.A_SMIN,A.W_L);
  (* Fetch and unsigned max *)
  "ldumax", LDOP (A.A_UMAX,A.RMW_P);
  "ldumaxa", LDOP (A.A_UMAX,A.RMW_A);
  "ldumaxl", LDOP (A.A_UMAX,A.RMW_L);
  "ldumaxal", LDOP (A.A_UMAX,A.RMW_AL);
  "ldumaxh", LDOPBH (A.H,A.A_UMAX,A.RMW_P);
  "ldumaxah", LDOPBH (A.H,A.A_UMAX,A.RMW_A);
  "ldumaxlh", LDOPBH (A.H,A.A_UMAX,A.RMW_L);
  "ldumaxalh", LDOPBH (A.H,A.A_UMAX,A.RMW_AL);
  "ldumaxb", LDOPBH (A.B,A.A_UMAX,A.RMW_P);
  "ldumaxab", LDOPBH (A.B,A.A_UMAX,A.RMW_A);
  "ldumaxlb", LDOPBH (A.B,A.A_UMAX,A.RMW_L);
  "ldumaxalb", LDOPBH (A.B,A.A_UMAX,A.RMW_AL);
  "stumax", STOP (A.A_UMAX,A.W_P);
  "stumaxl", STOP (A.A_UMAX,A.W_L);
  "stumaxh", STOPBH (A.H,A.A_UMAX,A.W_P);
  "stumaxlh", STOPBH (A.H,A.A_UMAX,A.W_L);
  "stumaxb", STOPBH (A.B,A.A_UMAX,A.W_P);
  "stumaxlb", STOPBH (A.B,A.A_UMAX,A.W_L);
  (* Fetch and unsigned min *)
  "ldumin", LDOP (A.A_UMIN,A.RMW_P);
  "ldumina", LDOP (A.A_UMIN,A.RMW_A);
  "lduminl", LDOP (A.A_UMIN,A.RMW_L);
  "lduminal", LDOP (A.A_UMIN,A.RMW_AL);
  "lduminh", LDOPBH (A.H,A.A_UMIN,A.RMW_P);
  "lduminah", LDOPBH (A.H,A.A_UMIN,A.RMW_A);
  "lduminlh", LDOPBH (A.H,A.A_UMIN,A.RMW_L);
  "lduminalh", LDOPBH (A.H,A.A_UMIN,A.RMW_AL);
  "lduminb", LDOPBH (A.B,A.A_UMIN,A.RMW_P);
  "lduminab", LDOPBH (A.B,A.A_UMIN,A.RMW_A);
  "lduminlb", LDOPBH (A.B,A.A_UMIN,A.RMW_L);
  "lduminalb", LDOPBH (A.B,A.A_UMIN,A.RMW_AL);
  "stumin", STOP (A.A_UMIN,A.W_P);
  "stuminl", STOP (A.A_UMIN,A.W_L);
  "stuminh", STOPBH (A.H,A.A_UMIN,A.W_P);
  "stuminlh", STOPBH (A.H,A.A_UMIN,A.W_L);
  "stuminb", STOPBH (A.B,A.A_UMIN,A.W_P);
  "stuminlb", STOPBH (A.B,A.A_UMIN,A.W_L);
  (* SupervisorCall *)
  "svc", SVC;
  (* Undefined *)
  "udf", UDF;
  (* Memory Tagging *)
  "stg", STG;
  "st2g", ST2G;
  "stzg", STZG;
  "stz2g", STZ2G;
  "ldg", LDG;
  "irg", IRG;
  (* Operations *)
  "ubfm", UBFM;
  "sbfm", SBFM;
  "mov", MOV;
  "movz", MOVZ;
  "movn", MOVN;
  "movk", MOVK;
  "adr", ADR;
  "rev16", REV16;
  "rev32", REV32;
  "rev64", REV64;
  "rev", REV;
  "extr", EXTR;
  "rbit", RBIT;
  "abs", ABS;
  "cmp", CMP;
  "tst", TST;
  (* Those operations are factorized *)
  "eor", OP A.EOR;
  "eon", OP A.EON;
  "orr", OP A.ORR;
  "orn", OP A.ORN;
  "and", OP A.AND;
  "ands", OP A.ANDS;
  "bic", OP A.BIC;
  "bics", OP A.BICS;
  (* Some arithmetic instruction have their own lexeme,
      for parser to handle then in special ways *)
  (* Also used as barrel shift *)
  "asr", TOK_ASR;
  "lsl", TOK_LSL;
  "lsr", TOK_LSR;
  "ror", TOK_ROR;
  "asrv", TOK_ASRV;
  "lslv", TOK_LSLV;
  "lsrv", TOK_LSRV;
  "rorv", TOK_RORV;
  (* extensions *)
  "uxtb", TOK_UXTB;
  "uxth", TOK_UXTH;
  "uxtw", TOK_UXTW;
  "uxtx", TOK_UXTX;
  "sxtb", TOK_SXTB;
  "sxth", TOK_SXTH;
  "sxtw", TOK_SXTW;
  "sxtx", TOK_SXTX;
  (* SUB, SUBS, ADD have 128 bits semantics*)
  "sub", TOK_SUB;
  "subs", TOK_SUBS;
  "add", TOK_ADD;
  "adds", TOK_ADDS;
  "neg", TOK_NEG;
  "negs", TOK_NEGS;
  "smaddl", MOPL AArch64Base.MOPLExt.(Signed,ADD);
  "smsubl", MOPL AArch64Base.MOPLExt.(Signed,SUB);
  "umaddl", MOPL AArch64Base.MOPLExt.(Unsigned,ADD);
  "umsubl", MOPL AArch64Base.MOPLExt.(Unsigned,SUB);
  "smull", MOPLZ AArch64Base.MOPLExt.(Signed,ADD);
  "smnegl", MOPLZ AArch64Base.MOPLExt.(Signed,SUB);
  "umull", MOPLZ AArch64Base.MOPLExt.(Unsigned,ADD);
  "umnegl", MOPLZ AArch64Base.MOPLExt.(Unsigned,SUB);
  "madd", MOP AArch64Base.MOPExt.(ADD);
  "msub", MOP AArch64Base.MOPExt.(SUB);
  "mneg", MOPZ AArch64Base.MOPExt.(SUB);
  (* Morello *)
  "alignd", ALIGND;
  "alignu", ALIGNU;
  "build", BUILD;
  "chkeq", CHKEQ;
  "chksld", CHKSLD;
  "chktgd", CHKTGD;
  "clrtag", CLRTAG;
  "cpy", CPY;
  "cpytype", CPYTYPE;
  "cpyvalue", CPYVALUE;
  "cseal", CSEAL;
  "cthi", SC A.CTHI;
  "gcflgs", GC A.GCFLGS;
  "gcperm", GC A.GCPERM;
  "gcseal", GC A.GCSEAL;
  "gctag", GC A.GCTAG;
  "gctype", GC A.GCTYPE;
  "gcvalue", GC A.GCVALUE;
  "ldct", LDCT;
  "scflgs", SC A.SCFLGS;
  "sctag", SC A.SCTAG;
  "scvalue", SC A.SCVALUE;
  "seal", SEAL;
  "stct", STCT;
  "unseal", UNSEAL;
  (* Guarded Control Stack *)
  "gcspopm", GCSPOPM;
  "gcspushm", GCSPUSHM;
  "gcsstr", GCSSTR;
  "gcsss1", GCSSS1;
  "gcsss2", GCSSS2;
  (* Misc *)
  "csel", CSEL;
  "csinc", CSINC;
  "csinv", CSINV;
  "csneg", CSNEG;
  "cset", CSET;
  "csetm", CSETM;
  "cinc", CINC;
  (* Fences *)
  "dmb", TOK_DMB;
  "dsb", TOK_DSB;
  "isb", TOK_ISB;
  "gcsb", TOK_GCSB;
  (* Fence Operands *)
  "sy", TOK_SY;
  "st", TOK_ST;
  "ld", TOK_LD;
  "osh", TOK_OSH;
  "oshst", TOK_OSHST;
  "oshld", TOK_OSHLD;
  "ish", TOK_ISH;
  "ishst", TOK_ISHST;
  "ishld", TOK_ISHLD;
  "nsh", TOK_NSH;
  "nshst", TOK_NSHST;
  "nshld", TOK_NSHLD;
  "dsync", TOK_DSYNC;
  (* inline barrel shift operands *)
  "msl", TOK_MSL;
  (* Cache maintenance *)
  "ic", IC;
  "dc", DC;
  "ialluis", A.IC.(IC_OP { funct=I; typ=ALL; point=U; domain=IS; });
  "ivauis", A.IC.(IC_OP { funct=I; typ=VA; point=U; domain=IS; });
  "iallu", A.IC.(IC_OP { funct=I; typ=ALL; point=U; domain=NO; });
  "ivau", IVAU;
  "ivac", A.DC.(DC_OP { funct=I; typ=VA; point=CO; });
  "cvac", A.DC.(DC_OP { funct=C; typ=VA; point=CO; });
  "civac", A.DC.(DC_OP { funct=CI; typ=VA; point=CO; });
  "zvac", A.DC.(DC_OP { funct=Z; typ=VA; point=CO; });
  "iswc", A.DC.(DC_OP { funct=I; typ=SW; point=CO; });
  "cswc", A.DC.(DC_OP { funct=C; typ=SW; point=CO; });
  "ciswc", A.DC.(DC_OP { funct=CI; typ=SW; point=CO; });
  "zswc", A.DC.(DC_OP { funct=Z; typ=SW; point=CO; });
  "cvau", A.DC.(DC_OP { funct=C; typ=VA; point=U; });
  "civau", A.DC.(DC_OP { funct=CI; typ=VA; point=U; });
  "zvau", A.DC.(DC_OP { funct=Z; typ=VA; point=U; });
  "iswu", A.DC.(DC_OP { funct=I; typ=SW; point=U; });
  "cswu", A.DC.(DC_OP { funct=C; typ=SW; point=U; });
  "ciswu", A.DC.(DC_OP { funct=CI; typ=SW; point=U; });
  "zswu", A.DC.(DC_OP { funct=Z; typ=SW; point=U; });
  (* Idem, tlb *)
  "tlbi", TLBI;
  (* Arguments, and there are many... *)
  "ipas2e1is", A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=IS; nXS=false; });
  "ipas2le1is", A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=IS; nXS=false; });
  "ipas2e1", A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=No; nXS=false; });
  "ipas2le1", A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=No; nXS=false; });
  "vmalle1is", A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=IS; nXS=false; });
  "vmalle1", A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=No; nXS=false; });
  "alle1is", A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=IS; nXS=false; });
  "alle2is", A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=IS; nXS=false; });
  "alle3is", A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=IS; nXS=false; });
  "alle1", A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=No; nXS=false; });
  "alle2", A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=No; nXS=false; });
  "alle3", A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=No; nXS=false; });
  "vae1is", A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=IS; nXS=false; });
  "vae2is", A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=IS; nXS=false; });
  "vae3is", A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=IS; nXS=false; });
  "vae1", A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=No; nXS=false; });
  "vae2", A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=No; nXS=false; });
  "vae3", A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=No; nXS=false; });
  "aside1is", A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=IS; nXS=false; });
  "aside1", A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=No; nXS=false; });
  "vaae1is", A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=IS; nXS=false; });
  "vaae1", A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=No; nXS=false; });
  "vale1is", A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=IS; nXS=false; });
  "vale2is", A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=IS; nXS=false; });
  "vale3is", A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=IS; nXS=false; });
  "vale1", A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=No; nXS=false; });
  "vale2", A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=No; nXS=false; });
  "vale3", A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=No; nXS=false; });
  "vaale1is", A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=IS; nXS=false; });
  "vaale1", A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=No; nXS=false; });
  "vmalls12e1is", A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=IS; nXS=false; });
  "vmalls12e1", A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=No; nXS=false; });
  (* nXS version of the above *)
  "ipas2e1isnxs", A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=IS; nXS=true; });
  "ipas2le1isnxs", A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=IS; nXS=true; });
  "ipas2e1nxs", A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=No; nXS=true; });
  "ipas2le1nxs", A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=No; nXS=true; });
  "vmalle1isnxs", A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=IS; nXS=true; });
  "vmalle1nxs", A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=No; nXS=true; });
  "alle1isnxs", A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=IS; nXS=true; });
  "alle2isnxs", A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=IS; nXS=true; });
  "alle3isnxs", A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=IS; nXS=true; });
  "alle1nxs", A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=No; nXS=true; });
  "alle2nxs", A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=No; nXS=true; });
  "alle3nxs", A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=No; nXS=true; });
  "vae1isnxs", A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=IS; nXS=true; });
  "vae2isnxs", A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=IS; nXS=true; });
  "vae3isnxs", A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=IS; nXS=true; });
  "vae1nxs", A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=No; nXS=true; });
  "vae2nxs", A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=No; nXS=true; });
  "vae3nxs", A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=No; nXS=true; });
  "aside1isnxs", A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=IS; nXS=true; });
  "aside1nxs", A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=No; nXS=true; });
  "vaae1isnxs", A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=IS; nXS=true; });
  "vaae1nxs", A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=No; nXS=true; });
  "vale1isnxs", A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=IS; nXS=true; });
  "vale2isnxs", A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=IS; nXS=true; });
  "vale3isnxs", A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=IS; nXS=true; });
  "vale1nxs", A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=No; nXS=true; });
  "vale2nxs", A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=No; nXS=true; });
  "vale3nxs", A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=No; nXS=true; });
  "vaale1isnxs", A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=IS; nXS=true; });
  "vaale1nxs", A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=No; nXS=true; });
  "vmalls12e1isnxs", A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=IS; nXS=true; });
  "vmalls12e1nxs", A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=No; nXS=true; });
  (* Address translation and its operands *)
  (* Restricted to stage 1 only for EL1 and EL0; excludes <pan> and <ignore> fields *)
  "at", AT;
  "s1e0r", A.AT.(AT_OP {stages=S1; level=A.E0; rw=R; });
  "s1e0w", A.AT.(AT_OP {stages=S1; level=A.E0; rw=W; });
  "s1e1r", A.AT.(AT_OP {stages=S1; level=A.E1; rw=R; });
  "s1e1w", A.AT.(AT_OP {stages=S1; level=A.E1; rw=W; });
  (* System registers *)
  "mrs", MRS;
  "msr", MSR;
] 

let keyword_table =
  let tbl = Hashtbl.create 1024 in
  List.iter
    (fun (name, tok) ->
      if Hashtbl.mem tbl name then
        Warn.fatal
          "Internal error: Keyword \"%s\" appears twice in the keyword_list"
          name
      else begin
        Hashtbl.add tbl name tok;
        Hashtbl.add tbl (String.uppercase_ascii name) tok
        end
    )
    keyword_list ;
  tbl

module Make(O:Config) = struct
open Lexing
open LexMisc
module LU = LexUtils.Make(O)

let parse_creg = if O.is_morello then A.parse_creg  else fun _ -> None

let check_name name =
  if O.debug then Printf.eprintf "Check: '%s'\n" name ;
  match Hashtbl.find_opt keyword_table name with
  | Some tok -> tok
  | None ->
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
| ".pagealign" { DOTPAGEALIGN }
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
