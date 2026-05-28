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

let keyword_table =
  let tbl = Hashtbl.create 1024 in
  let add names tok = List.iter (fun name -> Hashtbl.add tbl name tok) names in
  add ["nop"; "NOP"] (NOP);
  (* Hints are NOPS in AArch64 *)
  add ["hint"; "HINT"] (HINT);
  (* Halt instructions are used by Debug mode, not needed here - NOP *)
  add ["hlt"; "HLT"] (HLT);
  (* Branch *)
  add ["b"; "B"] (TOK_B);
  add ["br"; "BR"] (BR);
  add ["bl"; "BL"] (BL);
  add ["blr"; "BLR"] (BLR);
  add ["ret"; "RET"] (RET);
  add ["eret"; "ERET"] (ERET);
  add ["ne"; "NE"] (TOK_NE);
  add ["eq"; "EQ"] (TOK_EQ);
  add ["ge"; "GE"] (TOK_GE);
  add ["gt"; "GT"] (TOK_GT);
  add ["le"; "LE"] (TOK_LE);
  add ["lt"; "LT"] (TOK_LT);
  add ["cs"; "CS"] (TOK_CS);
  add ["cc"; "CC"] (TOK_CC);
  add ["mi"; "MI"] (TOK_MI);
  add ["pl"; "PL"] (TOK_PL);
  add ["vs"; "VS"] (TOK_VS);
  add ["vc"; "VC"] (TOK_VC);
  add ["hi"; "HI"] (TOK_HI);
  add ["ls"; "LS"] (TOK_LS);
  add ["al"; "AL"] (TOK_AL);
  add ["b.eq"; "B.EQ"; "b.none"; "B.NONE"] (BEQ);
  add ["b.ne"; "B.NE"; "b.any"; "B.ANY"] (BNE);
  add ["b.ge"; "B.GE"; "b.tcont"; "B.TCONT"] (BGE);
  add ["b.gt"; "B.GT"] (BGT);
  add ["b.le"; "B.LE"] (BLE);
  add ["b.lt"; "B.LT"; "b.tstop"; "B.TSTOP"] (BLT);
  add ["b.cs"; "B.CS"; "b.nlast"; "B.NLAST"] (BCS);
  add ["b.cc"; "B.CC"; "b.last"; "B.LAST"] (BCC);
  add ["b.mi"; "B.MI"; "b.first"; "B.FIRST"] (BMI);
  add ["b.pl"; "B.PL"; "b.nfirst"; "B.NFIRST"] (BPL);
  add ["b.vs"; "B.VS"] (BVS);
  add ["b.vc"; "B.VC"] (BVC);
  add ["b.hi"; "B.HI"] (BHI);
  add ["b.ls"; "B.LS"; "b.plast"; "B.PLAST"] (BLS);
  add ["b.al"; "B.AL"] (BAL);
  add ["cbz"; "CBZ"] (CBZ);
  add ["cbnz"; "CBNZ"] (CBNZ);
  add ["tbnz"; "TBNZ"] (TBNZ);
  add ["tbz"; "TBZ"] (TBZ);
  (* Memory *)
  add ["ldr"; "LDR"] (LDR);
  add ["ldrsw"; "LDRSW"] (LDRSW);
  add ["ldur"; "LDUR"] (LDUR);
  add ["ldapur"; "LDAPUR"] (LDAPUR);
  add ["ldp"; "LDP"] (LDP);
  add ["ldpsw"; "LDPSW"] (LDPSW);
  add ["ldnp"; "LDNP"] (LDNP);
  add ["ldiapp"; "LDIAPP"] (LDIAPP);
  add ["stp"; "STP"] (STP);
  add ["stnp"; "STNP"] (STNP);
  add ["stilp"; "STILP"] (STILP);
  add ["ldrb"; "LDRB"] (LDRB);
  add ["ldrh"; "LDRH"] (LDRH);
  add ["ldrsb"; "LDRSB"] (LDRSB);
  add ["ldrsh"; "LDRSH"] (LDRSH);
  add ["ldar"; "LDAR"] (LDAR);
  add ["ldarb"; "LDARB"] (LDARB);
  add ["ldarh"; "LDARH"] (LDARH);
  add ["ldapr"; "LDAPR"] (LDAPR);
  add ["ldaprb"; "LDAPRB"] (LDAPRB);
  add ["ldaprh"; "LDAPRH"] (LDAPRH);
  add ["ldxr"; "LDXR"] (LDXR);
  add ["ldxrb"; "LDXRB"] (LDXRB);
  add ["ldxrh"; "LDXRH"] (LDXRH);
  add ["ldaxr"; "LDAXR"] (LDAXR);
  add ["ldaxrb"; "LDAXRB"] (LDAXRB);
  add ["ldaxrh"; "LDAXRH"] (LDAXRH);
  add ["ldxp"; "LDXP"] (LDXP);
  add ["ldaxp"; "LDAXP"] (LDAXP);
  add ["str"; "STR"] (STR);
  add ["stlr"; "STLR"] (STLR);
  add ["stxr"; "STXR"] (STXR);
  add ["stlxr"; "STLXR"] (STLXR);
  add ["strb"; "STRB"] (STRB);
  add ["strh"; "STRH"] (STRH);
  add ["stlrb"; "STLRB"] (STLRB);
  add ["stlrh"; "STLRH"] (STLRH);
  add ["stxrb"; "STXRB"] (STXRB);
  add ["stlxrb"; "STLXRB"] (STLXRB);
  add ["stxrh"; "STXRH"] (STXRH);
  add ["stlxrh"; "STLXRH"] (STLXRH);
  add ["stxp"; "STXP"] (STXP);
  add ["stlxp"; "STLXP"] (STLXP);
  (* Neon Extension Memory *)
  add ["ld1"; "LD1"] (LD1);
  add ["ldap1"; "LDAP1"] (LDAP1);
  add ["ld1r"; "LD1R"] (LD1R);
  add ["ld2"; "LD2"] (LD2);
  add ["ld2r"; "LD2R"] (LD2R);
  add ["ld3"; "LD3"] (LD3);
  add ["ld3r"; "LD3R"] (LD3R);
  add ["ld4"; "LD4"] (LD4);
  add ["ld4r"; "LD4R"] (LD4R);
  add ["stur"; "STUR"] (STUR);
  add ["stlur"; "STLUR"] (STLUR);
  add ["stl1"; "STL1"] (STL1);
  add ["st1"; "ST1"] (ST1);
  add ["st2"; "ST2"] (ST2);
  add ["st3"; "ST3"] (ST3);
  add ["st4"; "ST4"] (ST4);
  add ["addv"; "ADDV"] (ADDV);
  add ["dup"; "DUP"] (DUP);
  add ["movi"; "MOVI"] (MOVI);
  add ["mvn"; "MVN"] (MVN);
  add ["fmov"; "FMOV"] (FMOV);
  (* Pointer Authentication Code *)
  add ["PACIA"; "pacia"] (PACIA);
  add ["PACIA1716"; "pacia1716"] (PACIA1716);
  add ["PACIAZ"; "paciaz"] (PACIAZ);
  add ["PACIZA"; "paciza"] (PACIZA);
  add ["PACIASP"; "paciasp"] (PACIASP);
  add ["PACIB"; "pacib"] (PACIB);
  add ["PACIB1716"; "pacib1716"] (PACIB1716);
  add ["PACIBZ"; "pacibz"] (PACIBZ);
  add ["PACIZB"; "pacizb"] (PACIZB);
  add ["PACIBSP"; "pacibsp"] (PACIBSP);
  add ["PACDA"; "pacda"] (PACDA);
  add ["PACDZA"; "pacdza"] (PACDZA);
  add ["PACDB"; "pacdb"] (PACDB);
  add ["PACDZB"; "pacdzb"] (PACDZB);
  add ["AUTIA"; "autia"] (AUTIA);
  add ["AUTIA1716"; "autia1716"] (AUTIA1716);
  add ["AUTIAZ"; "autiaz"] (AUTIAZ);
  add ["AUTIZA"; "autiza"] (AUTIZA);
  add ["AUTIASP"; "autiasp"] (AUTIASP);
  add ["AUTIB"; "autib"] (AUTIB);
  add ["AUTIB1716"; "autib1716"] (AUTIB1716);
  add ["AUTIBZ"; "autibz"] (AUTIBZ);
  add ["AUTIZB"; "autizb"] (AUTIZB);
  add ["AUTIBSP"; "autibsp"] (AUTIBSP);
  add ["AUTDA"; "autda"] (AUTDA);
  add ["AUTDZA"; "autdza"] (AUTDZA);
  add ["AUTDB"; "autdb"] (AUTDB);
  add ["AUTDZB"; "autdzb"] (AUTDZB);
  add ["XPACI"; "xpaci"] (XPACI);
  add ["XPACD"; "xpacd"] (XPACD);
  (* Scalabel Vector Extension *)
  add ["whilelt"; "WHILELT"] (WHILELT);
  add ["whilele"; "WHILELE"] (WHILELE);
  add ["whilelo"; "WHILELO"] (WHILELO);
  add ["whilels"; "WHILELS"] (WHILELS);
  add ["uaddv"; "UADDV"] (UADDV);
  add ["ld1b"; "LD1B"] (LD1B);
  add ["ld1h"; "LD1H"] (LD1H);
  add ["ld1w"; "LD1W"] (LD1W);
  add ["ld1d"; "LD1D"] (LD1D);
  add ["ld1q"; "LD1Q"] (LD1Q);
  add ["ld2b"; "LD2B"] (LD2B);
  add ["ld2h"; "LD2H"] (LD2H);
  add ["ld2w"; "LD2W"] (LD2W);
  add ["ld2d"; "LD2D"] (LD2D);
  add ["ld3b"; "LD3B"] (LD3B);
  add ["ld3h"; "LD3H"] (LD3H);
  add ["ld3w"; "LD3W"] (LD3W);
  add ["ld3d"; "LD3D"] (LD3D);
  add ["ld4b"; "LD4B"] (LD4B);
  add ["ld4h"; "LD4H"] (LD4H);
  add ["ld4w"; "LD4W"] (LD4W);
  add ["ld4d"; "LD4D"] (LD4D);
  add ["st1b"; "ST1B"] (ST1B);
  add ["st1h"; "ST1H"] (ST1H);
  add ["st1w"; "ST1W"] (ST1W);
  add ["st1d"; "ST1D"] (ST1D);
  add ["st1q"; "ST1Q"] (ST1Q);
  add ["st2b"; "ST2B"] (ST2B);
  add ["st2h"; "ST2H"] (ST2H);
  add ["st2w"; "ST2W"] (ST2W);
  add ["st2d"; "ST2D"] (ST2D);
  add ["st3b"; "ST3B"] (ST3B);
  add ["st3h"; "ST3H"] (ST3H);
  add ["st3w"; "ST3W"] (ST3W);
  add ["st3d"; "ST3D"] (ST3D);
  add ["st4b"; "ST4B"] (ST4B);
  add ["st4h"; "ST4H"] (ST4H);
  add ["st4w"; "ST4W"] (ST4W);
  add ["st4d"; "ST4D"] (ST4D);
  add ["index"; "INDEX"] (TOK_INDEX);
  add ["rdvl"; "RDVL"] (RDVL);
  add ["addvl"; "ADDVL"] (ADDVL);
  add ["cntb"; "CNTB"] (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD8));
  add ["cnth"; "CNTH"] (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD16));
  add ["cntw"; "CNTW"] (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD32));
  add ["cntd"; "CNTD"] (let open AArch64Base in  CNT_INC_SVE (CNT,VSIMD64));
  add ["incb"; "INCB"] (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD8));
  add ["inch"; "INCH"] (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD16));
  add ["incw"; "INCW"] (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD32));
  add ["incd"; "INCD"] (let open AArch64Base in  CNT_INC_SVE (INC,VSIMD64));
  add ["mul"; "MUL"] (TOK_MUL);
  add ["vl"; "VL"] (TOK_VL);
  add ["ptrue"; "PTRUE"] (PTRUE);
  add ["pow2"; "POW2"] (TOK_POW2);
  add ["vl1"; "VL1"] (TOK_VL1);
  add ["vl2"; "VL2"] (TOK_VL2);
  add ["vl3"; "VL3"] (TOK_VL3);
  add ["vl4"; "VL4"] (TOK_VL4);
  add ["vl5"; "VL5"] (TOK_VL5);
  add ["vl6"; "VL6"] (TOK_VL6);
  add ["vl7"; "VL7"] (TOK_VL7);
  add ["vl8"; "VL8"] (TOK_VL8);
  add ["vl16"; "VL16"] (TOK_VL16);
  add ["vl32"; "VL32"] (TOK_VL32);
  add ["vl64"; "VL64"] (TOK_VL64);
  add ["vl128"; "VL128"] (TOK_VL128);
  add ["vl256"; "VL256"] (TOK_VL256);
  add ["mul4"; "MUL4"] (TOK_MUL4);
  add ["mul3"; "MUL3"] (TOK_MUL3);
  add ["all"; "ALL"] (TOK_ALL);
  add ["movprfx"; "MOVPRFX"] (MOVPRFX);
  add ["ctermeq"; "CTERMEQ"] (CTERM AArch64Base.CTERM.EQ);
  add ["ctermne"; "CTERMNE"] (CTERM AArch64Base.CTERM.NE);
  (* Scalable Matrix Extension *)
  add ["addva"; "ADDVA"] (ADDA (AArch64Base.Vertical));
  add ["addha"; "ADDHA"] (ADDA (AArch64Base.Horizontal));
  add ["mova"; "MOVA"] (MOVA);
  add ["smstart"; "SMSTART"] (SMSTART);
  add ["smstop"; "SMSTOP"] (SMSTOP);
  add ["sm"; "SM"] (TOK_SM);
  add ["za"; "ZA"] (TOK_ZA);
  (* Compare and swap *)
  add ["cas"; "CAS"] (CAS);
  add ["casa"; "CASA"] (CASA);
  add ["casl"; "CASL"] (CASL);
  add ["casal"; "CASAL"] (CASAL);
  add ["cash"; "CASH"] (CASH);
  add ["casah"; "CASAH"] (CASAH);
  add ["caslh"; "CASLH"] (CASLH);
  add ["casalh"; "CASALH"] (CASALH);
  add ["casb"; "CASB"] (CASB);
  add ["casab"; "CASAB"] (CASAB);
  add ["caslb"; "CASLB"] (CASLB);
  add ["casalb"; "CASALB"] (CASALB);
  add ["casp"; "CASP"] (CASP);
  add ["caspa"; "CASPA"] (CASPA);
  add ["caspl"; "CASPL"] (CASPL);
  add ["caspal"; "CASPAL"] (CASPAL);
  (* Swap *)
  add ["swp"; "SWP"] (SWP);
  add ["swpa"; "SWPA"] (SWPA);
  add ["swpl"; "SWPL"] (SWPL);
  add ["swpal"; "SWPAL"] (SWPAL);
  add ["swph"; "SWPH"] (SWPH);
  add ["swpah"; "SWPAH"] (SWPAH);
  add ["swplh"; "SWPLH"] (SWPLH);
  add ["swpalh"; "SWPALH"] (SWPALH);
  add ["swpb"; "SWPB"] (SWPB);
  add ["swpab"; "SWPAB"] (SWPAB);
  add ["swplb"; "SWPLB"] (SWPLB);
  add ["swpalb"; "SWPALB"] (SWPALB);
  (* Fetch and ADD *)
  add ["ldadd"; "LDADD"] (LDOP (A.A_ADD,A.RMW_P));
  add ["ldadda"; "LDADDA"] (LDOP (A.A_ADD,A.RMW_A));
  add ["ldaddl"; "LDADDL"] (LDOP (A.A_ADD,A.RMW_L));
  add ["ldaddal"; "LDADDAL"] (LDOP (A.A_ADD,A.RMW_AL));
  add ["ldaddh"; "LDADDH"] (LDOPBH (A.H,A.A_ADD,A.RMW_P));
  add ["ldaddah"; "LDADDAH"] (LDOPBH (A.H,A.A_ADD,A.RMW_A));
  add ["ldaddlh"; "LDADDLH"] (LDOPBH (A.H,A.A_ADD,A.RMW_L));
  add ["ldaddalh"; "LDADDALH"] (LDOPBH (A.H,A.A_ADD,A.RMW_AL));
  add ["ldaddb"; "LDADDB"] (LDOPBH (A.B,A.A_ADD,A.RMW_P));
  add ["ldaddab"; "LDADDAB"] (LDOPBH (A.B,A.A_ADD,A.RMW_A));
  add ["ldaddlb"; "LDADDLB"] (LDOPBH (A.B,A.A_ADD,A.RMW_L));
  add ["ldaddalb"; "LDADDALB"] (LDOPBH (A.B,A.A_ADD,A.RMW_AL));
  add ["stadd"; "STADD"] (STOP (A.A_ADD,A.W_P));
  add ["staddl"; "STADDL"] (STOP (A.A_ADD,A.W_L));
  add ["staddh"; "STADDH"] (STOPBH (A.H,A.A_ADD,A.W_P));
  add ["staddlh"; "STADDLH"] (STOPBH (A.H,A.A_ADD,A.W_L));
  add ["staddb"; "STADDB"] (STOPBH (A.B,A.A_ADD,A.W_P));
  add ["staddlb"; "STADDLB"] (STOPBH (A.B,A.A_ADD,A.W_L));
  (* Fetch and exclusive or, EOR *)
  add ["ldeor"; "LDEOR"] (LDOP (A.A_EOR,A.RMW_P));
  add ["ldeora"; "LDEORA"] (LDOP (A.A_EOR,A.RMW_A));
  add ["ldeorl"; "LDEORL"] (LDOP (A.A_EOR,A.RMW_L));
  add ["ldeoral"; "LDEORAL"] (LDOP (A.A_EOR,A.RMW_AL));
  add ["ldeorh"; "LDEORH"] (LDOPBH (A.H,A.A_EOR,A.RMW_P));
  add ["ldeorah"; "LDEORAH"] (LDOPBH (A.H,A.A_EOR,A.RMW_A));
  add ["ldeorlh"; "LDEORLH"] (LDOPBH (A.H,A.A_EOR,A.RMW_L));
  add ["ldeoralh"; "LDEORALH"] (LDOPBH (A.H,A.A_EOR,A.RMW_AL));
  add ["ldeorb"; "LDEORB"] (LDOPBH (A.B,A.A_EOR,A.RMW_P));
  add ["ldeorab"; "LDEORAB"] (LDOPBH (A.B,A.A_EOR,A.RMW_A));
  add ["ldeorlb"; "LDEORLB"] (LDOPBH (A.B,A.A_EOR,A.RMW_L));
  add ["ldeoralb"; "LDEORALB"] (LDOPBH (A.B,A.A_EOR,A.RMW_AL));
  add ["steor"; "STEOR"] (STOP (A.A_EOR,A.W_P));
  add ["steorl"; "STEORL"] (STOP (A.A_EOR,A.W_L));
  add ["steorh"; "STEORH"] (STOPBH (A.H,A.A_EOR,A.W_P));
  add ["steorlh"; "STEORLH"] (STOPBH (A.H,A.A_EOR,A.W_L));
  add ["steorb"; "STEORB"] (STOPBH (A.B,A.A_EOR,A.W_P));
  add ["steorlb"; "STEORLB"] (STOPBH (A.B,A.A_EOR,A.W_L));
  (* Fetch and SET bit mask *)
  add ["ldset"; "LDSET"] (LDOP (A.A_SET,A.RMW_P));
  add ["ldseta"; "LDSETA"] (LDOP (A.A_SET,A.RMW_A));
  add ["ldsetl"; "LDSETL"] (LDOP (A.A_SET,A.RMW_L));
  add ["ldsetal"; "LDSETAL"] (LDOP (A.A_SET,A.RMW_AL));
  add ["ldseth"; "LDSETH"] (LDOPBH (A.H,A.A_SET,A.RMW_P));
  add ["ldsetah"; "LDSETAH"] (LDOPBH (A.H,A.A_SET,A.RMW_A));
  add ["ldsetlh"; "LDSETLH"] (LDOPBH (A.H,A.A_SET,A.RMW_L));
  add ["ldsetalh"; "LDSETALH"] (LDOPBH (A.H,A.A_SET,A.RMW_AL));
  add ["ldsetb"; "LDSETB"] (LDOPBH (A.B,A.A_SET,A.RMW_P));
  add ["ldsetab"; "LDSETAB"] (LDOPBH (A.B,A.A_SET,A.RMW_A));
  add ["ldsetlb"; "LDSETLB"] (LDOPBH (A.B,A.A_SET,A.RMW_L));
  add ["ldsetalb"; "LDSETALB"] (LDOPBH (A.B,A.A_SET,A.RMW_AL));
  add ["stset"; "STSET"] (STOP (A.A_SET,A.W_P));
  add ["stsetl"; "STSETL"] (STOP (A.A_SET,A.W_L));
  add ["stseth"; "STSETH"] (STOPBH (A.H,A.A_SET,A.W_P));
  add ["stsetlh"; "STSETLH"] (STOPBH (A.H,A.A_SET,A.W_L));
  add ["stsetb"; "STSETB"] (STOPBH (A.B,A.A_SET,A.W_P));
  add ["stsetlb"; "STSETLB"] (STOPBH (A.B,A.A_SET,A.W_L));
  (* Fetch and clear bit mask *)
  add ["ldclr"; "LDCLR"] (LDOP (A.A_CLR,A.RMW_P));
  add ["ldclra"; "LDCLRA"] (LDOP (A.A_CLR,A.RMW_A));
  add ["ldclrl"; "LDCLRL"] (LDOP (A.A_CLR,A.RMW_L));
  add ["ldclral"; "LDCLRAL"] (LDOP (A.A_CLR,A.RMW_AL));
  add ["ldclrh"; "LDCLRH"] (LDOPBH (A.H,A.A_CLR,A.RMW_P));
  add ["ldclrah"; "LDCLRAH"] (LDOPBH (A.H,A.A_CLR,A.RMW_A));
  add ["ldclrlh"; "LDCLRLH"] (LDOPBH (A.H,A.A_CLR,A.RMW_L));
  add ["ldclralh"; "LDCLRALH"] (LDOPBH (A.H,A.A_CLR,A.RMW_AL));
  add ["ldclrb"; "LDCLRB"] (LDOPBH (A.B,A.A_CLR,A.RMW_P));
  add ["ldclrab"; "LDCLRAB"] (LDOPBH (A.B,A.A_CLR,A.RMW_A));
  add ["ldclrlb"; "LDCLRLB"] (LDOPBH (A.B,A.A_CLR,A.RMW_L));
  add ["ldclralb"; "LDCLRALB"] (LDOPBH (A.B,A.A_CLR,A.RMW_AL));
  add ["stclr"; "STCLR"] (STOP (A.A_CLR,A.W_P));
  add ["stclrl"; "STCLRL"] (STOP (A.A_CLR,A.W_L));
  add ["stclrh"; "STCLRH"] (STOPBH (A.H,A.A_CLR,A.W_P));
  add ["stclrlh"; "STCLRLH"] (STOPBH (A.H,A.A_CLR,A.W_L));
  add ["stclrb"; "STCLRB"] (STOPBH (A.B,A.A_CLR,A.W_P));
  add ["stclrlb"; "STCLRLB"] (STOPBH (A.B,A.A_CLR,A.W_L));
  (* Fetch and signed max *)
  add ["ldsmax"; "LDSMAX"] (LDOP (A.A_SMAX,A.RMW_P));
  add ["ldsmaxa"; "LDSMAXA"] (LDOP (A.A_SMAX,A.RMW_A));
  add ["ldsmaxl"; "LDSMAXL"] (LDOP (A.A_SMAX,A.RMW_L));
  add ["ldsmaxal"; "LDSMAXAL"] (LDOP (A.A_SMAX,A.RMW_AL));
  add ["ldsmaxh"; "LDSMAXH"] (LDOPBH (A.H,A.A_SMAX,A.RMW_P));
  add ["ldsmaxah"; "LDSMAXAH"] (LDOPBH (A.H,A.A_SMAX,A.RMW_A));
  add ["ldsmaxlh"; "LDSMAXLH"] (LDOPBH (A.H,A.A_SMAX,A.RMW_L));
  add ["ldsmaxalh"; "LDSMAXALH"] (LDOPBH (A.H,A.A_SMAX,A.RMW_AL));
  add ["ldsmaxb"; "LDSMAXB"] (LDOPBH (A.B,A.A_SMAX,A.RMW_P));
  add ["ldsmaxab"; "LDSMAXAB"] (LDOPBH (A.B,A.A_SMAX,A.RMW_A));
  add ["ldsmaxlb"; "LDSMAXLB"] (LDOPBH (A.B,A.A_SMAX,A.RMW_L));
  add ["ldsmaxalb"; "LDSMAXALB"] (LDOPBH (A.B,A.A_SMAX,A.RMW_AL));
  add ["stsmax"; "STSMAX"] (STOP (A.A_SMAX,A.W_P));
  add ["stsmaxl"; "STSMAXL"] (STOP (A.A_SMAX,A.W_L));
  add ["stsmaxh"; "STSMAXH"] (STOPBH (A.H,A.A_SMAX,A.W_P));
  add ["stsmaxlh"; "STSMAXLH"] (STOPBH (A.H,A.A_SMAX,A.W_L));
  add ["stsmaxb"; "STSMAXB"] (STOPBH (A.B,A.A_SMAX,A.W_P));
  add ["stsmaxlb"; "STSMAXLB"] (STOPBH (A.B,A.A_SMAX,A.W_L));
  (* Fetch and signed min *)
  add ["ldsmin"; "LDSMIN"] (LDOP (A.A_SMIN,A.RMW_P));
  add ["ldsmina"; "LDSMINA"] (LDOP (A.A_SMIN,A.RMW_A));
  add ["ldsminl"; "LDSMINL"] (LDOP (A.A_SMIN,A.RMW_L));
  add ["ldsminal"; "LDSMINAL"] (LDOP (A.A_SMIN,A.RMW_AL));
  add ["ldsminh"; "LDSMINH"] (LDOPBH (A.H,A.A_SMIN,A.RMW_P));
  add ["ldsminah"; "LDSMINAH"] (LDOPBH (A.H,A.A_SMIN,A.RMW_A));
  add ["ldsminlh"; "LDSMINLH"] (LDOPBH (A.H,A.A_SMIN,A.RMW_L));
  add ["ldsminalh"; "LDSMINALH"] (LDOPBH (A.H,A.A_SMIN,A.RMW_AL));
  add ["ldsminb"; "LDSMINB"] (LDOPBH (A.B,A.A_SMIN,A.RMW_P));
  add ["ldsminab"; "LDSMINAB"] (LDOPBH (A.B,A.A_SMIN,A.RMW_A));
  add ["ldsminlb"; "LDSMINLB"] (LDOPBH (A.B,A.A_SMIN,A.RMW_L));
  add ["ldsminalb"; "LDSMINALB"] (LDOPBH (A.B,A.A_SMIN,A.RMW_AL));
  add ["stsmin"; "STSMIN"] (STOP (A.A_SMIN,A.W_P));
  add ["stsminl"; "STSMINL"] (STOP (A.A_SMIN,A.W_L));
  add ["stsminh"; "STSMINH"] (STOPBH (A.H,A.A_SMIN,A.W_P));
  add ["stsminlh"; "STSMINLH"] (STOPBH (A.H,A.A_SMIN,A.W_L));
  add ["stsminb"; "STSMINB"] (STOPBH (A.B,A.A_SMIN,A.W_P));
  add ["stsminlb"; "STSMINLB"] (STOPBH (A.B,A.A_SMIN,A.W_L));
  (* Fetch and unsigned max *)
  add ["ldumax"; "LDUMAX"] (LDOP (A.A_UMAX,A.RMW_P));
  add ["ldumaxa"; "LDUMAXA"] (LDOP (A.A_UMAX,A.RMW_A));
  add ["ldumaxl"; "LDUMAXL"] (LDOP (A.A_UMAX,A.RMW_L));
  add ["ldumaxal"; "LDUMAXAL"] (LDOP (A.A_UMAX,A.RMW_AL));
  add ["ldumaxh"; "LDUMAXH"] (LDOPBH (A.H,A.A_UMAX,A.RMW_P));
  add ["ldumaxah"; "LDUMAXAH"] (LDOPBH (A.H,A.A_UMAX,A.RMW_A));
  add ["ldumaxlh"; "LDUMAXLH"] (LDOPBH (A.H,A.A_UMAX,A.RMW_L));
  add ["ldumaxalh"; "LDUMAXALH"] (LDOPBH (A.H,A.A_UMAX,A.RMW_AL));
  add ["ldumaxb"; "LDUMAXB"] (LDOPBH (A.B,A.A_UMAX,A.RMW_P));
  add ["ldumaxab"; "LDUMAXAB"] (LDOPBH (A.B,A.A_UMAX,A.RMW_A));
  add ["ldumaxlb"; "LDUMAXLB"] (LDOPBH (A.B,A.A_UMAX,A.RMW_L));
  add ["ldumaxalb"; "LDUMAXALB"] (LDOPBH (A.B,A.A_UMAX,A.RMW_AL));
  add ["stumax"; "STUMAX"] (STOP (A.A_UMAX,A.W_P));
  add ["stumaxl"; "STUMAXL"] (STOP (A.A_UMAX,A.W_L));
  add ["stumaxh"; "STUMAXH"] (STOPBH (A.H,A.A_UMAX,A.W_P));
  add ["stumaxlh"; "STUMAXLH"] (STOPBH (A.H,A.A_UMAX,A.W_L));
  add ["stumaxb"; "STUMAXB"] (STOPBH (A.B,A.A_UMAX,A.W_P));
  add ["stumaxlb"; "STUMAXLB"] (STOPBH (A.B,A.A_UMAX,A.W_L));
  (* Fetch and unsigned min *)
  add ["ldumin"; "LDUMIN"] (LDOP (A.A_UMIN,A.RMW_P));
  add ["ldumina"; "LDUMINA"] (LDOP (A.A_UMIN,A.RMW_A));
  add ["lduminl"; "LDUMINL"] (LDOP (A.A_UMIN,A.RMW_L));
  add ["lduminal"; "LDUMINAL"] (LDOP (A.A_UMIN,A.RMW_AL));
  add ["lduminh"; "LDUMINH"] (LDOPBH (A.H,A.A_UMIN,A.RMW_P));
  add ["lduminah"; "LDUMINAH"] (LDOPBH (A.H,A.A_UMIN,A.RMW_A));
  add ["lduminlh"; "LDUMINLH"] (LDOPBH (A.H,A.A_UMIN,A.RMW_L));
  add ["lduminalh"; "LDUMINALH"] (LDOPBH (A.H,A.A_UMIN,A.RMW_AL));
  add ["lduminb"; "LDUMINB"] (LDOPBH (A.B,A.A_UMIN,A.RMW_P));
  add ["lduminab"; "LDUMINAB"] (LDOPBH (A.B,A.A_UMIN,A.RMW_A));
  add ["lduminlb"; "LDUMINLB"] (LDOPBH (A.B,A.A_UMIN,A.RMW_L));
  add ["lduminalb"; "LDUMINALB"] (LDOPBH (A.B,A.A_UMIN,A.RMW_AL));
  add ["stumin"; "STUMIN"] (STOP (A.A_UMIN,A.W_P));
  add ["stuminl"; "STUMINL"] (STOP (A.A_UMIN,A.W_L));
  add ["stuminh"; "STUMINH"] (STOPBH (A.H,A.A_UMIN,A.W_P));
  add ["stuminlh"; "STUMINLH"] (STOPBH (A.H,A.A_UMIN,A.W_L));
  add ["stuminb"; "STUMINB"] (STOPBH (A.B,A.A_UMIN,A.W_P));
  add ["stuminlb"; "STUMINLB"] (STOPBH (A.B,A.A_UMIN,A.W_L));
  (* SupervisorCall *)
  add ["svc"; "SVC"] (SVC);
  (* Undefined *)
  add ["udf"; "UDF"] (UDF);
  (* Memory Tagging *)
  add ["stg"; "STG"] (STG);
  add ["st2g"; "ST2G"] (ST2G);
  add ["stzg"; "STZG"] (STZG);
  add ["stz2g"; "STZ2G"] (STZ2G);
  add ["ldg"; "LDG"] (LDG);
  add ["irg"; "IRG"] (IRG);
  (* Operations *)
  add ["ubfm"; "UBFM"] (UBFM);
  add ["sbfm"; "SBFM"] (SBFM);
  add ["mov"; "MOV"] (MOV);
  add ["movz"; "MOVZ"] (MOVZ);
  add ["movn"; "MOVN"] (MOVN);
  add ["movk"; "MOVK"] (MOVK);
  add ["adr"; "ADR"] (ADR);
  add ["rev16"; "REV16"] (REV16);
  add ["rev32"; "REV32"] (REV32);
  add ["rev64"; "REV64"] (REV64);
  add ["rev"; "REV"] (REV);
  add ["extr"; "EXTR"] (EXTR);
  add ["rbit"; "RBIT"] (RBIT);
  add ["abs"; "ABS"] (ABS);
  add ["cmp"; "CMP"] (CMP);
  add ["tst"; "TST"] (TST);
  (* Those operations are factorized *)
  add ["eor"; "EOR"] (OP A.EOR);
  add ["eon"; "EON"] (OP A.EOR);
  add ["orr"; "ORR"] (OP A.ORR);
  add ["orn"; "ORN"] (OP A.ORN);
  add ["and"; "AND"] (OP A.AND);
  add ["ands"; "ANDS"] (OP A.ANDS);
  add ["bic"; "BIC"] (OP A.BIC);
  add ["bics"; "BICS"] (OP A.BICS);
  (* Some arithmetic instruction have their own lexeme,
     for parser to handle then in special ways *)
  (* Also used as barrel shift *)
  add ["asr"; "ASR"] (TOK_ASR);
  add ["lsl"; "LSL"] (TOK_LSL);
  add ["lsr"; "LSR"] (TOK_LSR);
  add ["ror"; "ROR"] (TOK_ROR);
  add ["asrv"; "ASRV"] (TOK_ASRV);
  add ["lslv"; "LSLV"] (TOK_LSLV);
  add ["lsrv"; "LSRV"] (TOK_LSRV);
  add ["rorv"; "RORV"] (TOK_RORV);
  (* extensions *)
  add ["uxtb"; "UXTB"] (TOK_UXTB);
  add ["uxth"; "UXTH"] (TOK_UXTH);
  add ["uxtw"; "UXTW"] (TOK_UXTW);
  add ["uxtx"; "UXTX"] (TOK_UXTX);
  add ["sxtb"; "SXTB"] (TOK_SXTB);
  add ["sxth"; "SXTH"] (TOK_SXTH);
  add ["sxtw"; "SXTW"] (TOK_SXTW);
  add ["sxtx"; "SXTX"] (TOK_SXTX);
  (* SUB, SUBS, ADD have 128 bits semantics*)
  add ["sub"; "SUB"] (TOK_SUB);
  add ["subs"; "SUBS"] (TOK_SUBS);
  add ["add"; "ADD"] (TOK_ADD);
  add ["adds"; "ADDS"] (TOK_ADDS);
  add ["neg"; "NEG"] (TOK_NEG);
  add ["negs"; "NEGS"] (TOK_NEGS);
  add ["smaddl"; "SMADDL"] (MOPL AArch64Base.MOPLExt.(Signed,ADD));
  add ["smsubl"; "SMSUBL"] (MOPL AArch64Base.MOPLExt.(Signed,SUB));
  add ["umaddl"; "UMADDL"] (MOPL AArch64Base.MOPLExt.(Unsigned,ADD));
  add ["umsubl"; "UMSUBL"] (MOPL AArch64Base.MOPLExt.(Unsigned,SUB));
  add ["smull"; "SMULL"] (MOPLZ AArch64Base.MOPLExt.(Signed,ADD));
  add ["smnegl"; "SMNEGL"] (MOPLZ AArch64Base.MOPLExt.(Signed,SUB));
  add ["umull"; "UMULL"] (MOPLZ AArch64Base.MOPLExt.(Unsigned,ADD));
  add ["umnegl"; "UMNEGL"] (MOPLZ AArch64Base.MOPLExt.(Unsigned,SUB));
  add ["madd"; "MADD"] (MOP AArch64Base.MOPExt.(ADD));
  add ["msub"; "MSUB"] (MOP AArch64Base.MOPExt.(SUB));
  add ["mneg"; "MNEG"] (MOPZ AArch64Base.MOPExt.(SUB));
  (* Morello *)
  add ["alignd"; "ALIGND"] (ALIGND);
  add ["alignu"; "ALIGNU"] (ALIGNU);
  add ["build"; "BUILD"] (BUILD);
  add ["chkeq"; "CHKEQ"] (CHKEQ);
  add ["chksld"; "CHKSLD"] (CHKSLD);
  add ["chktgd"; "CHKTGD"] (CHKTGD);
  add ["clrtag"; "CLRTAG"] (CLRTAG);
  add ["cpy"; "CPY"] (CPY);
  add ["cpytype"; "CPYTYPE"] (CPYTYPE);
  add ["cpyvalue"; "CPYVALUE"] (CPYVALUE);
  add ["cseal"; "CSEAL"] (CSEAL);
  add ["cthi"; "CTHI"] (SC A.CTHI);
  add ["gcflgs"; "GCFLGS"] (GC A.GCFLGS);
  add ["gcperm"; "GCPERM"] (GC A.GCPERM);
  add ["gcseal"; "GCSEAL"] (GC A.GCSEAL);
  add ["gctag"; "GCTAG"] (GC A.GCTAG);
  add ["gctype"; "GCTYPE"] (GC A.GCTYPE);
  add ["gcvalue"; "GCVALUE"] (GC A.GCVALUE);
  add ["ldct"; "LDCT"] (LDCT);
  add ["scflgs"; "SCFLGS"] (SC A.SCFLGS);
  add ["sctag"; "SCTAG"] (SC A.SCTAG);
  add ["scvalue"; "SCVALUE"] (SC A.SCVALUE);
  add ["seal"; "SEAL"] (SEAL);
  add ["stct"; "STCT"] (STCT);
  add ["unseal"; "UNSEAL"] (UNSEAL);
  (* Guarded Control Stack *)
  add ["gcspopm"; "GCSPOPM"] (GCSPOPM);
  add ["gcspushm"; "GCSPUSHM"] (GCSPUSHM);
  add ["gcsstr"; "GCSSTR"] (GCSSTR);
  add ["gcsss1"; "GCSSS1"] (GCSSS1);
  add ["gcsss2"; "GCSSS2"] (GCSSS2);
  (* Misc *)
  add ["csel"; "CSEL"] (CSEL);
  add ["csinc"; "CSINC"] (CSINC);
  add ["csinv"; "CSINV"] (CSINV);
  add ["csneg"; "CSNEG"] (CSNEG);
  add ["cset"; "CSET"] (CSET);
  add ["csetm"; "CSETM"] (CSETM);
  add ["cinc"; "CINC"] (CINC);
  (* Fences *)
  add ["dmb"; "DMB"] (TOK_DMB);
  add ["dsb"; "DSB"] (TOK_DSB);
  add ["isb"; "ISB"] (TOK_ISB);
  add ["gcsb"; "GCSB"] (TOK_GCSB);
  (* Fence Operands *)
  add ["sy"; "SY"] (TOK_SY);
  add ["st"; "ST"] (TOK_ST);
  add ["ld"; "LD"] (TOK_LD);
  add ["osh"; "OSH"] (TOK_OSH);
  add ["oshst"; "OSHST"] (TOK_OSHST);
  add ["oshld"; "OSHLD"] (TOK_OSHLD);
  add ["ish"; "ISH"] (TOK_ISH);
  add ["ishst"; "ISHST"] (TOK_ISHST);
  add ["ishld"; "ISHLD"] (TOK_ISHLD);
  add ["nsh"; "NSH"] (TOK_NSH);
  add ["nshst"; "NSHST"] (TOK_NSHST);
  add ["nshld"; "NSHLD"] (TOK_NSHLD);
  add ["dsync"; "DSYNC"] (TOK_DSYNC);
  (* inline barrel shift operands *)
  add ["msl"; "MSL"] (TOK_MSL);
  (* Cache maintenance *)
  add ["ic"; "IC"] (IC);
  add ["dc"; "DC"] (DC);
  add ["ialluis"; "IALLUIS"] (A.IC.(IC_OP { funct=I; typ=ALL; point=U; domain=IS; }));
  add ["ivauis"; "IVAUIS"] (A.IC.(IC_OP { funct=I; typ=VA; point=U; domain=IS; }));
  add ["iallu"; "IALLU"] (A.IC.(IC_OP { funct=I; typ=ALL; point=U; domain=NO; }));
  add ["ivau"; "IVAU"] (IVAU);
  add ["ivac"; "IVAC"] (A.DC.(DC_OP { funct=I; typ=VA; point=CO; }));
  add ["cvac"; "CVAC"] (A.DC.(DC_OP { funct=C; typ=VA; point=CO; }));
  add ["civac"; "CIVAC"] (A.DC.(DC_OP { funct=CI; typ=VA; point=CO; }));
  add ["zvac"; "ZVAC"] (A.DC.(DC_OP { funct=Z; typ=VA; point=CO; }));
  add ["iswc"; "ISWC"] (A.DC.(DC_OP { funct=I; typ=SW; point=CO; }));
  add ["cswc"; "CSWC"] (A.DC.(DC_OP { funct=C; typ=SW; point=CO; }));
  add ["ciswc"; "CISWC"] (A.DC.(DC_OP { funct=CI; typ=SW; point=CO; }));
  add ["zswc"; "ZSWC"] (A.DC.(DC_OP { funct=Z; typ=SW; point=CO; }));
  add ["cvau"; "CVAU"] (A.DC.(DC_OP { funct=C; typ=VA; point=U; }));
  add ["civau"; "CIVAU"] (A.DC.(DC_OP { funct=CI; typ=VA; point=U; }));
  add ["zvau"; "ZVAU"] (A.DC.(DC_OP { funct=Z; typ=VA; point=U; }));
  add ["iswu"; "ISWU"] (A.DC.(DC_OP { funct=I; typ=SW; point=U; }));
  add ["cswu"; "CSWU"] (A.DC.(DC_OP { funct=C; typ=SW; point=U; }));
  add ["ciswu"; "CISWU"] (A.DC.(DC_OP { funct=CI; typ=SW; point=U; }));
  add ["zswu"; "ZSWU"] (A.DC.(DC_OP { funct=Z; typ=SW; point=U; }));
  (* Idem, tlb *)
  add ["tlbi"; "TLBI"] (TLBI);
  (* Arguments, and there are many... *)
  add ["ipas2e1is"; "IPAS2E1IS"] (A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=IS; nXS=false; }));
  add ["ipas2le1is"; "IPAS2LE1IS"] (A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=IS; nXS=false; }));
  add ["ipas2e1"; "IPAS2E1"] (A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=No; nXS=false; }));
  add ["ipas2le1"; "IPAS2LE1"] (A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=No; nXS=false; }));
  add ["vmalle1is"; "VMALLE1IS"] (A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=IS; nXS=false; }));
  add ["vmalle1"; "VMALLE1"] (A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=No; nXS=false; }));
  add ["alle1is"; "ALLE1IS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=IS; nXS=false; }));
  add ["alle2is"; "ALLE2IS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=IS; nXS=false; }));
  add ["alle3is"; "ALLE3IS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=IS; nXS=false; }));
  add ["alle1"; "ALLE1"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=No; nXS=false; }));
  add ["alle2"; "ALLE2"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=No; nXS=false; }));
  add ["alle3"; "ALLE3"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=No; nXS=false; }));
  add ["vae1is"; "VAE1IS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=IS; nXS=false; }));
  add ["vae2is"; "VAE2IS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=IS; nXS=false; }));
  add ["vae3is"; "VAE3IS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=IS; nXS=false; }));
  add ["vae1"; "VAE1"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=No; nXS=false; }));
  add ["vae2"; "VAE2"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=No; nXS=false; }));
  add ["vae3"; "VAE3"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=No; nXS=false; }));
  add ["aside1is"; "ASIDE1IS"] (A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=IS; nXS=false; }));
  add ["aside1"; "ASIDE1"] (A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=No; nXS=false; }));
  add ["vaae1is"; "VAAE1IS"] (A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=IS; nXS=false; }));
  add ["vaae1"; "VAAE1"] (A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=No; nXS=false; }));
  add ["vale1is"; "VALE1IS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=IS; nXS=false; }));
  add ["vale2is"; "VALE2IS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=IS; nXS=false; }));
  add ["vale3is"; "VALE3IS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=IS; nXS=false; }));
  add ["vale1"; "VALE1"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=No; nXS=false; }));
  add ["vale2"; "VALE2"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=No; nXS=false; }));
  add ["vale3"; "VALE3"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=No; nXS=false; }));
  add ["vaale1is"; "VAALE1IS"] (A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=IS; nXS=false; }));
  add ["vaale1"; "VAALE1"] (A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=No; nXS=false; }));
  add ["vmalls12e1is"; "VMALLS12E1IS"] (A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=IS; nXS=false; }));
  add ["vmalls12e1"; "VMALLS12E1"] (A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=No; nXS=false; }));
  (* nXS version of the above *)
  add ["ipas2e1isnxs"; "IPAS2E1ISNXS"] (A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=IS; nXS=true; }));
  add ["ipas2le1isnxs"; "IPAS2LE1ISNXS"] (A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=IS; nXS=true; }));
  add ["ipas2e1nxs"; "IPAS2E1NXS"] (A.TLBI.(TLBI_OP {typ=IPAS2; level=A.E1; domain=No; nXS=true; }));
  add ["ipas2le1nxs"; "IPAS2LE1NXS"] (A.TLBI.(TLBI_OP {typ=IPAS2L; level=A.E1; domain=No; nXS=true; }));
  add ["vmalle1isnxs"; "VMALLE1ISNXS"] (A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=IS; nXS=true; }));
  add ["vmalle1nxs"; "VMALLE1NXS"] (A.TLBI.(TLBI_OP {typ=VMALL; level=A.E1; domain=No; nXS=true; }));
  add ["alle1isnxs"; "ALLE1ISNXS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=IS; nXS=true; }));
  add ["alle2isnxs"; "ALLE2ISNXS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=IS; nXS=true; }));
  add ["alle3isnxs"; "ALLE3ISNXS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=IS; nXS=true; }));
  add ["alle1nxs"; "ALLE1NXS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E1; domain=No; nXS=true; }));
  add ["alle2nxs"; "ALLE2NXS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E2; domain=No; nXS=true; }));
  add ["alle3nxs"; "ALLE3NXS"] (A.TLBI.(TLBI_OP {typ=ALL; level=A.E3; domain=No; nXS=true; }));
  add ["vae1isnxs"; "VAE1ISNXS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=IS; nXS=true; }));
  add ["vae2isnxs"; "VAE2ISNXS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=IS; nXS=true; }));
  add ["vae3isnxs"; "VAE3ISNXS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=IS; nXS=true; }));
  add ["vae1nxs"; "VAE1NXS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E1; domain=No; nXS=true; }));
  add ["vae2nxs"; "VAE2NXS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E2; domain=No; nXS=true; }));
  add ["vae3nxs"; "VAE3NXS"] (A.TLBI.(TLBI_OP {typ=VA; level=A.E3; domain=No; nXS=true; }));
  add ["aside1isnxs"; "ASIDE1ISNXS"] (A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=IS; nXS=true; }));
  add ["aside1nxs"; "ASIDE1NXS"] (A.TLBI.(TLBI_OP {typ=ASID; level=A.E1; domain=No; nXS=true; }));
  add ["vaae1isnxs"; "VAAE1ISNXS"] (A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=IS; nXS=true; }));
  add ["vaae1nxs"; "VAAE1NXS"] (A.TLBI.(TLBI_OP {typ=VAA; level=A.E1; domain=No; nXS=true; }));
  add ["vale1isnxs"; "VALE1ISNXS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=IS; nXS=true; }));
  add ["vale2isnxs"; "VALE2ISNXS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=IS; nXS=true; }));
  add ["vale3isnxs"; "VALE3ISNXS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=IS; nXS=true; }));
  add ["vale1nxs"; "VALE1NXS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E1; domain=No; nXS=true; }));
  add ["vale2nxs"; "VALE2NXS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E2; domain=No; nXS=true; }));
  add ["vale3nxs"; "VALE3NXS"] (A.TLBI.(TLBI_OP {typ=VAL; level=A.E3; domain=No; nXS=true; }));
  add ["vaale1isnxs"; "VAALE1ISNXS"] (A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=IS; nXS=true; }));
  add ["vaale1nxs"; "VAALE1NXS"] (A.TLBI.(TLBI_OP {typ=VAAL; level=A.E1; domain=No; nXS=true; }));
  add ["vmalls12e1isnxs"; "VMALLS12E1ISNXS"] (A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=IS; nXS=true; }));
  add ["vmalls12e1nxs"; "VMALLS12E1NXS"] (A.TLBI.(TLBI_OP {typ=VMALLS12; level=A.E1; domain=No; nXS=true; }));
  (* Address translation and its operands *)
  (* Restricted to stage 1 only for EL1 and EL0; excludes <pan> and <ignore> fields *)
  add ["at"; "AT"] (AT);
  add ["s1e0r"; "S1E0R"] (A.AT.(AT_OP {stages=S1; level=A.E0; rw=R; }));
  add ["s1e0w"; "S1E0W"] (A.AT.(AT_OP {stages=S1; level=A.E0; rw=W; }));
  add ["s1e1r"; "S1E1R"] (A.AT.(AT_OP {stages=S1; level=A.E1; rw=R; }));
  add ["s1e1w"; "S1E1W"] (A.AT.(AT_OP {stages=S1; level=A.E1; rw=W; }));
  (* System registers *)
  add ["mrs"; "MRS"] (MRS);
  add ["msr"; "MSR"] (MSR);
  tbl

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
