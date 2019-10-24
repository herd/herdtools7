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
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open AArch64Parser
module A = AArch64Base
module LU = LexUtils.Make(O)

let check_name name =
if O.debug then Printf.eprintf "Check: '%s'\n"  name ;
match name with
| "nop"|"NOP" -> NOP
(* Branch *)
| "b"  | "B"  -> B
| "br"  | "BR"  -> BR
| "bl"  | "BL"  -> BL
| "blr"  | "BLR"  -> BLR
| "ret"  | "RET" -> RET
| "ne"  | "NE"  -> NE
| "eq"  | "EQ"  -> EQ
| "b.eq" | "B.EQ" -> BEQ
| "b.ne" | "B.NE" -> BNE
| "cbz"  | "CBZ" -> CBZ
| "cbnz"  | "CBNZ" -> CBNZ
(* Memory *)
| "ldr"|"LDR" -> LDR
| "ldp"|"LDP" -> LDP
| "ldnp"|"LDNP" -> LDP
| "stp"|"STP" -> STP
| "stnp"|"STNP" -> STNP
| "ldrb"|"LDRB" -> LDRB
| "ldrh"|"LDRH" -> LDRH
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
(* Memory Tagging *)
| "stg"|"STG" -> STG
| "ldg"|"LDG" -> LDG
(* Operations *)
| "sxtw"|"SXTW" -> SXTW
| "mov"|"MOV" -> MOV
| "adr"|"ADR" -> ADR
| "rbit"|"RBIT" -> RBIT
| "add"|"ADD" -> OP A.ADD
| "adds"|"ADDS" -> OP A.ADDS
| "eor"|"EOR" -> OP A.EOR
| "orr"|"ORR" -> OP A.ORR
| "and"|"AND" -> OP A.AND
| "ands"|"ANDS" -> OP A.ANDS
| "sub"|"SUB" -> OP A.SUB
| "subs"|"SUBS" -> OP A.SUBS
| "cmp"|"CMP" -> CMP
| "tst"|"TST" -> TST
(* Misc *)
| "csel"|"CSEL" -> CSEL
| "csinc"|"CSINC" -> CSINC
| "csinv"|"CSINV" -> CSINV
| "csneg"|"CSNEG" -> CSNEG
| "cset"|"CSET" -> CSET
(* Fences *)
| "dmb"|"DMB" -> DMB
| "dsb"|"DSB" -> DSB
| "isb"|"ISB" -> ISB
(* Fence Operands *)
| "sy"|"SY" ->SY
| "st"|"ST" -> ST
| "ld"|"LD" -> LD
| "osh"|"OSH" -> OSH
| "oshst"|"OSHST" -> OSHST
| "oshld"|"OSHLD" -> OSHLD
| "ish"|"ISH" -> ISH
| "ishst"|"ISHST" -> ISHST
| "ishld"|"ISHLD" -> ISHLD
| "nsh"|"NSH" -> NSH
| "nshst"|"NSHST" -> NSHST
| "nshld"|"NSHLD" -> NSHLD
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
(* System registers *)
| "mrs"|"MRS" -> MRS
| "ctr_el0"|"CTR_EL0" -> SYSREG A.CTR_EL0
| "dciz_el0"|"DCIZ_EL0" -> SYSREG A.DCIZ_EL0
| "mdccsr_el0"|"MDCCSR_EL0" -> SYSREG A.MDCCSR_EL0
| "dbgdtr_el0"|"DBGDTR_EL0" -> SYSREG A.DBGDTR_EL0
| "dbgdtrrx_el0"|"DBGDTRRX_EL0" -> SYSREG A.DBGDTRRX_EL0
| "Dbgdtrtx_el0"|"DBGDTRTX_EL0" -> SYSREG A.DBGDTRTX_EL0
| _ ->
    begin match A.parse_wreg name with
    | Some r -> ARCH_WREG r
    | None ->
        begin match A.parse_xreg name with
        | Some r -> ARCH_XREG r
        | None -> NAME name
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
| '#' ('-' ? num as x) { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| ['w''W']'%' (name as name) { SYMB_WREG name }
| ['x''X']?'%' (name as name) { SYMB_XREG name }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
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
