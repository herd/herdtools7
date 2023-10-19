(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
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
open X86_64Parser
module LU = LexUtils.Make(O)

let check_name name =
match name with
| "add"|"ADD"   -> I_ADD
| "addb"|"ADDB"   -> I_ADDB
| "addw"|"ADDW"   -> I_ADDW
| "addl"|"ADDL"   -> I_ADDL
| "addq"|"ADDQ"   -> I_ADDQ
| "xor"|"XOR"    -> I_XOR
| "xorb"|"XORB"    ->I_XORB
| "xorw"|"XORW"    ->I_XORW
| "xorl"|"XORL"    ->I_XORL
| "xorq"|"XORQ"    ->I_XORQ
| "or"|"OR"    ->I_OR
| "and"|"AND"  ->I_AND
| "orb"|"ORB"    ->I_ORB
| "orw"|"ORW"    ->I_ORW
| "orl"|"ORL"    ->I_ORL
| "orq"|"ORQ"    ->I_ORQ
| "shl"|"SHL" ->I_SHL
| "mov"|"MOV"   ->I_MOV
| "movb"|"MOVB"   ->I_MOVB
| "movw"|"MOVW"   ->I_MOVW
| "movl"|"MOVL"   ->I_MOVL
| "movq"|"MOVQ"   ->I_MOVQ
| "movnti"|"MOVNTI"   ->I_MOVNTI
| "movntil"|"MOVNTIL"   ->I_MOVNTIL
| "movntiq"|"MOVNTIQ"   ->I_MOVNTIQ
| "movd"|"MOVD" ->I_MOVD
| "movntdqa"|"MOVNTDQA" ->I_MOVNTDQA
| "dec"|"DEC"    ->I_DEC
| "decb"|"DECB"    ->I_DECB
| "decw"|"DECW"    ->I_DECW
| "decl"|"DECL"    ->I_DECL
| "decq"|"DECQ"    ->I_DECQ
| "inc"|"INC"    ->I_INC
| "incb"|"INCB"    ->I_INCB
| "incw"|"INCW"    ->I_INCW
| "incl"|"INCL"    ->I_INCL
| "incq"|"INCQ"    ->I_INCQ
| "cmp"|"CMP"    ->I_CMP
| "cmpb"|"CMPB"    ->I_CMPB
| "cmpw"|"CMPW"    ->I_CMPW
| "cmpl"|"CMPL"    ->I_CMPL
| "cmpq"|"CMPQ"    ->I_CMPQ
| "cmovc"|"CMOVC"    ->I_CMOVC
| "cmovcb"|"CMOVCB"    ->I_CMOVCB
| "cmovcw"|"CMOVCW"    ->I_CMOVCW
| "cmovcl"|"CMOVCL"    ->I_CMOVCL
| "cmovcq"|"CMOVCQ"    ->I_CMOVCQ
| "ret" | "RET"        ->I_RET
| "retq" | "RETQ"      ->I_RET
| "jmp"|"JMP"   ->I_JMP
| "je"|"JE"    ->I_JE
| "jne"|"JNE"    ->I_JNE
| "jle"|"JLE"    ->I_JLE
| "jlt"|"JLT"    ->I_JLT
| "jgt"|"JGT"    ->I_JGT
| "jge"|"JGE"    ->I_JGE
| "js"|"JS"    ->I_JS
| "jns"|"JNS"    ->I_JNS
| "lock"|"LOCK"   ->I_LOCK
| "xchg"|"XCHG"    ->I_XCHG
| "xchgb"|"XCHGB"    ->I_XCHGB
| "xchgw"|"XCHGW"    ->I_XCHGW
| "xchgl"|"XCHGL"    ->I_XCHGL
| "xchgq"|"XCHGQ"    ->I_XCHGQ
| "uxch"|"UXCH"    ->I_UXCH
| "uxchb"|"UXCHB"    ->I_UXCHB
| "uxchw"|"UXCHW"    ->I_UXCHW
| "uxchl"|"UXCHL"    ->I_UXCHL
| "uxchq"|"UXCHQ"    ->I_UXCHQ
| "cmpxchg"|"CMPXCHG"    ->I_CMPXCHG
| "cmpxchgb"|"CMPXCHGB"    ->I_CMPXCHGB
| "cmpxchgw"|"CMPXCHGW"    ->I_CMPXCHGW
| "cmpxchgl"|"CMPXCHGL"    ->I_CMPXCHGL
| "cmpxchgq"|"CMPXCHGQ"    ->I_CMPXCHGQ
| "mfence"|"MFENCE"   ->I_MFENCE
| "lfence"|"LFENCE"   ->I_LFENCE
| "sfence"|"SFENCE"   ->I_SFENCE
| "setnb"|"SETNB"       ->I_SETNB
| "nopl" | "NOPL" -> I_NOP
| "nop" | "NOP" -> I_NOPA
| "nopw" | "NOPW" -> I_NOP
| "endbr64" -> I_ENDBR64
| "clflush"|"CLFLUSH" ->I_CLFLUSH X86_64Base.NoOpt
| "clflushopt"|"CLFLUSHOPT" ->I_CLFLUSH X86_64Base.Opt
| _ -> begin match X86_64Base.parse_any_reg name with
  | Some r -> ARCH_REG r
  | None ->
      begin match  X86_64Base.parse_xmm_reg name with
      | Some r -> XMM r
      | None -> NAME name
      end
  end

}
let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = '_'? alpha (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM x }
| '$' ('-'? num as x) { INTEL_NUM x }
| 'P' (num as x)
    { PROC (int_of_string x) }
| '%' (name as name)
    { match X86_64Base.parse_any_reg name with
    | Some r -> ARCH_REG r
    | None ->
        begin match X86_64Base.parse_xmm_reg name with
        | Some xmm -> XMM xmm
        | None -> SYMB_REG name
        end }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| name as x { check_name x }
| eof { EOF }
| ""  { error "X86_64 Lexer" lexbuf }

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
