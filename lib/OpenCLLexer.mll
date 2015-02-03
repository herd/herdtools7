(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*     John Wickerson, Imperial College London, UK.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
module Make(O:LexUtils.Config) = struct
open OpenCLParser
open Lexing
open LexMisc
module LU = LexUtils.Make(O)

(* Compiled efficiently by the next version of ocaml *)
let tr_name = function
| "volatile" -> VOLATILE
| "unsigned" -> UNSIGNED
| "signed" -> SIGNED
| "_Atomic" -> ATOMIC
| "char" -> CHAR
| "short" -> SHORT
| "int" ->  INT
| "long" -> LONG
| "float" -> FLOAT
| "double" -> DOUBLE
| "_Bool" -> BOOL
| "atomic_bool" -> ATOMIC_TYPE "_Bool"
| "atomic_char" -> ATOMIC_TYPE "char"
| "atomic_schar" -> ATOMIC_TYPE "signed char"
| "atomic_uchar" -> ATOMIC_TYPE "unsigned char"
| "atomic_short" -> ATOMIC_TYPE "short"
| "atomic_ushort" -> ATOMIC_TYPE "unsigned short"
| "atomic_int" -> ATOMIC_TYPE "int"
| "atomic_uint" -> ATOMIC_TYPE "unsigned int"
| "atomic_long" -> ATOMIC_TYPE "long"
| "atomic_ulong" -> ATOMIC_TYPE "unsigned long"
| "atomic_llong" -> ATOMIC_TYPE "long long"
| "atomic_ullong" -> ATOMIC_TYPE "unsigned long long"
| "atomic_char16_t" -> ATOMIC_TYPE "__CHAR16_TYPE__"
| "atomic_char32_t" -> ATOMIC_TYPE "__CHAR32_TYPE__"
| "atomic_wchar_t" -> ATOMIC_TYPE "__WCHAR_TYPE__"
| "atomic_int_least8_t" -> ATOMIC_TYPE "__INT_LEAST8_TYPE__"
| "atomic_uint_least8_t" -> ATOMIC_TYPE "__UINT_LEAST8_TYPE__"
| "atomic_int_least16_t" -> ATOMIC_TYPE "__INT_LEAST16_TYPE__"
| "atomic_uint_least16_t" -> ATOMIC_TYPE "__UINT_LEAST16_TYPE__"
| "atomic_int_least32_t" -> ATOMIC_TYPE "__INT_LEAST32_TYPE__"
| "atomic_uint_least32_t" -> ATOMIC_TYPE "__UINT_LEAST32_TYPE__"
| "atomic_int_least64_t" -> ATOMIC_TYPE "__INT_LEAST64_TYPE__"
| "atomic_uint_least64_t" -> ATOMIC_TYPE "__UINT_LEAST64_TYPE__"
| "atomic_int_fast8_t" -> ATOMIC_TYPE "__INT_FAST8_TYPE__"
| "atomic_uint_fast8_t" -> ATOMIC_TYPE "__UINT_FAST8_TYPE__"
| "atomic_int_fast16_t" -> ATOMIC_TYPE "__INT_FAST16_TYPE__"
| "atomic_uint_fast16_t" -> ATOMIC_TYPE "__UINT_FAST16_TYPE__"
| "atomic_int_fast32_t" -> ATOMIC_TYPE "__INT_FAST32_TYPE__"
| "atomic_uint_fast32_t" -> ATOMIC_TYPE "__UINT_FAST32_TYPE__"
| "atomic_int_fast64_t" -> ATOMIC_TYPE "__INT_FAST64_TYPE__"
| "atomic_uint_fast64_t" -> ATOMIC_TYPE "__UINT_FAST64_TYPE__"
| "atomic_intptr_t" -> ATOMIC_TYPE "__INTPTR_TYPE__"
| "atomic_uintptr_t" -> ATOMIC_TYPE "__UINTPTR_TYPE__"
| "atomic_size_t" -> ATOMIC_TYPE "__SIZE_TYPE__"
| "atomic_ptrdiff_t" -> ATOMIC_TYPE "__PTRDIFF_TYPE__"
| "atomic_intmax_t" -> ATOMIC_TYPE "__INTMAX_TYPE__"
| "atomic_uintmax_t" -> ATOMIC_TYPE "__UINTMAX_TYPE__"
| "NULL" -> NULL
(* Atomic fetch *)
| "atomic_fetch_add" -> ATOMIC_FETCH Op.Add
| "atomic_fetch_add_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Add
| "atomic_fetch_sub" -> ATOMIC_FETCH Op.Add
| "atomic_fetch_sub_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Add
| "atomic_fetch_or" -> ATOMIC_FETCH Op.Or
| "atomic_fetch_or_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Or
| "atomic_fetch_xor" -> ATOMIC_FETCH Op.Xor
| "atomic_fetch_xor_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Xor
| "atomic_fetch_and" -> ATOMIC_FETCH Op.And
| "atomic_fetch_and_explicit" -> ATOMIC_FETCH_EXPLICIT Op.And
|  x -> IDENTIFIER x
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let name = (alpha | '_') (alpha|digit|'_')*
let num = digit+

rule token deep = parse
| [' ''\t']+ { token deep lexbuf }
| '\n' { incr_lineno lexbuf ; token deep lexbuf ; }
| "/*" { LU.skip_c_comment lexbuf ; token deep lexbuf }
| "//" { LU.skip_c_line_comment lexbuf ; token deep lexbuf }
| '-' ? num as x { CONSTANT (int_of_string x) } 
| 'P' (num+ as x) { PROC (int_of_string x) }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| ':' { COLON }
| '*' { STAR }
| '/' { DIV }
| '+' { ADD }
| '-' { SUB }
| '^' { XOR }
| '&' { LAND }
| '(' { LPAR }
| ')' { RPAR }
| '{' { if deep then LBRACE else begin	
          let buf = Buffer.create 4096 in
          get_body 0 buf lexbuf;
          BODY (Buffer.contents buf)
        end }
| '}' { RBRACE }
| "while" { WHILE }
| "if"    { IF }
| "else"  { ELSE }
| '=' {EQ}
| "==" {EQ_OP}
| "!=" {NEQ_OP}
| "<=" {LEQ_OP}
| '.' {DOT}
| "memory_scope_work_item" | "s_wi" 
  {MEMSCOPE (OpenCLBase.S_workitem)}
| "memory_scope_sub_group" | "s_sg" 
  {MEMSCOPE (OpenCLBase.S_subgroup)}
| "memory_scope_work_group" | "s_wg" 
  {MEMSCOPE (OpenCLBase.S_workgroup)}
| "memory_scope_device" | "s_dev" 
  {MEMSCOPE (OpenCLBase.S_device)}
| "memory_scope_all_svm_devices" | "s_all" 
  {MEMSCOPE (OpenCLBase.S_all_svm_devices)}
| "CLK_GLOBAL_MEM_FENCE" { MEMREGION (OpenCLBase.GlobalMem) }
| "CLK_LOCAL_MEM_FENCE"  { MEMREGION (OpenCLBase.LocalMem) }
| "__global" | "global"  { GLOBAL }
| "__local" | "local"    { LOCAL }
| "scopeTree" | "ScopeTree" { SCOPETREE }
| "kernel"                  { KERNEL }
| "device"                  { DEVICE }
| "cta" | "block" | "work_group" { CTA }
| "warp" | "sub_group"      { WARP }
| "thread"                  { THREAD }
| "memory_order_acquire" | "mo_acq" {MEMORDER (OpenCLBase.Acq)}
| "memory_order_release" | "mo_rel" {MEMORDER (OpenCLBase.Rel)}
| "memory_order_acq_rel" | "mo_ar"  {MEMORDER (OpenCLBase.Acq_Rel)}
| "memory_order_seq_cst" | "mo_sc"  {MEMORDER (OpenCLBase.SC)}
| "memory_order_relaxed" | "mo_rlx" {MEMORDER (OpenCLBase.Rlx)}
| "barrier" | "work_group_barrier" { BARRIER }
| "fence" | "atomic_work_item_fence" { FENCE }
| "atomic_load" | "load" { LD }
| "atomic_store" | "store" { ST }
| "atomic_load_explicit" | "load_exp" { LD_EXPLICIT }
| "atomic_store_explicit" | "store_exp" { ST_EXPLICIT }
| "atomic_exchange" { EXC }
| "atomic_exchange_explicit" { EXC_EXPLICIT }
| "atomic_compare_exchange_weak"  { WCAS }
| "atomic_compare_exchange_strong"  { SCAS }
| "atomic_compare_exchange_weak_explicit"  { WCAS_EXPLICIT }
| "atomic_compare_exchange_strong_explicit"  { SCAS_EXPLICIT }
| "lock"  { LOCK }
| "unlock"    { UNLOCK }
| name as x   { tr_name x  }
| eof { EOF }
| "" { LexMisc.error "OpenCL lexer" lexbuf }

and get_body i buf = parse
| '\n' as lxm
    { incr_lineno lexbuf;
      Buffer.add_char buf lxm ;
      get_body i buf lexbuf ; }
| '{' as lxm
    { Buffer.add_char buf lxm;
      get_body (succ i) buf lexbuf
    }
| '}' as lxm
    { if i > 0 then begin
       Buffer.add_char buf lxm;
       get_body (pred i) buf lexbuf
     end
    }
| eof { LexMisc.error "eof in body" lexbuf }
| _ as lxm { Buffer.add_char buf lxm; get_body i buf lexbuf }

{

let token deep lexbuf =
   let tok = token deep lexbuf in
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
