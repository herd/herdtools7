(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
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
open CParser
open Lexing
open LexMisc
module LU = LexUtils.Make(O)

(* Compiled efficiently by the next version of ocaml *)
let tr_name s = match s with
| "volatile" -> VOLATILE
| "_Atomic" -> ATOMIC
| "char" -> CHAR
| "int" -> INT
| "long" -> LONG
| "int8_t"
| "uint8_t"
| "int16_t"
| "uint16_t"
| "int32_t"
| "uint32_t"
| "int64_t"
| "uint64_t"
| "intptr_t"
| "uintptr_t"
(* Mutexes *)
| "mtx_t"        (* c11 *)
| "spinlock_t"   (* linux *)
(* Atomics *)
| "atomic_t"     (* linux *)

  ->
  BASE_TYPE s
| "atomic_int" -> ATOMIC_TYPE "int"
| "void" -> VOID
| "NULL" -> NULL
(* C11 primitives, quite a lot! *)
| "atomic_fetch_add_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Add
| "atomic_fetch_sub_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Add
| "atomic_fetch_or_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Or
| "atomic_fetch_xor_explicit" -> ATOMIC_FETCH_EXPLICIT Op.Xor
| "atomic_fetch_and_explicit" -> ATOMIC_FETCH_EXPLICIT Op.And
| "memory_order_acquire" -> MEMORDER (MemOrder.Acq)
| "memory_order_release" -> MEMORDER (MemOrder.Rel)
| "memory_order_acq_rel" -> MEMORDER (MemOrder.Acq_Rel)
| "memory_order_seq_cst" -> MEMORDER (MemOrder.SC)
| "memory_order_relaxed" -> MEMORDER (MemOrder.Rlx)
| "memory_order_consume" -> MEMORDER (MemOrder.Con)
|"atomic_thread_fence" -> FENCE
| "atomic_load_explicit" -> LD_EXPLICIT
| "atomic_store_explicit" -> ST_EXPLICIT
| "atomic_exchange_explicit" -> EXC_EXPLICIT
| "lock"|"mtx_lock" -> LOCK
| "WCAS" -> WCAS
| "SCAS" -> SCAS
| "atomic_compare_exchange_weak" -> WCAS
| "atomic_compare_exchange_strong" -> SCAS
| "atomic_compare_exchange_weak_explicit" -> WCAS_EXPLICIT
| "atomic_compare_exchange_strong_explicit" -> SCAS_EXPLICIT
| "unlock"|"mtx_unlock"  -> UNLOCK
(* Internal Linux *)
| "__fence" -> UNDERFENCE
| "__load" -> LOAD
| "__store" -> STORE
| "__xchg" -> XCHG
| "__cmpxchg" -> CMPXCHG
| "__lock" -> SPINLOCK
| "__trylock" -> SPINTRYLOCK
| "__unlock" -> SPINUNLOCK
| "__atomic_op" -> UNDERATOMICOP
| "__atomic_op_return" -> UNDERATOMICOPRETURN
| "__atomic_fetch_op" -> UNDERATOMICFETCHOP
| "__atomic_add_unless" -> UNDERATOMICADDUNLESS
| "atomic_add_unless" -> ATOMICADDUNLESS
(* Others *)
| x -> IDENTIFIER x
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let name = (alpha | '_') (alpha|digit|'_')*
let num = digit+

rule token deep = parse
| [' ''\t''\r']+ { token deep lexbuf }
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
| "<" { LT }
| ">" { GT }
| "<=" { LE }
| ">=" { GE }
| '.' {DOT}
| "constvar:" (name as s) { CONSTVAR s }
| "codevar:" (name as s) { CODEVAR s }
| '%' name as s { IDENTIFIER s }
| name as x   { tr_name x  }
| eof { EOF }
| "" { LexMisc.error "C lexer" lexbuf }

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
