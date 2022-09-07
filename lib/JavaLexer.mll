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
open JavaParser
open JavaBase
exception Error

let tr_name = function
    (* primitive types *)
    | "int"                     -> INT
    (* read access modes *)
    | "get"                     -> READ (AccessModes.Plain)
    | "getVolatile"             -> READ (AccessModes.Volatile)
    | "getAcquire"              -> READ (AccessModes.Acquire)
    | "getOpaque"               -> READ (AccessModes.Opaque)
    (* write access modes *)
    | "set"                     -> WRITE (AccessModes.Plain)
    | "setVolatile"             -> WRITE (AccessModes.Volatile)
    | "setRelease"              -> WRITE (AccessModes.Release)
    | "setOpaque"               -> WRITE (AccessModes.Opaque)
    (* atomic update access modes *)
    (* compare and set  *)
    (*
    | "compareAndSet"
      -> COMPARE_AND_SET (Op.Eq, AccessModes.Volatile, AccessModes.Volatile)
    | "weakCompareAndSet"
      -> COMPARE_AND_SET (Op.Eq, AccessModes.Volatile, AccessModes.Volatile)
    | "weakCompareAndSetAcquire"
      -> COMPARE_AND_SET (Op.Eq, AccessModes.Acquire , AccessModes.Plain)
    | "weakCompareAndSetRelease"
      -> COMPARE_AND_SET (Op.Eq, AccessModes.Plain, AccessModes.Release)
    | "weakCompareAndSetPlain"
      -> COMPARE_AND_SET (Op.Eq, AccessModes.Plain, AccessModes.Plain)
    *)
     (* compare and exchange *)
    | "compareAndExchange"
      -> COMPARE_AND_EXCHANGE (AccessModes.Volatile, AccessModes.Volatile)
    | "compareAndExchangeAcquire"
      -> COMPARE_AND_EXCHANGE (AccessModes.Acquire, AccessModes.Plain)
    | "compareAndExchangeRelease"
      -> COMPARE_AND_EXCHANGE (AccessModes.Plain, AccessModes.Release)
    (* get and set (is_weak?, access_mode for R, access_mode for W) *)
    (*
    | "getAndSet"
      -> GET_AND_SET (AccessModes.Volatile, AccessModes.Volatile)
    | "getAndSetAcquire"
      -> GET_AND_SET (AccessModes.Acquire, AccessModes.Plain)
    | "getAndSetRelease"
      -> GET_AND_SET (AccessModes.Plain, AccessModes.Release)
    *)
    (* numeric atomic update access modes (is_weak?, access_mode for R, access_mode for W) *)
    | "getAndAdd"
      -> GET_AND_OP (Op.Add,AccessModes.Volatile)
    | "getAndAddAcquire"
      -> GET_AND_OP (Op.Add,AccessModes.Acquire)
    | "getAndAddRelease"
      -> GET_AND_OP (Op.Add,AccessModes.Release)
    (* bitwise atomic update access modes *)
    | "getAndBitwiseOr"
      -> GET_AND_OP (Op.Or, AccessModes.Volatile)
    | "getAndBitwiseOrAcquire"
      -> GET_AND_OP (Op.Or, AccessModes.Acquire)
    | "getAndBitwiseOrRelease"
      -> GET_AND_OP (Op.Or, AccessModes.Release)
    | "getAndBitwiseAnd"
      -> GET_AND_OP (Op.And, AccessModes.Volatile)
    | "getAndBitwiseAndAcquire"
      -> GET_AND_OP (Op.And, AccessModes.Acquire)
    | "getAndBitwiseAndRelease"
      -> GET_AND_OP (Op.And, AccessModes.Release)
    | "getAndBitwiseXor"
      -> GET_AND_OP (Op.Xor, AccessModes.Volatile)
    | "getAndBitwiseXorAcquire"
      -> GET_AND_OP (Op.Xor, AccessModes.Acquire)
    | "getAndBitwiseXorRelease"
      -> GET_AND_OP (Op.Xor, AccessModes.Release)
    | x -> IDENTIFIER x
}

let digit = ['0'-'9']
let lowercase_alpha = ['a'-'z']
let capitalized_alpha = ['A' - 'Z']

(* local variable name or function names *)
let name = (lowercase_alpha | '_') (capitalized_alpha|lowercase_alpha|digit|'_')*

(* varhandle names are capitalized, such as "X" for x and "Y" for y *)
let varhandle_name = (capitalized_alpha | '_') (capitalized_alpha | digit | '_')*

let num = digit+

rule token = parse
| [' ''\t''\r']+ { token lexbuf }
| '\n' { token lexbuf ;}
| '-' ? num as x { CONSTANT (int_of_string x) }
| "Thread" (num as x) { PROC (int_of_string x) }
| ';' { SEMI }
| ',' { COMMA }
| "||" { OR }
| '*' { MUL }
| '/' { DIV }
| '+' { ADD }
| '-' { SUB }
| '^' { XOR }
| "&&" { AND }
| '(' { LPAR }
| ')' { RPAR }
| '{' { LBRACE }
| '}' { RBRACE }
| "if"    { IF }
| "else"  { ELSE }
| '=' { EQ }
| "==" { EQ_OP }
| "!=" { NEQ_OP }
| "<" { LT }
| ">" { GT }
| "<=" { LE }
| ">=" { GE }
| '.' { DOT }
| "fullFence"       { FENCE_FULL }
| "acquireFence"    { FENCE_ACQUIRE }
| "releaseFence"    { FENCE_RELEASE }
| "loadLoadFence"   { FENCE_LL }
| "storeStoreFence" { FENCE_SS }
| varhandle_name as vh { VARHANDLE vh }
| name as x   { tr_name x  }
| eof { EOF }
| "" { raise Error }


{
let token lexbuf =
   let tok = token lexbuf in
   tok

end
}
