(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
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
  val aarch64 : bool
end

module type Out = sig
  type t
  val put_char : t -> char -> unit
  val put : t -> string -> unit
end

module Make(O:Config)(Out:Out) = struct
}
let blank = [' ''\t']
let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let reg = alpha (alpha|num)*
    
rule main out = parse
| '(' blank* "xor" blank+ (reg as r1) blank+ (reg as r2) ')' as line
{
 begin if r1 = r2 then
   Out.put out (Printf.sprintf "(and %s 128)" r1)
 else
   Out.put out line
 end ;
 main out lexbuf
}
| "f[sync]"|"[deref]"|"[lderef]" as token
    { if O.aarch64 then false else begin
        Out.put out token ;
        main out lexbuf
      end }

| _ as c { Out.put_char out c ; main out lexbuf }
| eof    { true }
{

 let tr out lexbuf  = main out lexbuf
end
}
