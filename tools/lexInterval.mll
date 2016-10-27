(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
open Interval
exception Error

let as_nat s = try int_of_string s with _ ->  raise Error
type i = Bound of Interval.b | Single of int
}

let digit = ['0'-'9']
let nat = digit+
let sp = [' ''\t']
rule low = parse
| sp* '[' (nat as nat) { Bound (Nat (as_nat nat,Closed)) }
| sp* ']' (nat as nat) { Bound (Nat (as_nat nat,Open)) }
| sp* (nat as nat) sp * eof  { Single (as_nat nat) }
| "" { raise Error }

and high = parse
| (nat as nat) ']' { Nat (as_nat nat,Closed) }
| (nat as nat) '[' { Nat (as_nat nat,Open) }
| '['              { Infinity }

and dots = parse 
| sp* ".." '.'* sp* { () }
| "" { raise Error }

{

let parse s =
  let buf = Lexing.from_string s in
  let b1 = low buf in
  match b1 with
  | Single i ->
      let b = Nat (i,Closed) in
      b,b
  | Bound b1 ->
      dots buf ;
      let b2 = high buf in
      b1,b2
}
