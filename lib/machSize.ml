(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*************************************** ******************************)

type endian = Little | Big

type sz = Byte | Short | Word | Quad


let pp = function
  | Byte -> "b"
  | Short -> "h"
  | Word -> "w"
  | Quad -> "q"

let nbytes = function
  | Byte -> 1
  | Short -> 2
  | Word -> 4
  | Quad -> 8

let nbits sz = nbytes sz * 8

(* Correct endianess *)
let swap16 x =
  let r = (x land 0xff) lsl 8 in
  let r = r lor (x land 0xff00) lsr 8 in
  r

(* k is total size (in bits) *)
let rec swap k x =
   if k <= 16 then swap16 x
   else
     let k2 = k / 2 in
     let mask = (1 lsl k2) - 1 in
     let r1 = swap k2 (x land mask)
     and r2 = swap k2 (x lsr k2) in
     (r1 lsl k2) lor r2


let tr_endian sz = match sz with
| Byte -> fun x -> x
| Short -> swap16
| Word|Quad -> swap (nbits sz)
