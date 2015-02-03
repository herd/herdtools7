(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

type t = string

let lab_count = ref 0 

let reset () = lab_count := 0

let next_label s =
  let x = !lab_count in
  incr lab_count ;
  sprintf "%s%02i" s x
 
let fail p = sprintf "Fail%i" p
and exit p = sprintf "Exit%i" p

type next = Next | To of t
