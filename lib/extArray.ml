(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Extensible arrays *)

type 'a t =
  { mutable t : 'a array ; mutable next : int ; }

let create () = { t = [||]; next=0; }

let resize t =
  let sz = Array.length t.t in
  let nsz = 2 * sz + 1 in
  let nt = Array.make nsz (Obj.magic 0) in
  Array.blit t.t 0 nt 0 sz ;
  t.t <- nt

let rec resize_for t nadd =
  if t.next + nadd - 1 >= Array.length t.t then begin
    resize t ;
    resize_for t nadd
  end

let add t x =
  if t.next >= Array.length t.t then resize t ;
  t.t.(t.next) <- x ;
  t.next <- t.next+1

let blit t src idx sz =
  resize_for t sz ;
  Array.blit src idx t.t t.next sz ;
  t.next <- t.next + sz

let to_array t = Array.sub t.t 0 t.next
