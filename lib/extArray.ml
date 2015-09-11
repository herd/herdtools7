(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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
