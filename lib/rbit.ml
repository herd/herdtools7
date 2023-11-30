(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Reverse bits *)

module type I = sig
    type t
    val zero : t
    val shift_left : t  -> int -> t
    val shift_right_logical : t -> int -> t
    val bit_at : int -> t -> t
    val logor : t -> t -> t
  end

module Make(I:I) :
sig
  val rbit : MachSize.sz -> I.t -> I.t
end =
  struct
    let rec rbit_rec acc k x =
      if k <= 0 then acc
      else
        let b = I.bit_at 0 x in
        let acc = I.shift_left acc 1 in
        let acc = I.logor acc b
        and x = I.shift_right_logical x 1 in
        rbit_rec acc (k-1) x

    let rbit sz x = rbit_rec I.zero (MachSize.nbits sz)  x
  end
