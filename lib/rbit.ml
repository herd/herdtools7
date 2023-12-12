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

(** Reverse bits and bytes *)

module type I = sig
    type t
    val zero : t
    val one : t
    val pp : bool -> t -> string
    val shift_left : t  -> int -> t
    val shift_right_logical : t -> int -> t
    val bit_at : int -> t -> t
    val logor : t -> t -> t
    val mask : MachSize.sz -> t -> t
end

module Make(I:I) :
sig
  val rbit : MachSize.sz -> I.t -> I.t
  val revbytes : MachSize.sz -> MachSize.sz -> I.t -> I.t
end =
  struct

    let rec rev_rec extract sz dst k src =
      if k <= 0 then dst
      else
        let b = extract src in
        let dst = I.shift_left dst sz in
        let dst = I.logor dst b in
        let src = I.shift_right_logical src sz in
        rev_rec extract sz dst (k-sz) src

    let rbit sz src = rev_rec (I.bit_at 0) 1 I.zero (MachSize.nbits sz)  src

    let rbytes sz src = rev_rec (I.mask MachSize.Byte)  8 I.zero sz src

    let revbytes csz sz x =
      let mask x = I.mask csz x in
      let csz = MachSize.nbits csz
      and sz = MachSize.nbits sz in
      assert (csz <= sz) ;
      let rec loop acc c =
        if c < 0 then acc
        else
          let elem =
            I.shift_right_logical x (c*csz)
            |> mask
            |> rbytes csz in
          let acc =
            I.shift_left elem (c*csz)
            |> I.logor acc in
          loop acc (c-1) in
      loop I.zero (sz/csz-1)

  end
