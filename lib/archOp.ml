(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Operations that are arch specific *)

module type S = sig
  type op1
  val pp_op1 : bool (* hexa *) -> op1 -> string

  type scalar
  type pteval
  type cst = (scalar,pteval) Constant.t

  (* Specific operations *)
  val do_op1 : op1 -> cst -> cst option

  (******************************************)
  (* Particular cases of generic operations *)
  (******************************************)

  (* Compute page key *)
  val shift_address_right : string -> scalar -> cst option

  (* Computing on page table entries *)
  val orop : pteval -> scalar -> pteval option
  val andnot2 : pteval -> scalar -> pteval option

  (* Masking some structured constant *)
  val mask : cst -> MachSize.sz -> cst option
end

module No(Cst:Constant.S) = struct
  type op1
  let pp_op1 _hexa _ = assert false

  type scalar = Cst.Scalar.t
  type pteval = Cst.PteVal.t
  type cst = (scalar,pteval) Constant.t

  let do_op1 _ _ = None
  let shift_address_right _ _ = None
  let orop _ _ = None
  let andnot2 _ _ = None
  let mask _ _ = None
end
