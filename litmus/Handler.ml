(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2022-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Arch dependent code for exception handlers *)

module type S = sig
  type ins

  val user_mode : ins list
  val kernel_mode : ins list

  val fault_handler_prologue : int option -> ins list
  val fault_handler_epilogue : ins list
end

module No(A:sig type ins end) =
struct
  type ins = A.ins

  let user_mode = [] and kernel_mode = []

  let fault_handler_prologue _ = []
  let fault_handler_epilogue = []
end
