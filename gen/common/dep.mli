(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type S = sig
  type dp

  val equal_dp : dp -> dp -> bool
  val pp_dp : dp -> string
  val fold_dp : (dp -> 'a -> 'a) -> 'a -> 'a

  (* Defaults for backward compatibility *)
  val ddr_default : dp option
  val ddw_default : dp option
  val ctrlr_default : dp option
  val ctrlw_default : dp option

  (* Predicates *)
  val is_ctrlr : dp -> bool
  val is_addr : dp -> bool
  val is_data : dp -> bool

  (* Dependencies composition by sequence *)
  val fst_dp : dp -> dp list
  val sequence_dp : dp -> dp -> dp list
end

module No : sig
  include S
end

module Basic : sig
  type dp = ADDR | DATA | CTRL
  include S with type dp := dp
end

module Bell : sig
  type dp = ADDR | DATA | CTRL
  include S with type dp := dp
end

module Full : sig
  type dp = ADDR | DATA | CTRL | CTRLISYNC
  include S with type dp := dp
end

module AArch64 : sig
  type csel = OkCsel | NoCsel
  type dp = Full.dp * csel
  include S with type dp := dp
end
