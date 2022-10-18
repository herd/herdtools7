(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Explicit annotation for accesses, non-explicit accesses are more of less
    accesses added by, e.g. the virtual memory system. *)

module type S = sig
  type explicit
  val exp_annot : explicit
  val nexp_annot : explicit
  val is_explicit_annot : explicit -> bool
  val is_not_explicit_annot : explicit -> bool
  val pp_explicit : explicit -> string
  val explicit_sets : (string * (explicit -> bool)) list
end

(* Default setting: all accesses are explicit *)
module No : S with type explicit=unit

(* Default setting, action level *)
module NoAction : sig
  val is_explicit :'act -> bool
  val is_not_explicit :'act -> bool
end
