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
  type cofeat
  val no_cofeat : cofeat
  val pp_cofeat : cofeat -> string
  val cofeat_sets : (string * (cofeat -> bool)) list
end

(* Default setting: all accesses are explicit *)
module No : S with type cofeat=unit