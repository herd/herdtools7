(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  include ArchExtra.Config
end

(* Abstract signature of architectures *)

module type Base = sig
  module V : sig
    type v = Constant.v
    include Constant.S
    val maybevToV  : v -> v
  end

  type reg

  include Location.S
  with type loc_reg = reg and
  type loc_global = string

  val vToName : V.v -> loc_global

  val parse_reg : string -> reg option
  val reg_compare : reg -> reg -> int

  type state = (location * V.v) list
  type fullstate = (location * (MiscParser.run_type * V.v)) list

  module Out : Target.S with type arch_reg = reg (* Out abstracted *)

  val arch : Archs.t

  val find_in_state : location -> state -> V.v
  val pp_reg : reg -> string
end

module type S =
  sig
    include ArchBase.S
    module V :
        sig
          type v = Constant.v
          include Constant.S
          val maybevToV  : v -> v
        end

    val reg_to_string : reg -> string

    include ArchExtra.S with module I.V = V
    and type I.arch_reg = reg

  end
