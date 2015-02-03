(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
