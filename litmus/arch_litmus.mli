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
  include ArchExtra_litmus.Config
end

(* Abstract signature of architectures *)

module type Base = sig

  val base_type : CType.t

  type reg
  type instruction

  val dump_instruction : instruction -> string

  module
    V : Constant.S
    with type Instr.t = instruction
     and type Instr.exec = instruction

  module RegSet : MySet.S with type elt = reg
  module RegMap : MyMap.S with type key = reg

  include Location.S

  with type loc_reg = reg and
  type loc_global = Global_litmus.t

  val is_pte_loc : location -> bool
  val location_of_addr : string -> location


  val parse_reg : string -> reg option
  val reg_compare : reg -> reg -> int

  type state = (location * V.v) list

  val debug_state : state -> string

  type fullstate = (location * (TestType.t * V.v)) list

  module Out : Target.S
  with type arch_reg = reg (* Out abstracted *)
  and module V = V

  val dump_loc_tag : location -> string
  val dump_rloc_tag : rlocation -> string

  val arch : Archs.t

  val find_in_state : location -> state -> V.v
  val pp_reg : reg -> string
  val type_reg : reg -> CType.t

  val features : ((instruction -> bool) * string) list
  val user_handler_clobbers : string list

  (* In the following two functions first argument specifies user mode *)
  val default_sync_handler : bool -> string
  val vector_table : bool -> string -> string list

(* Reaction to different types, fail or warn *)
  val error : (CType.t -> CType.t -> bool)
  val warn : (CType.t -> CType.t -> bool)

  module FaultType : FaultType.S

  module GetInstr : GetInstr.S with type t = instruction

end

module type K = sig
  include ArchBase.S
  module V : Constant.S

  module RegSet : MySet.S with type elt = reg

  include Location.S
  with type loc_reg = reg and
  type loc_global = string

  type state = (location * V.v) list
  type fullstate = (location * (TestType.t * V.v)) list

  module Out : Target.S
  with type arch_reg = reg (* Out abstracted *)

  val find_in_state : location -> state -> V.v
  val pp_reg : reg -> string
end



module type S =
  sig

    include ArchBase.S

    module
      V : Constant.S
      with type Instr.t = instruction
      and type Instr.exec = instruction

(* Reaction to different types, fail or warn *)
    val error : (CType.t -> CType.t -> bool)
    val warn : (CType.t -> CType.t -> bool)

    val reg_to_string : reg -> string

    include ArchExtra_litmus.S with module I.V = V
    and type I.arch_reg = reg

    val features : ((instruction -> bool) * string) list
    val nop : instruction

    include HardwareExtra.S

    module FaultType : FaultType.S

    module GetInstr : GetInstr.S with type t = instruction

  end
