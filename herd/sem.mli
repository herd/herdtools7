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

(** Semantics of instructions *)

module type Config = sig
  val moreedges : bool
  include SemExtra.Config
end

module type Semantics =
  sig
    include SemExtra.S

(* Barrier pretty print (for minimal model) *)
    val barriers : pp_barrier list
    val isync : pp_barrier option
(* Extra condition on RMW *)
    val atomic_pair_allowed : event -> event -> bool
(* Instruction semantics, highly arch dependant *)
    module Mixed(SZ:ByteSize.S) : sig 
      val build_semantics : A.inst_instance_id -> (A.program_order_index * branch) M.t
    end
  end

