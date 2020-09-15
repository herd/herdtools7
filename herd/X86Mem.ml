(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Entry to models for X86  *)

module type Config = sig
  val model : Model.t
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:X86Barrier.S with type a = S.barrier)
 :
    XXXMem.S with
module S = S
    =
  struct

    open Model

    let model = O.model

    module S = S

    module ModelConfig = (O : Model.Config)

    let check_event_structure test = match O.model with
    | Minimal uni ->
        let module X =
          Minimal.Make
            (struct
              let uniproc = uni
              include ModelConfig
            end)
            (S) in
        X.check_event_structure test
    | Generic m ->
        let module X =
          MachModelChecker.Make
            (struct
              let m = m
              let bell_model_info = None
              include ModelConfig
             end)(S) in
        X.check_event_structure test
    | File _ -> assert false
    | m ->
        Warn.fatal "Model %s not implemented for X86" (Model.pp m)
end
