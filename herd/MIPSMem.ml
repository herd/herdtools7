(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Entry to models for MIPS  *)

module type Config = sig
  val model : Model.t
  include Model.Config
end
module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:MIPSBarrier.S with type a = S.barrier)
 :
    XXXMem.S with

module S = S
    =
  struct

    open Model

    module S = S

    let model = O.model
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
    | CAV12 opt ->
        let module X = 
          CAV12.Make
            (struct
              let opt = opt
              include ModelConfig
            end)
            (S)
            (AllBarrier.FromMIPS(B)) in
        X.check_event_structure test        
    | Jade opt ->
        let module X = 
          Jade.Make
            (struct
              let opt = opt
              include ModelConfig
            end)
            (S)
            (AllBarrier.FromMIPS(B)) in
        X.check_event_structure test                
    | X86TSO ->
        let module X =
          X86TSO.Make(ModelConfig)(S)(AllBarrier.FromMIPS(B)) in
        X.check_event_structure test
    | Generic m ->
        let module X =
          MachModelChecker.Make
            (struct
              let m = m
              include ModelConfig
             end)(S) in
        X.check_event_structure test
    | File _ -> assert false
  end
