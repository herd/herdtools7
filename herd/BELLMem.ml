(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Entry to models for BELL  *)

module type Config = sig
  val model : Model.t
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:BELLBarrier.S with type a = S.barrier)
 :
    XXXMem.S with
module S = S
    =
  struct

    open Model

    let model = O.model

(*
    let bell_model = match O.bell_model with
      | Some m -> m
      | None -> Warn.fatal "Running a bell test requires a .bell file"	
*)    

    module S = S

    module ModelConfig = (O : Model.Config)

    let check_event_structure test = match O.model with
    | Generic m ->
        let module X =
          BELLModelChecker.Make
            (struct
              let m = m
              include ModelConfig
             end)(S)(AllBarrier.FromBELL(B)) in
        X.check_event_structure test
    | File _ -> assert false
    | m ->
        Warn.fatal "Model %s not implemented for BELL" (Model.pp m)
end
