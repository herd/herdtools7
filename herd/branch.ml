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

(** Branching in code *)

module type S = sig
  type lbl = string
(* From arch *)
  type reg
  type v
(* t monad controls the branch machinery *)
  type 'a monad

(* Branch information, result of our instruction semantics *)
  type t =
    (* continue in sequence, setting registers *)
    | Next of (reg * v) list
    (* jump to arg *)
    | Jump of lbl
    (* if v is one, jump to address, otherwise continue in sequence *)
    | CondJump of v * lbl
    (* Stop now *)
    | Exit
    (* Raise Fault *)
    | Fault of Dir.dirn
    (* Return from Fault Handler *)
    | FaultRet of lbl

(* Next instruction in sequence *)
  val nextT : t monad
  val next1T : unit -> t monad
  val next2T : (unit * unit) -> t monad
  val next3T : ((unit * unit) * unit) -> t monad
  val next4T : (((unit * unit) * unit) * unit) -> t monad
  val nextSetT : reg -> v -> t monad
(* Non-conditional branch *)
  val branchT : lbl ->  t monad
(* Conditional branch *)
  val bccT : v -> lbl -> t monad
  val faultRetT : lbl -> t monad
end


module Make(M:Monad.S) = struct

  type lbl = string
  type reg = M.A.reg
  type v = M.A.V.v
  type 'a monad = 'a M.t

  type t =
    (* continue in sequence *)
    | Next of (reg * v) list
    (* jump to arg *)
    | Jump of lbl
    (* if v is one, jump to address, otherwise continue in sequence *)
    | CondJump of v * lbl
    (* Stop now *)
    | Exit
    (* Raise Fault *)
    | Fault of Dir.dirn
    (* Return from Fault Handler *)
    | FaultRet of lbl

(* Utilities *)


  let nextT = M.unitT (Next [])
  let next1T () = nextT
  let next2T ((),()) = nextT
  let next3T (((),()),()) = nextT
  let next4T ((((),()),()), ()) = nextT
  let nextSetT r v = M.unitT (Next [r,v])
  let branchT lbl = M.unitT (Jump lbl)
  let bccT v lbl = M.unitT (CondJump (v,lbl))
  let faultRetT lbl = M.unitT (FaultRet lbl)
end
