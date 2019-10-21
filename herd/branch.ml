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
  type v
(* t monad controls the branch machinery *)
  type 'a monad

(* Branch information, result of our instruction semantics *)
  type t =
    (* continue in sequence *)
    | Next
    (* jump to arg *)
    | Jump of lbl
    (* if v is one, jump to address, otherwise continue in sequence *)
    | CondJump of v * lbl
    (* Stop now *)
    | Exit

(* Next instruction in sequence *)
  val nextT : t monad
(* Non-conditional branch *)
  val branchT : lbl ->  t monad
(* Conditional branch *)
  val bccT : v -> lbl -> t monad
end

module Make(M:Monad.S) = struct

  type lbl = string
  type v = M.A.V.v
  type 'a monad = 'a M.t

  type t =
    (* continue in sequence *)
    | Next
    (* jump to arg *)
    | Jump of lbl
    (* if v is one, jump to address, otherwise continue in sequence *)
    | CondJump of v * lbl
    (* Stop now *)
    | Exit

(* Utilities *)


  let nextT = M.unitT Next
  let branchT lbl = M.unitT (Jump lbl)
  let bccT v lbl = M.unitT (CondJump (v,lbl))
end
