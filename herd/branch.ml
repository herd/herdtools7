(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

(* Utilities *)


  let nextT = M.unitT Next
  let branchT lbl = M.unitT (Jump lbl)
  let bccT v lbl = M.unitT (CondJump (v,lbl))
end

