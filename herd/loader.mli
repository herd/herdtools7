(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** "load" program in memory, somehow abstracted *)

module type S = sig
  type nice_prog
  type program
  type start_points

  val load : nice_prog -> program * start_points
end

module Make : functor (A:Arch.S) -> S
with type nice_prog = A.nice_prog
and type program = A.program
and type start_points = A.start_points
