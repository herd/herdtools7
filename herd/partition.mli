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

(** Imperative disjoint set data structure *)

module Make (O:Set.OrderedType) : sig
  type t

(* All creation steps must precede union/find operations *)
  val create : unit -> t
  val add : t -> O.t -> t

(* Union/Find *)
  val find : t -> O.t -> O.t
  val union : t -> O.t -> O.t ->  unit

(* Extract result *)
  module Sol : Map.S with type key = O.t
  val as_solution : t -> O.t Sol.t
end
