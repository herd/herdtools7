(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Parse tags *)

module type Opt = sig
  type t
  val tags : string list
  val parse : string -> t option
  val pp : t -> string
end

module Make : functor (O:Opt) -> sig
  val argfun : string -> (string -> bool) ->  (string -> unit)
  val parse_withfun : string -> (O.t -> unit) -> string -> O.t option -> string * Arg.spec * string
  val parse : string -> O.t ref -> string -> string * Arg.spec * string
end
