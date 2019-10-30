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
  val parse_opt : string -> O.t option ref ->  string -> string * Arg.spec * string
  val parse_fun : string -> (O.t -> unit) -> string  -> string * Arg.spec * string
end

module type OptS = sig
  include Opt
  val compare : t -> t -> int
  val setnow : t -> bool (* examine tag for immediate action *)
end

module MakeS : functor (O:OptS) -> sig
    val parse_tag_set : string -> (O.t -> bool) ref -> string -> unit
    val parse : string -> (O.t -> bool) ref -> string -> string * Arg.spec * string
  end
