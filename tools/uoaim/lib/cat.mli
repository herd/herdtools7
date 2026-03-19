(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module SetExp : sig
  type t

  val inter : t list -> t
  val union : t list -> t
  val negate : t -> t
  val of_ident : string -> t
  val pp : Format.formatter -> t -> unit
end

type set_exp = SetExp.t

module RelExp : sig
  type t

  val seq : t list -> t
  val inter : t list -> t
  val union : t list -> t
  val plus : t -> t
  val invert : t -> t
  val of_ident : string -> t
  val as_ident : t -> string option
  val of_set : SetExp.t -> t
  val negate : t -> t
  val identifiers : t -> string list

  val inverted_idents : t -> string list
  (** [inverted_idents exp] returns all pure identifiers "x" in [exp] which
      would be rendered as "x^-1" when pretty-printing [exp]. *)

  val pp : Format.formatter -> t -> unit
end

type rel_exp = RelExp.t
