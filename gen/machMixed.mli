(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  val naturalsize : MachSize.sz option
  val fullmixed : bool
end

type offset = int
type t = MachSize.sz * offset

val equal : t -> t -> bool

val overlap : t -> t -> bool

module Make :
  functor (C:Config) ->
  sig

    val pp_mixed : t -> string

    val fold_mixed : (t -> 'a -> 'a) -> 'a -> 'a

    val tr_value : MachSize.sz -> int -> int
  end

module type ValsConfig = sig
  val naturalsize : unit -> MachSize.sz
  val endian : Endian.t
end

module Vals :
  functor(C:ValsConfig) ->
  sig
    val overwrite_value :
      int (* old *) -> MachSize.sz -> offset -> int (* write *) -> int

    val extract_value : int -> MachSize.sz -> offset -> int

  end
