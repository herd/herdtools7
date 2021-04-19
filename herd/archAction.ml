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

(** Action that are arch specific *)

module type S = sig
  type t
  type v
  type loc
  type value_set
  type solution
  type arch_lannot
  type arch_explicit

  val pp : t -> string

  val get_lannot : t -> arch_lannot
  val get_explicit : t ->  arch_explicit
  val value_of : t -> v option
  val read_of : t -> v option
  val written_of : t -> v option
  val location_of : t -> loc option
  val is_store : t -> bool
  val is_load : t -> bool
  val get_size: t -> MachSize.sz
  val get_kind : t -> Access.t
  val undetermined_vars : t -> value_set
  val simplify_vars : solution -> t -> t
end

module type NoConf = sig
  type v
  type loc
  type value_set
  type solution
  type arch_lannot
  type arch_explicit
end

module No(C:NoConf) = struct
  type t
  type v = C.v
  type loc = C.loc
  type value_set = C.value_set
  type solution = C.solution
  type arch_lannot = C.arch_lannot
  type arch_explicit = C.arch_explicit

  let pp _ = assert false

  let get_lannot _ = assert false
  let get_explicit _ = assert false
  let read_of _ = assert false
  let written_of _ = assert false
  let value_of _ = assert false
  let location_of _ = assert false
  let is_store _ = assert false
  let is_load _ = assert false
  let get_size _ = assert false
  let get_kind _ = assert false
  let undetermined_vars _ = assert false
  let simplify_vars _ = assert false
end
