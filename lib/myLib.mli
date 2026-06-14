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

(** Resolve library file names against configured search paths. *)

module type Config = sig
  val includes : string list
  val env : string option
  val libdir : string
  val debug : bool
end

val pp_debug : string -> unit

module Make :
  functor (C:Config) ->
    sig
      val find : string -> string
      (** [find path] resolves [path] to a concrete the library file path.

          If [path] is implicit, it is searched in the current directory,
          {!C.includes}, {!C.env}, and finally {!C.libdir}. The first existing
          candidate is returned. Non-implicit paths are returned unchanged.

          @raise Misc.Fatal on implicit paths that cannot be resolved. *)
    end
