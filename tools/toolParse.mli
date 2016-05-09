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

(***************************************)
(* Apply a function (zyva) to one test *)
(***************************************)

module Top :
    functor (T:sig type t end) -> (* Return type, must be abstracted *)
      functor (B: functor(A:ArchBase.S) ->
        (sig val zyva : Name.t -> A.pseudo MiscParser.t -> T.t end)) ->
sig
  val from_file : string -> T.t
end

module Tops :
    functor (T:sig type t end) -> (* Return type, must be abstracted *)
      functor (B: functor(A:ArchBase.S) ->
        (sig val zyva : ( Name.t * A.pseudo MiscParser.t) list -> T.t end)) ->
sig
  val from_files : string list -> T.t
end

