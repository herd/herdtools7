(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Name selection utilities *)

val parse_select : string list ref ->  Arg.key * Arg.spec * Arg.doc
val parse_names : string list ref ->  Arg.key * Arg.spec * Arg.doc
val parse_rename : string list ref ->  Arg.key * Arg.spec * Arg.doc
val parse_excl : string list ref ->  Arg.key * Arg.spec * Arg.doc
val parse_hexa : bool ref ->  Arg.key * Arg.spec * Arg.doc
val parse_int32 : bool ref ->  Arg.key * Arg.spec * Arg.doc

module Make :
  functor
   (I:sig
     val verbose : int
     val rename : string list
     val select : string list
     val names : string list
     val excl : string list
   end) ->
     sig
       val rename : string -> string
       val rename_opt : string -> string option
       val names : StringSet.t option
       val ok : string -> bool
     end
