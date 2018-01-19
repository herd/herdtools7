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

(** Check code w.r.t. bell definitions *)

module Make :
functor (O:sig val debug : bool val compat :bool end) ->
  functor (A:Arch_herd.S) ->
      functor
        (C:sig
          val info : BellModel.info option
          val get_id_and_list : A.instruction -> string * string list
          val set_list : A.instruction -> string list -> A.instruction
          val tr_compat : A.instruction -> A.instruction
        end) ->
  sig
    val check : A.pseudo MiscParser.t -> A.pseudo MiscParser.t
  end
