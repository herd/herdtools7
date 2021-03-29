(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make :
    functor (I:sig type elt end) ->
      sig
        type elt = I.elt
        type elt_hashed = elt Hashcons.hash_consed

        type t = t_node Hashcons.hash_consed
        and t_node =
          | Nil
          | Cons of elt_hashed * t


        val as_hash : t -> int

        val nilp : t -> bool

        val nil : t
        val cons : elt_hashed -> t -> t

        val iter : (elt_hashed -> unit) -> t -> unit
        val map : (elt_hashed -> 'a) -> t -> 'a list
        val pp : (elt_hashed -> string) -> t -> string
      end
