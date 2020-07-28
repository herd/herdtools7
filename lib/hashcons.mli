(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Hash tables for hash-consing. Code borrowed to Jean-Christophe Filliatre,
   included by permission. *)

type 'a hash_consed = private { 
  hkey : int;
  tag : int;
  node : 'a }

type 'a t

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type key
    type node = key hash_consed
    type t
    val create : int -> t
    val clear : t -> unit
    val hashcons : t -> key -> node
    val iter : (node -> unit) -> t -> unit
  end

module Make(H : HashedType) : (S with type key = H.t)
