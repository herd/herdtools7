(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


module type S = sig

(* Business as usual *)
  type  t
  val machsize : MachSize.sz

  val zero : t val one : t

  val of_string : string -> t
  val pp : bool -> t -> string
  val of_int : int -> t
  val to_int : t -> int (* Hum *)

  val compare : t -> t -> int

(* Operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val logor : t -> t -> t
  val logand : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> int -> t
  val shift_right_logical : t -> int -> t
  val addk : t -> int -> t
  val lt : t -> t -> bool
  val le : t -> t -> bool
  val mask : MachSize.sz -> t -> t
end
    
