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

(** Map for renaming, also provides an order *)

type 'a t 

val empty : 'a t

val add_binding : 'a t -> string -> int -> 'a -> 'a t
val find_value : 'a t -> string -> 'a
val find_value_opt : 'a t -> string -> 'a option
val find_order : 'a t -> string -> int

val fold : (string -> ('a*int) -> 'b -> 'b) -> 'a t -> 'b -> 'b
