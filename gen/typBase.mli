(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Base type for produced tests *)

type sgn = Signed | Unsigned
type t =   Int | Std of sgn * MachSize.sz

val tags : string list

val parse : string -> t option

val pp : t -> string

val default : t
val is_default : t -> bool
val get_size : t -> MachSize.sz
