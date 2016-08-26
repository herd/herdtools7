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

(** Emit C printf  (or expand to emit functions) *)

module type S = sig
  type sfmt = string
  val fx : ?out:string -> Indent.t -> sfmt -> string list -> unit
  val f :  ?out:string -> sfmt -> string list -> unit
  val fi :  ?out:string -> sfmt -> string list -> unit
  val fii :  ?out:string -> sfmt -> string list -> unit
  val fiii :  ?out:string -> sfmt -> string list -> unit
  val fiv :  ?out:string -> sfmt -> string list -> unit
  val fv :  ?out:string -> sfmt -> string list -> unit
end
