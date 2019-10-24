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

(** Show dot files as Postscript, controlled by '-gv' option *)
module Generator : functor (O:PrettyConf.S) -> sig
  val generator : string
end

module Make : functor (O:PrettyConf.S)  -> sig
(* Fork a gv window to show that file *)
val show_file : string -> unit

(* Idem, but show the graph produced by the argument function *)
val show : (out_channel -> unit) -> unit

end
