(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


type ckpt

module Make(C:AutoConf.S) : sig

  val restore_explo : ckpt -> unit
  val go_explo : unit -> unit

  val restore_conform : ckpt -> unit
  val go_conform : unit -> unit

end
