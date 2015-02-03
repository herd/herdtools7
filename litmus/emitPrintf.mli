(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
