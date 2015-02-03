(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* General indented printers *)

type t
val as_string : t -> string
val tab : t -> t
val indent0 : t
val indent : t
val indent2 : t
val indent3 : t
val indent4 : t
val indent5 : t

module type S = 
  sig
    val hexa : bool
    val out : out_channel

    val fprintf :  ('a, out_channel, unit, unit) format4 -> 'a
    val fx : t ->  ('a, out_channel, unit, unit) format4 -> 'a
    val f :  ('a, out_channel, unit, unit) format4 -> 'a
    val fi : ('a, out_channel, unit, unit) format4 -> 'a
    val fii : ('a, out_channel, unit, unit) format4 -> 'a
    val fiii : ('a, out_channel, unit, unit) format4 -> 'a
    val fiv : ('a, out_channel, unit, unit) format4 -> 'a
    val fv : ('a, out_channel, unit, unit) format4 -> 'a

    val output : string -> unit
    val ox : t -> string -> unit
    val oy : t -> string -> unit        
    val o : string -> unit
    val oi : string -> unit
    val oii : string -> unit
    val oiii : string -> unit
    val oiv : string -> unit
    val ov : string -> unit
  end

module Make : functor (Chan : sig val hexa : bool val out : out_channel end) -> S
