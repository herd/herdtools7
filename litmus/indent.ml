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

type t = string

let as_string s = s

let indent0 = ""
let indent = "  "
let tab s = s ^ indent
let indent2 = tab indent
let indent3 = tab indent2
let indent4 = tab indent3
let indent5 = tab indent4

module type S = sig
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

module Make (Chan : sig val hexa : bool val out : out_channel end) = struct
  open Printf
  let hexa = Chan.hexa
  let out = Chan.out

  let fprintf fmt = Printf.fprintf Chan.out fmt

  let fx indent fmt =
    output_string Chan.out indent ;    
    kfprintf (fun chan -> output_char chan '\n')
      Chan.out fmt

  let f fmt = fx indent0 fmt
  let fi fmt = fx indent fmt
  let fii fmt = fx indent2 fmt
  let fiii fmt = fx indent3 fmt
  let fiv fmt = fx indent4 fmt
  let fv fmt = fx indent5 fmt

  let output s = output_string Chan.out s

  let ox i s =
   output_string Chan.out i ;
   output_string Chan.out  s ;
   output_char Chan.out '\n' 

  let oy i s =
   output_string Chan.out i ;
   output_string Chan.out indent ;
   output_string Chan.out  s ;
   output_char Chan.out '\n' 

  let o s =
   output_string Chan.out  s ;
   output_char Chan.out '\n' 

  let oi s = ox indent s 
  let oii s = ox indent2 s
  let oiii s = ox indent3 s
  let oiv s = ox indent4 s
  let ov s = ox indent5 s

end
