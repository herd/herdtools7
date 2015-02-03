(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(************************************************)
(* "load" program in memory, somehow abstracted *)
(************************************************)
module type S = sig
  type nice_prog
  type program
  type start_points

  val load : nice_prog -> program * start_points
end

module Make(A:Arch.S) : S
with type nice_prog = A.nice_prog
and type program = A.program
and type start_points = A.start_points =

  struct

    type nice_prog = A.nice_prog
    type program = A.program
    type start_points = A.start_points
    module V = A.V

    let rec load_code addr mem code = match code with
    | [] -> mem,[]
    | ins::code ->
	load_ins addr mem ins code

    and load_ins addr mem ins code = match ins with
    | A.Nop -> load_code addr mem code 
    | A.Instruction ins ->
	let mem,start = load_code (addr+4) mem code in
	mem,(SymbConstant.intToV addr,ins)::start
    | A.Label (lbl,ins) ->
	let mem,start = load_ins addr mem ins code in
	if A.LabelMap.mem lbl mem then
	  Warn.user_error
	    "Label %s occurs more that once" lbl ;
	A.LabelMap.add lbl start mem,start
    | A.Macro (_,_) -> assert false

    let rec load = function
    | [] -> A.LabelMap.empty,[]
    | (proc,code)::prog ->
	let addr = 1000 * (proc+1) in
	let mem,starts = load prog in
	let mem,start = load_code addr mem code in
	mem,(proc,start)::starts

end
