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

(************************************************)
(* "load" program in memory, somehow abstracted *)
(************************************************)
module type S = sig
  type nice_prog
  type program
  type start_points

  val load : nice_prog -> program * start_points
end

module Make(A:Arch_herd.S) : S
with type nice_prog = A.nice_prog
and type program = A.program
and type start_points = A.start_points =

  struct

    type nice_prog = A.nice_prog
    type program = A.program
    type start_points = A.start_points

    let rec load_code addr mem code = match code with
    | [] -> mem,[]
    | ins::code ->
	load_ins addr mem ins code

    and load_ins addr mem ins code = match ins with
    | A.Nop -> load_code addr mem code 
    | A.Instruction ins ->
	let mem,start = load_code (addr+4) mem code in
	mem,(addr,ins)::start
    | A.Label (lbl,ins) ->
	let mem,start = load_ins addr mem ins code in
	if A.LabelMap.mem lbl mem then
	  Warn.user_error
	    "Label %s occurs more that once" lbl ;
	A.LabelMap.add lbl start mem,start
    | A.Symbolic _
    | A.Macro (_,_) -> assert false

    let rec load = function
    | [] -> A.LabelMap.empty,[]
    | ((proc,_),code)::prog ->
	let addr = 1000 * (proc+1) in
	let mem,starts = load prog in
	let mem,start = load_code addr mem code in
	mem,(proc,start)::starts

end
