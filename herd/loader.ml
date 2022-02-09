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
  type return_labels

  val load : nice_prog -> program * start_points * return_labels
end

module Make(A:Arch_herd.S) : S
  with type nice_prog = A.nice_prog
   and type program = A.program
   and type start_points = A.start_points
   and type return_labels = A.return_labels =

struct

  type nice_prog = A.nice_prog
  type program = A.program
  type start_points = A.start_points
  type return_labels = A.return_labels

  let rec load_code proc addr mem rets link_num = function
    | [] -> mem,[],rets,link_num
    | ins::code ->
      load_ins proc addr mem rets link_num code ins

  and load_ins proc addr mem rets link_num code = fun x ->
    match x with
    | A.Nop ->
      load_code proc addr mem rets link_num code
    | A.Instruction ins ->
      if Misc.is_some (A.is_link ins) then
        let new_mem,start,new_rets,new_num =
          load_code proc (addr+4) mem rets (link_num+1) code in
        let lbl = Printf.sprintf "##%d" link_num in
        let newer_mem =
            if Label.Map.mem lbl new_mem then
              Warn.user_error
                "Label %s cannot be created, since it is reserved internally" lbl ;
            Label.Map.add lbl (proc,start) new_mem in
        let newer_rets = IntMap.add addr lbl new_rets in
        newer_mem,(addr,ins)::start,newer_rets,new_num
      else
        let mem,start,new_rets,new_num =
          load_code proc (addr+4) mem rets link_num code in
        mem,(addr,ins)::start,new_rets,new_num
    | A.Label (lbl,ins) ->
        let mem,start,new_rets,new_num =
          load_ins proc addr mem rets link_num code ins in
        if Label.Map.mem lbl mem then
          Warn.user_error
            "Label %s occurs more that once" lbl ;
        Label.Map.add lbl (proc,start) mem,start,new_rets,new_num
    | A.Symbolic _
    | A.Macro (_,_) -> assert false

  let load prog =
    let rec load_iter num = function
    | [] -> Label.Map.empty,[],IntMap.empty,num
    | ((proc,_),code)::prog ->
      let addr = 1000 * (proc+1) in
      let mem,starts,rets,new_num = load_iter num prog in
      let fin_mem,start,fin_rets,fin_num = load_code proc addr mem rets new_num code in
      fin_mem,(proc,start)::starts,fin_rets,fin_num in
    let mem, starts, rets, _ = load_iter 0 prog in
    mem, starts, rets

end
