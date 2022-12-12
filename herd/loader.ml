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
  type code_segment

  val load : nice_prog -> program * start_points * code_segment
end

let func_size = 1000
let proc_size = 10000

let func_start_addr proc = function
  | MiscParser.Main -> (proc + 1) * proc_size
  | MiscParser.FaultHandler -> (proc + 1) * proc_size + func_size

module Make(A:Arch_herd.S) =
struct

  type nice_prog = A.nice_prog
  type program = A.program
  type start_points = A.start_points
  type code_segment = A.code_segment

  let rec load_code proc addr mem rets = function
    | [] ->
       mem,[],IntMap.add addr (proc,[]) rets
    | ins::code ->
      load_ins proc addr mem rets code ins

  and load_ins proc addr mem rets code = fun x ->
    match x with
    | A.Nop ->
      load_code proc addr mem rets code
    | A.Instruction ins ->
        let new_mem,start,new_rets =
          load_code proc (addr+4) mem rets code in
        let new_start = (addr,ins)::start in
        let newer_rets = IntMap.add addr (proc,new_start)  new_rets in
        new_mem,new_start,newer_rets
    | A.Label (lbl,ins) ->
        let mem,start,new_rets =
          load_ins proc addr mem rets code ins in
        if Label.Map.mem lbl mem then
          Warn.user_error
            "Label %s occurs more that once" lbl ;
        Label.Map.add lbl addr mem,start,new_rets
    | A.Symbolic _
    | A.Macro (_,_) -> assert false

  let load prog =
    let rec load_iter = function
    | [] -> Label.Map.empty,[],IntMap.empty
    | ((proc,_,func),code)::prog ->
       let mem,starts,rets = load_iter prog in
       let addr = func_start_addr proc func in
       let fin_mem,start,fin_rets = load_code proc addr mem rets code in
       fin_mem,(proc,func,start)::starts,fin_rets in
    let mem,starts,codes = load_iter prog in
    let mains,fhandlers =
      List.partition (fun (_,func,_) -> func=MiscParser.Main) starts in
    let add_fhandler (proc,_,start) =
      let fhandler = List.find_opt (fun (p,_,_) -> p=proc) fhandlers in
      match fhandler with
      | Some (_,_,fh_start) ->
         (proc,start,Some fh_start)
      | None -> (proc,start,None) in
    mem,List.map add_fhandler mains,codes

end
