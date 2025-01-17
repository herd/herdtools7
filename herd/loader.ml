(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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

let func_size = Pseudo.func_size
let proc_size = Pseudo.proc_size
let page_size = Pseudo.page_size

let func_start_addr proc = function
  | MiscParser.Main -> (proc + 1) * proc_size
  | MiscParser.FaultHandler -> (proc + 1) * proc_size + func_size

module type S = sig
  type nice_prog
  type program
  type start_points
  type code_segment

  val load : nice_prog -> program * start_points * code_segment
end

module Make(A:Arch_herd.S) =
struct

  type nice_prog = A.nice_prog
  type program = A.program
  type start_points = A.start_points
  type code_segment = A.code_segment

  let next_addr_after_pagealign addr =
    let addr_part = addr mod proc_size in
    let proc_part = addr - addr_part in
    proc_part + ((addr_part / page_size) + (if addr_part mod page_size = 0 then 0 else 1) ) * page_size

  let preload_labels proc = fun m addr ->
    let add_label lbl addr m =
      if Label.Map.mem lbl m then
        Warn.user_error
          "Label %s occurs more that once" lbl ;
      Label.Map.add lbl (proc,addr) m in
    (A.fold_label_addr add_label) m addr

  let preload =
    List.fold_left
      (fun m ((proc,_,func),code) ->
        let addr = func_start_addr proc func in
        preload_labels proc m addr code)
      Label.Map.empty

  let convert_lbl_to_offset proc pc mem instr =
    let labelmap =
      let open BranchTarget in
      function
      | Lbl l ->
         let tgt_proc, tgt_addr =
           try Label.Map.find l mem
           with Not_found ->
             Warn.user_error
               "Label %s not found on %s, although used in the instruction %s"
               (Label.pp l)
               (Proc.pp proc)
               (A.dump_instruction instr) in
         if Proc.equal tgt_proc proc then
           Offset (tgt_addr - pc)
         else
           Warn.user_error
             "%s cannot refer to %s defined by %s, use register with initial value %s"
             (Proc.pp proc) (Label.pp l)
             (Proc.pp tgt_proc) (Label.Full.pp (tgt_proc,l))
    | Offset _ as x -> x in
    A.map_labels_base labelmap instr

  let rec load_code proc addr mem = function
    | [] -> []
    | ins::code -> load_ins proc addr mem code ins

  and load_ins proc addr mem code = function
    | A.Nop ->
       load_code proc addr mem code
    | A.Instruction ins ->
        let start =
          load_code proc (addr+A.size_of_ins ins) mem code in
        let new_ins =
          convert_lbl_to_offset proc addr mem ins in
        (addr,new_ins)::start
    | A.Label (_,A.Nop) ->
        load_code proc addr mem code
    | A.Label (_,_) -> assert false (* Expected to have been normalised already! *)
    | A.Symbolic _
    | A.Macro (_,_) -> assert false
    | A.Pagealign ->
      assert false
    | A.Skip n ->
      let new_addr = addr + n in
      load_code proc new_addr mem code

  let make_padding old_addr new_addr =
    let offset = new_addr - old_addr in
    let immbranch_v = Option.get (A.mk_imm_branch offset) in
    let immbranch_sz = A.size_of_ins immbranch_v in
    assert (offset mod 4 = 0); (* FIXME: this check will fail when page alignment is implemented on architectures where instructions are not all 32-bit-sized *)
    if offset == 0 then
      []
    else if offset == immbranch_sz then
      [(A.Instruction (immbranch_v))]
    else if offset > immbranch_sz then
      [(A.Instruction (immbranch_v)); A.Skip (offset-immbranch_sz)]
    else
      (* The case of "offset < immbranch_sz" should not be possible on
       * archictectures where page alignment is currently supported *)
      assert false

  let rec normalise_code addr = function
  | [] -> []
  | pseudoins::code -> normalise_ins addr code pseudoins

  and normalise_ins addr code pseudo_ins =
    match pseudo_ins with
    | A.Nop ->
      A.Nop :: (normalise_code addr code)
    | A.Instruction ins ->
      let next_addr = addr + (A.size_of_ins ins) in
      A.Instruction ins :: (normalise_code next_addr code)
    | A.Label (lbl,pseudo_ins) ->
        let next_code = match pseudo_ins with
        | A.Nop -> code
        | _ -> pseudo_ins::code
        in
        A.Label (lbl, A.Nop) :: (normalise_code addr next_code)
    | A.Pagealign -> begin
      try
        let new_addr = next_addr_after_pagealign addr in
        let padding = make_padding addr new_addr in
        normalise_code addr (padding @ code)
      with Invalid_argument _ -> Warn.fatal "Error in pre-processing litmus test"
      end
    | A.Skip n ->
      let next_addr = addr + n in
      (A.Skip n) :: (normalise_code next_addr code)
    | A.Symbolic _
    | A.Macro (_,_) -> assert false

  and normalise_prog = function
  | [] -> []
  | ((proc,foo,func),code)::pseudo_prog ->
    let addr = func_start_addr proc func in
    ((proc, foo, func),normalise_code addr code)::(normalise_prog pseudo_prog)

  let rec mk_rets_from_starts proc addr rets start =
    match start with
    | [] ->
      IntMap.add addr (proc,[]) rets
    | (addr, ins)::start_tl ->
      let ins_sz = A.size_of_ins ins in
      let new_rets = IntMap.add addr (proc,start) rets in
      mk_rets_from_starts proc (addr+ins_sz) new_rets start_tl


  let load pseudo_prog =
    let pseudo_prog = normalise_prog pseudo_prog in
    let mem = preload pseudo_prog in
    let rec load_iter = function
      | [] -> [],IntMap.empty
      | ((proc,_,func),code)::pseudo_prog_tl ->
         let starts,rets = load_iter pseudo_prog_tl in
         let addr = func_start_addr proc func in
         let start = load_code proc addr mem code in
         let fin_rets = mk_rets_from_starts proc addr rets start in
         (proc,func,start)::starts,fin_rets in
    let starts,code_segments = load_iter pseudo_prog in
    let mains,fhandlers =
      List.partition (fun (_,func,_) -> func=MiscParser.Main) starts in
    let add_fhandler (proc,_,start) =
      let fhandler =
        List.find_opt (fun (p,_,_) -> Proc.equal p proc) fhandlers in
      match fhandler with
      | Some (_,_,fh_start) ->
         (proc,start,Some fh_start)
      | None -> (proc,start,None) in
    let starts = List.map add_fhandler mains in
    let prog = Label.Map.map snd mem in
    prog,starts,code_segments

end
