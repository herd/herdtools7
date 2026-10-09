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
  type annotated_prog
  type program
  type start_points
  type code_segment

  val load : nice_prog -> program * start_points * code_segment * annotated_prog
end

module Make(A:Arch_herd.S) =
struct

  type nice_prog = A.nice_prog
  type annotated_prog = A.annotated_prog
  type program = A.program
  type start_points = A.start_points
  type code_segment = A.code_segment

  type 'ins loader_info = {
    addr : int;
    normalised_padding : A.instruction A.kpseudo list option;
    ins : 'ins A.kpseudo;
  }

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

  let rec load_code ~proc ~addr ~static_poi mem = function
    | [] -> []
    | ins::code -> load_ins ~proc ~addr ~static_poi mem code ins

  and load_ins ~proc ~addr ~static_poi mem code = function
    | A.Nop ->
       load_code ~proc ~addr ~static_poi mem code
    | A.Instruction ins ->
        let start =
          load_code ~proc ~addr:(addr+A.size_of_ins ins) ~static_poi:(static_poi+1) mem code in
        let new_ins =
          convert_lbl_to_offset proc addr mem ins in
        let code_ins = A.CodeInstr.{ instr = new_ins; static_poi; } in
        (addr,code_ins)::start
    | A.Label (_,A.Nop) ->
        load_code ~proc ~addr ~static_poi mem code
    | A.Label (_,_) -> assert false (* Expected to have been normalised already! *)
    | A.Symbolic _
    | A.Macro (_,_) -> assert false
    | A.Pagealign ->
      assert false
    | A.Skip n ->
      let new_addr = addr + n in
      load_code ~proc ~addr:new_addr ~static_poi mem code

  let make_padding old_addr new_addr =
    let offset = new_addr - old_addr in
    let immbranch_v =
      match A.mk_imm_branch offset with
      | Some v -> v
      | None -> Warn.fatal "Error in pre-processing litmus test employing page alignment syntax"
    in
    let immbranch_sz = A.size_of_ins immbranch_v in
    (* NOTE: the following check will fail once page alignment is implemented on
    architectures where instructions are not all 32-bit-sized *)
    assert (offset mod 4 = 0);
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
    let no_padding ins = { addr; normalised_padding = None; ins; } in
    match pseudo_ins with
    | A.Nop ->
      no_padding A.Nop :: normalise_code addr code
    | A.Instruction ins ->
      let next_addr = addr + (A.size_of_ins ins) in
      no_padding pseudo_ins :: normalise_code next_addr code
    | A.Label (lbl,pseudo_ins) ->
        let next_code = match pseudo_ins with
        | A.Nop -> code
        | _ -> pseudo_ins::code
        in
        no_padding (A.Label (lbl,A.Nop)) :: normalise_code addr next_code
    | A.Pagealign ->
        let new_addr = next_addr_after_pagealign addr in
        let padding = make_padding addr new_addr in
        (* Keep the directive in the source view; expand its padding only
           when producing executable code, even when the padding is empty. *)
        { addr; normalised_padding = Some padding; ins = A.Pagealign; }
        :: normalise_code new_addr code
    | A.Skip n ->
      let next_addr = addr + n in
      no_padding pseudo_ins :: normalise_code next_addr code
    | A.Symbolic _
    | A.Macro (_,_) -> assert false

  let normalise_prog =
    List.map
      (fun (((proc,_,func) as p),code) ->
        let addr = func_start_addr proc func in
        p,normalise_code addr code)

  (* check whether any Main instruction enters into the fault handler addr space,
     since it's only adding a fixed offset *)
  let check_handler_overlap prog =
    let handlers =
      List.fold_left
        (fun handlers ((proc,_,func),_) ->
          match func with
          | MiscParser.Main -> handlers
          | MiscParser.FaultHandler -> IntSet.add proc handlers)
        IntSet.empty prog in
    List.iter
      (fun ((proc,_,func),code) ->
        if func = MiscParser.Main && IntSet.mem proc handlers then begin
          let handler_addr = func_start_addr proc MiscParser.FaultHandler in
          let check_ins addr = function
            | A.Instruction ins ->
                let next_addr = addr + A.size_of_ins ins in
                if addr >= handler_addr || next_addr > handler_addr then
                  Warn.user_error
                    "Main code for %s overlaps its fault handler at address %d (instruction at address %d)"
                    (Proc.pp proc) handler_addr addr;
                next_addr
            | A.Skip n -> addr + n
            | A.Nop | A.Label (_,A.Nop) -> addr
            | A.Label (_,_) | A.Pagealign | A.Symbolic _ | A.Macro _ ->
                assert false in
          List.iter
            (fun { addr; normalised_padding; ins; } ->
              match normalised_padding with
              | Some padding -> ignore (List.fold_left check_ins addr padding)
              | None -> ignore (check_ins addr ins))
            code
        end)
      prog

  let expand_padding =
    List.map
      (fun (proc,code) ->
        let code = List.concat_map
          (fun { normalised_padding; ins; _ } ->
            match normalised_padding with
            | Some padding -> padding
            | None -> [ins]) code in
        proc,code)

  let annotate_prog code_segments =
    List.map
      (fun (proc,code) ->
        let code = List.map
          (fun { addr; ins; _ } ->
            A.pseudo_map
             (fun instr ->
               let _,code = IntMap.find addr code_segments in
               match code with
               | (_,code_ins)::_ -> A.CodeInstr.{code_ins with instr;}
               | [] -> assert false) ins) code in
        proc,code)

  let rec mk_rets_from_starts proc addr rets start =
    match start with
    | [] ->
      (* The end of main code can coincide with the handler's first instruction. *)
      if IntMap.mem addr rets then rets
      else IntMap.add addr (proc,[]) rets
    | (addr, ins)::start_tl ->
      let ins_sz = A.size_of_ins ins.A.CodeInstr.instr in
      let new_rets = IntMap.add addr (proc,start) rets in
      mk_rets_from_starts proc (addr+ins_sz) new_rets start_tl


  let load pseudo_prog =
    let normalised_prog = normalise_prog pseudo_prog in
    check_handler_overlap normalised_prog;
    let pseudo_prog = expand_padding normalised_prog in
    let mem = preload pseudo_prog in
    let rec load_iter = function
      | [] -> [],IntMap.empty
      | ((proc,_,func),code)::pseudo_prog_tl ->
         let starts,rets = load_iter pseudo_prog_tl in
         let addr = func_start_addr proc func in
         let start = load_code ~proc ~addr ~static_poi:0 mem code in
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
    let annotated_prog = annotate_prog code_segments normalised_prog in
    prog,starts,code_segments,annotated_prog

end
