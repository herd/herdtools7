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
let proc_size = 1000000

let align_to_next p addr =
  let addr_part = addr mod proc_size in
  let proc_part = addr - addr_part in
  let n = Int.shift_left 1 p in
  proc_part + ((addr_part / n) + (if addr_part mod n = 0 then 0 else 1) ) * n

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

  let preload_labels proc =
    let add_label lbl addr m =
      if Label.Map.mem lbl m then
        Warn.user_error
          "Label %s occurs more that once" lbl ;
      Label.Map.add lbl (proc,addr) m in
    A.fold_label_addr add_label

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

  let rec load_code proc addr mem rets = function
    | [] ->
       [],IntMap.add addr (proc,[]) rets
    | ins::code ->
       load_ins proc addr mem rets code ins

  and load_ins proc addr mem rets code = fun x ->
    (* Printf.printf "Loading addr=%d\n" addr; *)
    match x with
    | A.Nop ->
       load_code proc addr mem rets code
    | A.Instruction ins ->
        let start,new_rets =
          load_code proc (addr+A.size_of_ins ins) mem rets code in
        let new_ins =
          convert_lbl_to_offset proc addr mem ins in
        let new_start = (addr,new_ins)::start in
        let newer_rets = IntMap.add addr (proc,new_start)  new_rets in
        new_start,newer_rets
    | A.Label (_,ins) ->
        let start,new_rets = load_ins proc addr mem rets code ins in
        start,new_rets
    | A.Symbolic _
    | A.Macro (_,_) -> assert false
    | A.Align _ -> 
      load_code proc addr mem rets code
      (* let new_addr = align_to_next p addr in
      assert ((new_addr - addr) mod (A.size_of_ins A.mynop) = 0);
      let diff = (new_addr - addr)/(A.size_of_ins A.mynop) in
      let padding = List.init diff (fun _ -> A.Instruction A.mynop) in
      load_code proc addr mem rets (padding @ code) *)

  let rec preprocess_code addr = function 
  | [] -> []
  | ins::code -> preprocess_ins addr code ins

  and preprocess_ins addr code = function
    | A.Nop ->
       preprocess_code addr code
    | A.Instruction ins ->
        (A.Instruction ins) :: (preprocess_code (addr+A.size_of_ins ins) code)
    | A.Label (lbl,ins) ->
        let rec check_label = fun x ->
          match x with
          | A.Nop -> (None,None)
          | A.Instruction ins  -> (None,Some (A.size_of_ins ins))
          | A.Symbolic _  -> (None,None)
          | A.Macro (_,_) -> (None,None)
          | A.Align p -> (Some (align_to_next p addr),None)
          | A.Label (_,ins) ->
              check_label ins
        in
        (match check_label ins with
        | (Some _, Some _) -> assert false
        | (Some new_addr, None) -> begin
          assert ((new_addr - addr) mod (A.size_of_ins A.mynop) = 0);
          let diff = (new_addr - addr)/(A.size_of_ins A.mynop) in
          let padding = List.init diff (fun _ -> A.Instruction A.mynop) in
          (A.Label (lbl,ins)) :: preprocess_code addr (padding @ code)
          end
        | (None, Some sz) ->
          (A.Label (lbl,ins)) :: preprocess_code (addr+sz) code
        | (None, None) ->
          (A.Label (lbl,ins)) :: preprocess_code addr code
        )
    | A.Symbolic _
    | A.Macro (_,_) -> assert false
    | A.Align p -> 
      let new_addr = align_to_next p addr in
      assert ((new_addr - addr) mod (A.size_of_ins A.mynop) = 0);
      let diff = (new_addr - addr)/(A.size_of_ins A.mynop) in
      let padding = List.init diff (fun _ -> A.Instruction A.mynop) in
      (* Printf.printf "    thought the addr would be %d, but need to proceed from %d, the diff being %d\n" addr new_addr diff; *)
      (A.Align p) :: preprocess_code addr (padding @ code)
      (* load_code proc (align_to_next addr p) mem rets code *)
  and preprocess = function
  | [] -> []
  | ((proc,foo,func),code)::prog ->
    let addr = func_start_addr proc func in
    ((proc, foo, func),preprocess_code addr code)::(preprocess prog)

  let load prog =
    let prog = preprocess prog in
    let mem = preload prog in
    let rec load_iter = function
      | [] -> [],IntMap.empty
      | ((proc,_,func),code)::prog ->
         let starts,rets = load_iter prog in
         let addr = func_start_addr proc func in
         let start,fin_rets = load_code proc addr mem rets code in
         (proc,func,start)::starts,fin_rets in
    let starts,codes = load_iter prog in
    let mains,fhandlers =
      List.partition (fun (_,func,_) -> func=MiscParser.Main) starts in
    let add_fhandler (proc,_,start) =
      let fhandler =
        List.find_opt (fun (p,_,_) -> Proc.equal p proc) fhandlers in
      match fhandler with
      | Some (_,_,fh_start) ->
         (proc,start,Some fh_start)
      | None -> (proc,start,None) in
    Label.Map.map snd mem,List.map add_fhandler mains,codes

end
