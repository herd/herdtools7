(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Semantics of CPP11 instructions *)

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module OpenCL = OpenCLArch.Make(C.PC)(V)
    module Act = OpenCLAction.Make(OpenCL)
    include SemExtra.Make(C)(OpenCL)(Act)
    let barriers = []
    let isync = None

(****************************)	  
(* Build semantics function *)
(****************************)	  

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>::) = M.(>>::)
    let (>>!) = M.(>>!)
    let (>>>) = M.(>>>)
		       
    let read_loc b s mo = M.read_loc (fun loc v -> Act.Access (Dir.R, loc, v, mo, s, b))
    let read_exchange vstored s mo =
      M.read_loc (fun loc v -> Act.RMW (loc,v,vstored,mo,s))
    let read_reg r ii = read_loc false OpenCL.S_workitem OpenCL.NA (A.Location_reg (ii.A.proc,r)) ii
    let read_mem b s mo a = read_loc b s mo (A.Location_global a)

    let write_loc s mo loc v ii =
      M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo, s, false)) ii >>! v
    let write_reg r v ii = write_loc OpenCL.S_workitem OpenCL.NA (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem s mo a  = write_loc s mo (A.Location_global a) 	     
		 
    let fetch_op op v s mo loc =
      M.fetch op v (fun v vstored -> Act.RMW (loc,v,vstored,mo,s))

    let rec build_semantics_expr e ii : V.v M.t = match e with
      | OpenCL.Econstant v -> 	
        M.unitT (V.maybevToV v)

      | OpenCL.Eregister reg ->
        read_reg reg ii 

      | OpenCL.Eassign (reg,e) ->
        (* TODO: This might not be right, since there is no
           sequence point at an assignment *)
        build_semantics_expr e ii >>= fun v ->
        write_reg reg v ii 

      | OpenCL.Eop (op, e1, e2) ->
        build_semantics_expr e1 ii >>| 
        build_semantics_expr e2 ii >>= fun (v1,v2) ->
        M.op op v1 v2

      | OpenCL.Estore(loc,e,mo,ms) ->
          build_semantics_expr loc ii >>| 
          build_semantics_expr e ii >>= fun (loc,v) ->
          write_mem ms mo loc v ii 

      | OpenCL.Eexchange(loc,e,mo,ms) ->
          build_semantics_expr loc ii >>| 
          build_semantics_expr e ii >>= fun (loc,v) ->
            read_exchange v ms mo (A.Location_global loc) ii

      | OpenCL.Efetch (op,loc,e,mo,ms) ->
          build_semantics_expr loc ii >>| 
          build_semantics_expr e ii >>= fun (loc,v) ->
            fetch_op op v ms mo (A.Location_global loc) ii

      | OpenCL.Eload(loc,mo,ms) ->
          build_semantics_expr loc ii >>= fun loc ->
          read_mem false ms mo loc ii 

      | OpenCL.Ecas(obj,exp,des,success,failure,ms,strong) ->
       (* Obtain location of "expected" value *)
        build_semantics_expr exp ii >>= fun loc_exp ->
       (* Obtain location of object *)
        build_semantics_expr obj ii >>= fun loc_obj ->
       (* Non-atomically read the value at "expected" location *)
        read_mem false OpenCL.S_all_svm_devices OpenCL.NA loc_exp ii >>*= fun v_exp ->
 (* Non-deterministic choice *)
        M.altT
           (read_mem true ms failure loc_obj ii >>*= fun v_obj ->
           (* For "strong" cas: fail only when v_obj != v_exp *)
           (if strong then M.neqT v_obj v_exp else M.unitT ()) >>= fun () -> 
           (* Non-atomically write that value into the "expected" location *)
           write_mem OpenCL.S_all_svm_devices OpenCL.NA loc_exp v_obj ii >>!
           V.intToV 0)
          (* Obtain "desired" value *)
          (build_semantics_expr des ii >>= fun v_des -> 
           (* Do RMW action on "object", to change its value from "expected"
              to "desired", using memory order "success" *)
           M.mk_singleton_es
             (Act.RMW (A.Location_global loc_obj,v_exp,v_des,success,ms)) ii >>!
           V.intToV 1)

      | OpenCL.Efence(rs,mo,ms) ->
	M.mk_singleton_es (Act.Fence (rs,mo,ms,Act.Normal_fence)) ii >>! V.intToV 0

      | OpenCL.Ecomma(e1,e2) ->
        build_semantics_expr e1 ii >>= fun _v ->
        build_semantics_expr e2 ii

      | OpenCL.Eparen e ->
        build_semantics_expr e ii

        
    let rec build_semantics ii : (A.program_order_index * B.t) M.t = match ii.A.inst with
      | OpenCL.Pblock insts -> 
        build_semantics_list insts ii 
      
      | OpenCL.Pexpr e ->
        build_semantics_expr e ii >>!
        (A.next_po_index ii.A.program_order_index, B.Next)

      | OpenCL.Pbarrier(lbl,r,ms) ->
        let lbl = Printf.sprintf "%s-%i" lbl ii.A.unroll_count in
        M.mk_singleton_es 
          (Act.Fence (r,OpenCL.Rel,ms,Act.Entry_fence lbl)) ii 
        >>> fun _ ->
	let ii' = 
          {ii with A.program_order_index = A.next_po_index ii.A.program_order_index;} 
        in 
        M.mk_singleton_es 
          (Act.Fence (r,OpenCL.Acq,ms,Act.Exit_fence lbl)) ii' 
        >>> fun _ ->
        M.unitT (A.next_po_index ii'.A.program_order_index, B.Next)
      
      | OpenCL.Pif (e,i1,i2) ->
        build_semantics_expr e ii >>> fun ret ->
        let ii' = 
          {ii with A.program_order_index = A.next_po_index ii.A.program_order_index;} 
        in
        let then_branch = build_semantics {ii' with A.inst = i1} in
        let else_branch = build_semantics {ii' with A.inst = i2} in
        M.choiceT ret then_branch else_branch
      
      | OpenCL.Pwhile (e,i1) ->
        if ii.A.unroll_count > 2 (* TODO: magic number, eek! *) then 
          ((* TODO: tooFar := true ; *) M.unitT (ii.A.program_order_index, B.Next)) 
        else
        build_semantics_expr e ii >>> fun ret ->
	let ii' = 
          {ii with A.program_order_index = A.next_po_index ii.A.program_order_index;} 
        in 
        let continue_loop = 
          build_semantics {ii' with A.inst = i1} >>> fun (prog_order, _branch) ->
          build_semantics {ii' with A.program_order_index = prog_order; 
                                    A.unroll_count = ii'.A.unroll_count + 1; } 
        in
        let exit_loop = M.unitT (ii'.A.program_order_index, B.Next) in 
        M.choiceT ret continue_loop exit_loop
        
    and build_semantics_list insts ii = match insts with
      | [] -> M.unitT (ii.A.program_order_index, B.Next)
      | inst :: insts ->
	let ii = {ii with A.inst=inst; } in
	build_semantics ii >>> fun (prog_order, _branch) -> 
        build_semantics_list insts {ii with A.program_order_index = prog_order;}
     
  end
