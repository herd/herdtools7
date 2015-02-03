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

    module CPP11 = CPP11Arch.Make(C.PC)(V)
    module Act = CPP11Action.Make(CPP11)
    include SemExtra.Make(C)(CPP11)(Act)
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
		       
    let read_loc mo = M.read_loc (fun loc v -> Act.Access (Dir.R, loc, v, mo))
    let read_exchange vstored mo =
      M.read_loc (fun loc v -> Act.RMW (loc,v,vstored,mo))

    let read_reg r ii = read_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) ii
    let read_mem mo a = read_loc mo (A.Location_global a)

    let write_loc mo loc v ii =
      M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo)) ii >>! v

    let write_reg r v ii = write_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem mo a  = write_loc mo (A.Location_global a) 	     
		 
    let fetch_op op v mo loc =
      M.fetch op v (fun v vstored -> Act.RMW (loc,v,vstored,mo))

    let rec build_semantics_expr e ii : V.v M.t = match e with
      | CPP11.Econstant v -> 	
        M.unitT (V.maybevToV v)

      | CPP11.Eregister reg ->
        read_reg reg ii 

      | CPP11.Eassign (reg,e) ->
        (* TODO: This might not be right, since there is no
           sequence point at an assignment *)
        build_semantics_expr e ii >>= fun v ->
        write_reg reg v ii 

      | CPP11.Eop (op, e1, e2) ->
        build_semantics_expr e1 ii >>| 
        build_semantics_expr e2 ii >>= fun (v1,v2) ->
        M.op op v1 v2

      | CPP11.Estore(loc,e,mo) ->
          build_semantics_expr loc ii >>| 
          build_semantics_expr e ii >>= fun (loc,v) ->
          write_mem mo loc v ii 

      | CPP11.Eexchange(loc,e,mo) ->
          build_semantics_expr loc ii >>| 
          build_semantics_expr e ii >>= fun (loc,v) ->
            read_exchange v mo (A.Location_global loc) ii

      | CPP11.Efetch (op,loc,e,mo) ->
          build_semantics_expr loc ii >>| 
          build_semantics_expr e ii >>= fun (loc,v) ->
            fetch_op op v mo (A.Location_global loc) ii

      | CPP11.Eload(loc,mo) ->
          build_semantics_expr loc ii >>= fun loc ->
          read_mem mo loc ii 

      | CPP11.Elock loc ->
          build_semantics_expr loc ii >>= fun loc ->
          M.altT
          (* successful attempt to obtain mutex *)
	    (M.mk_singleton_es (Act.Lock (A.Location_global loc, true)) ii >>! 
             V.intToV 0)
          (* unsuccessful attempt to obtain mutex *)
            (M.mk_singleton_es (Act.Lock (A.Location_global loc, false)) ii >>! 
             V.intToV 1)

      | CPP11.Eunlock loc ->
          build_semantics_expr loc ii >>= fun loc ->
	  M.mk_singleton_es (Act.Unlock (A.Location_global loc)) ii >>! 
          V.intToV 0

      | CPP11.Ecas(obj,exp,des,success,failure,strong) ->
       (* Obtain location of "expected" value *)
        build_semantics_expr exp ii >>= fun loc_exp ->
       (* Obtain location of object *)
        build_semantics_expr obj ii >>= fun loc_obj ->
       (* Non-atomically read the value at "expected" location *)
        read_mem CPP11.NA loc_exp ii >>*= fun v_exp ->
 (* Non-deterministic choice *)
        M.altT
           (read_mem failure loc_obj ii >>*= fun v_obj ->
           (* For "strong" cas: fail only when v_obj != v_exp *)
           (if strong then M.neqT v_obj v_exp else M.unitT ()) >>= fun () -> 
           (* Non-atomically write that value into the "expected" location *)
           write_mem CPP11.NA loc_exp v_obj ii >>!
           V.intToV 0)
          (* Obtain "desired" value *)
          (build_semantics_expr des ii >>= fun v_des -> 
           (* Do RMW action on "object", to change its value from "expected"
              to "desired", using memory order "success" *)
           M.mk_singleton_es
             (Act.RMW (A.Location_global loc_obj,v_exp,v_des,success)) ii >>!
           V.intToV 1)
(*
        (* Obtain location of "expected" value *)
        M.unitT (CPP11.maybev_to_location exp) >>= fun loc_exp ->
        (* Obtain location of object *) 
        M.unitT (CPP11.maybev_to_location obj) >>= fun loc_obj -> 
        (* Non-atomically read the value at "expected" location *)
        read_loc CPP11.NA loc_exp ii >>*= fun v_exp -> 
        (* Non-deterministic choice *)
        M.altT
          (* Read memory at location "object", using memory order "failure" *)
          (read_loc failure loc_obj ii >>*= fun v_obj ->
           (* For "strong" cas: fail only when v_obj != v_exp *)
           (if strong then M.addNeqConstraintT v_obj v_exp else (fun x -> x)) (
           (* Non-atomically write that value into the "expected" location *)
           write_loc CPP11.NA loc_exp v_obj ii) >>!
           V.intToV 0)
          (* Obtain "desired" value *)
          (build_semantics_expr des ii >>= fun v_des -> 
           (* Do RMW action on "object", to change its value from "expected"
              to "desired", using memory order "success" *)
           M.mk_singleton_es (Act.RMW (loc_obj,v_exp,v_des,success)) ii >>!
           V.intToV 1)
*)
      | CPP11.Efence(mo) ->
	M.mk_singleton_es (Act.Fence mo) ii >>! V.intToV 0

      | CPP11.Ecomma(e1,e2) ->
        build_semantics_expr e1 ii >>= fun _v ->
        build_semantics_expr e2 ii

      | CPP11.Eparen e ->
        build_semantics_expr e ii

        
    let rec build_semantics ii : (A.program_order_index * B.t) M.t = match ii.A.inst with
      | CPP11.Pblock insts -> 
        build_semantics_list insts ii 
      
      | CPP11.Pexpr e ->
        build_semantics_expr e ii >>!
        (A.next_po_index ii.A.program_order_index, B.Next)
      
      | CPP11.Pif (e,i1,i2) ->
        build_semantics_expr e ii >>> fun ret ->
        let ii' = 
          {ii with A.program_order_index = A.next_po_index ii.A.program_order_index;} 
        in
        let then_branch = build_semantics {ii' with A.inst = i1} in
        let else_branch = build_semantics {ii' with A.inst = i2} in
        M.choiceT ret then_branch else_branch
      
      | CPP11.Pwhile (e,i1) ->
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
