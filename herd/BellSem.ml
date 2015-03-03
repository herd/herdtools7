(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Semantics of Bell *)

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module Bell = BellArch.Make(C.PC)(V)
    module Act = BellAction.Make(Bell)
    include SemExtra.Make(C)(Bell)(Act)

(* Not doing barrier pretty print *)
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

    let mk_read ato s loc v = Act.Access (Dir.R, loc, v, ato, s)

    let read_reg r ii = 
      M.read_loc (mk_read false []) (A.Location_reg (ii.A.proc,r)) ii

    let read_mem a s ii = 
      M.read_loc (mk_read false s) (A.Location_global a) ii

    let read_mem_atom a s ii = 
      M.read_loc (mk_read true s) (A.Location_global a) ii


(*    let read_mem_atom cop a ii = 
      M.read_loc (mk_read true cop) (A.Location_global a) ii *)

    let write_reg r v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, [])) ii

    let write_mem a v s ii = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, false, s)) ii

    let write_mem_atom a v s ii = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, true, s)) ii


(*    let write_mem_atom cop a v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, true, cop)) ii *)


    let create_barrier b ii = 
      M.mk_singleton_es (Act.Barrier b) ii
	
    let read_roa roa ii = 
      match roa with 
      | BellBase.Rega r -> read_reg r ii
      | BellBase.Abs a -> (M.unitT (V.maybevToV a))

    let read_roi roi ii = 
      match roi with
      | BellBase.Regi r -> read_reg r ii
      | BellBase.Imm i -> (M.unitT (V.intToV i))	

    let read_iar roi ii = 
      match roi with
      | BellBase.IAR_roa roa -> read_roa roa ii
      | BellBase.IAR_imm i -> (M.unitT (V.intToV i))	


    let do_2op v1 v2 op addr = match op with
      | BellBase.RMWExch -> ((M.unitT v1) >>| 
	                     (M.unitT v2) >>|
			     (M.unitT addr))

      | BellBase.RMWAdd  -> ((M.unitT v1) >>| 
	                     (M.op Op.Add v1 v2) >>|
                             (M.unitT addr))

    let do_3op v1 v2 v3 op addr = match op with
      | BellBase.RMWCAS -> (M.op Op.Eq v1 v2) >>=
	                   (fun eq -> 
			     (M.unitT v1) >>|
	                     (M.op3 Op.If eq v3 v1) >>|
	                     (M.unitT addr))

    let solve_addr_op ao ii = match ao with
      | BellBase.Addr_op_atom roa -> read_roa roa ii
      | BellBase.Addr_op_add(roa,roi) -> (read_roa roa ii >>|
	  read_roi roi ii) >>= 
	(fun (v1,v2) -> M.op Op.Add v1 v2)


    let build_semantics ii = 
      let build_semantics_inner ii =
	match ii.A.inst with
	| BellBase.Pld(r,addr_op,s) ->
	  solve_addr_op addr_op ii >>=
	    (fun addr -> read_mem addr s ii) >>=
	    (fun v -> write_reg r v ii) >>!
	    B.Next
	    
	| BellBase.Pst(addr_op, roi, s) ->
	  (solve_addr_op addr_op ii >>|
	      read_roi roi ii) >>=
	    (fun (addr,v) -> write_mem addr v s ii) >>!
	    B.Next

	| BellBase.Prmw2_op(r,roa,roi,op,s) ->
	  (read_roa roa ii ) >>=
	    (fun addr -> 
	      (read_mem_atom addr s ii) >>|
	      (read_roi roi ii) >>|
	      (M.unitT addr)
	    ) >>=
	    (fun ((v1,v2),addr) -> do_2op v1 v2 op addr) >>=
	    (fun ((prev,res),addr) -> 
	      (write_reg r prev ii) >>|
	      (write_mem_atom addr res s ii)) >>!
	    B.Next

	| BellBase.Prmw3_op(r,roa,roi1,roi2,op,s) ->
	  (read_roa roa ii ) >>=
	    (fun addr -> 
	      (read_mem_atom addr s ii) >>|
	      (read_roi roi1 ii) >>|
	      (read_roi roi2 ii) >>|
		  (M.unitT addr)
	     ) >>=
	    (fun (((v1,v2),v3),addr) -> do_3op v1 v2 v3 op addr) >>=
	    (fun ((prev,res),addr) -> 
	      (write_reg r prev ii) >>|
	      (write_mem_atom addr res s ii)) >>!
	    B.Next


	| BellBase.Pmov(r, roi) ->
	   (read_iar roi ii) >>=
	     (fun v -> write_reg r v ii) >>! B.Next
					       
	| BellBase.Padd(r, roi1, roi2) ->
	   (read_iar roi1 ii) >>|
	     (read_iar roi2 ii) >>=
	     (fun (v1,v2) -> M.op Op.Add v1 v2) >>=
	     (fun v -> write_reg r v ii) >>!
	     B.Next

	| BellBase.Pand(r, roi1, roi2) ->
	   (read_iar roi1 ii) >>|
	     (read_iar roi2 ii) >>=
	     (fun (v1,v2) -> M.op Op.And v1 v2) >>=
	     (fun v -> write_reg r v ii) >>!
	     B.Next

	| BellBase.Pxor(r, roi1, roi2) ->
	   (read_iar roi1 ii) >>|
	     (read_iar roi2 ii) >>=
	     (fun (v1,v2) -> M.op Op.Xor v1 v2) >>=
	     (fun v -> write_reg r v ii) >>!
	     B.Next

	| BellBase.Pbeq(roi1,roi2,lbl) ->
	  (read_roi roi1 ii) >>|
	      (read_roi roi2 ii) >>=
		(fun (v1,v2) -> M.op Op.Eq v1 v2 >>=
		  (fun v -> B.bccT v lbl))		  
	    
	| BellBase.Pfence(BellBase.Fence s) ->
      	  create_barrier s ii >>! B.Next	  
      in 
      M.addT (A.next_po_index ii.A.program_order_index) (build_semantics_inner ii)
  end
    
