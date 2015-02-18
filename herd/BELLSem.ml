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

(** Semantics of BELL *)

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module BELL = BELLArch.Make(C.PC)(V)
    module Act = BELLAction.Make(BELL)
    include SemExtra.Make(C)(BELL)(Act)

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

(*    let read_mem_atom cop a ii = 
      M.read_loc (mk_read true cop) (A.Location_global a) ii *)

    let write_reg r v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, [])) ii

    let write_mem a v s ii = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, false, s)) ii

(*    let write_mem_atom cop a v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, true, cop)) ii *)


    let create_barrier b ii = 
      M.mk_singleton_es (Act.Barrier b) ii
	
    let read_roa roa ii = 
      match roa with 
      | BELLBase.Rega r -> read_reg r ii
      | BELLBase.Abs a -> (M.unitT (V.maybevToV a))

    let read_roi roi ii = 
      match roi with
      | BELLBase.Regi r -> read_reg r ii
      | BELLBase.Imm i -> (M.unitT (V.intToV i))	

    let build_semantics ii = 
      let build_semantics_inner ii =
	match ii.A.inst with
	| BELLBase.Pld(r,roa,s) ->
	  read_roa roa ii >>=
	    (fun addr -> read_mem addr s ii) >>=
	    (fun v -> write_reg r v ii) >>!
	    B.Next
	    
	| BELLBase.Pst(roa, roi, s) ->
	  (read_roa roa ii >>|
	      read_roi roi ii) >>=
	    (fun (addr,v) -> write_mem addr v s ii) >>!
	    B.Next

	| BELLBase.Pmov(r, roi) ->
	   (read_roi roi ii) >>=
	     (fun v -> write_reg r v ii) >>! B.Next
					       
	| BELLBase.Padd(r, roi1, roi2) ->
	   (read_roi roi1 ii) >>|
	     (read_roi roi2 ii) >>=
	     (fun (v1,v2) -> M.op Op.Add v1 v2) >>=
	     (fun v -> write_reg r v ii) >>!
	     B.Next

	| BELLBase.Pand(r, roi1, roi2) ->
	   (read_roi roi1 ii) >>|
	     (read_roi roi2 ii) >>=
	     (fun (v1,v2) -> M.op Op.And v1 v2) >>=
	     (fun v -> write_reg r v ii) >>!
	     B.Next

	| BELLBase.Pbeq(roi1,roi2,lbl) ->
	  (read_roi roi1 ii) >>|
	      (read_roi roi2 ii) >>=
		(fun (v1,v2) -> M.op Op.Eq v1 v2 >>=
		  (fun v -> B.bccT v lbl))		  
	    
	| BELLBase.Pfence(BELLBase.Fence s) ->
      	  create_barrier s ii >>! B.Next	  
      in 
      M.addT (A.next_po_index ii.A.program_order_index) (build_semantics_inner ii)
  end
    
