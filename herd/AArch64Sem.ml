(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module Make (C:Sem.Config)(V:Value.S)
=
  struct
    module AArch64 =
      AArch64Arch.Make
        (struct include C.PC let moreedges = C.moreedges end)(V)
    module Act = MachAction.Make(AArch64)
    include SemExtra.Make(C)(AArch64)(Act)

(* Barrier pretty print *)
    let barriers = 
      let bs = AArch64Base.do_fold_dmb_dsb C.moreedges (fun h t -> h::t) []
      in List.map 
	   (fun b ->
	    { barrier = b;
	      pp = Misc.lowercase (AArch64Base.pp_barrier b)})
	   bs
    let isync = Some { barrier = AArch64Base.ISB;pp = "isb";}
 
(* Semantics proper *)
    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>!) = M.(>>!)
    let (>>::) = M.(>>::)

    let mk_read an loc v = Act.Access (Dir.R, loc, v,an)
					      
    let read_loc is_data = 
      M.read_loc is_data (mk_read AArch64.N)

    let read_reg is_data r ii = 
      M.read_loc is_data (mk_read AArch64.N) (A.Location_reg (ii.A.proc,r)) ii

    let read_reg_ord = read_reg false
    let read_reg_data = read_reg true

    let read_mem a ii  = 
      M.read_loc false (mk_read AArch64.N) (A.Location_global a) ii

    let read_mem_acquire a ii  = 
      M.read_loc false (mk_read AArch64.A) (A.Location_global a) ii

    let read_mem_atomic a ii = 
      M.read_loc false (mk_read AArch64.X) (A.Location_global a) ii

    let read_mem_atomic_acquire a ii = 
      M.read_loc false (mk_read AArch64.XA) (A.Location_global a) ii
		 
    let write_loc loc v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, loc, v, AArch64.N)) ii
    let write_reg r v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v,AArch64.N)) ii
    let write_mem a v ii  = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v,AArch64.N)) ii
    let write_mem_release a v ii  = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v,AArch64.L)) ii

    let write_mem_atomic a v resa ii =
      let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
      M.mk_singleton_es_eq (Act.Access (Dir.W, A.Location_global a, v,AArch64.X)) eq ii
			   
    let write_mem_atomic_release a v resa ii =
      let eq = [M.VC.Assign (a,M.VC.Atom resa)] in
      M.mk_singleton_es_eq (Act.Access (Dir.W, A.Location_global a, v,AArch64.XL)) eq ii
			
    let create_barrier b ii = 
      M.mk_singleton_es (Act.Barrier b) ii

    let commit ii = 
      M.mk_singleton_es (Act.Commit true) ii
		  
    let flip_flag v = M.op Op.Xor v V.one	
    let is_zero v = M.op Op.Eq v V.zero
    let is_not_zero v = M.op Op.Ne v V.zero
    
    let atomic_pair_allowed _ _ = true

    let build_semantics ii = 
      M.addT (A.next_po_index ii.A.program_order_index)
        AArch64Base.( 
	match ii.A.inst with

	(* Branches *)
	| I_B l -> 
	   B.branchT l

	| I_BC(c,l)-> 
	   let cond = match c with
	     | NE -> is_not_zero
	     | EQ -> is_zero
	   in
	   (read_reg_ord NZP ii) 
	   >>= cond
	   >>= fun v -> commit ii
	   >>= fun () -> B.bccT v l

	| I_CBZ(_,r,l) -> 
	   (read_reg_ord r ii)
	   >>= is_zero
	   >>= fun v -> commit ii
	   >>= fun () -> B.bccT v l

	| I_CBNZ(_,r,l) -> 
	   (read_reg_ord r ii)
	   >>= is_not_zero
	   >>= fun v -> commit ii
	   >>= fun () -> B.bccT v l

	(* Load and Store *)
	| I_LDR(_,rd,rs,kr) ->
	   begin match kr with
	   | K k ->
	      (read_reg_ord rs ii)
	      >>= (fun v -> M.add v (V.intToV k))
	   | RV(_,r) ->
	      (read_reg_ord rs ii >>| read_reg_ord r ii)
	      >>= (fun (v1,v2) -> M.add v1 v2)
	   end
	   >>= (fun a -> read_mem a ii)
	   >>= (fun v -> write_reg rd v ii)
	   >>! B.Next

	| I_LDAR(_,t,rd,rs) ->
	   (read_reg_ord rs ii)
	   >>= fun a -> begin match t with
		 | XX ->
		    (write_reg ResAddr a ii >>|
		       (read_mem_atomic a ii
			>>= (fun v -> (write_reg rd v ii))))
		    >>! B.Next
		 | AA -> 
		    (read_mem_acquire a ii)
		    >>= (fun v -> (write_reg rd v ii))
		    >>! B.Next
		 | AX ->
		    (write_reg ResAddr a ii
		    >>| (read_mem_atomic_acquire a ii 
			 >>= (fun v -> write_reg rd v ii)))
		    >>! B.Next
	   end
	
	| I_STR(_,rs,rd,kr) ->
	   (read_reg_data rs ii >>|
	      match kr with
	      | K k ->
		 (read_reg_ord rd ii)
		 >>= (fun v -> M.add v (V.intToV k))
	      | RV(_,r) ->
		 (read_reg_ord rd ii >>| read_reg_ord r ii)
		 >>= (fun (v1,v2) -> M.add v1 v2))
	   >>= (fun (v,a) -> write_mem a v ii)
	   >>! B.Next

	| I_STLR(_,rs,rd) -> 
	   (read_reg_ord rd ii >>| read_reg_data rs ii)
	   >>= (fun (a,v) -> write_mem_release a v ii)
	   >>! B.Next
	
	| I_STXR(_,t,rr,rs,rd) -> 
	   (read_reg_ord rd ii >>| read_reg_data rs ii >>| read_reg_ord ResAddr ii)
	   >>= (fun ((a,v),res) -> 
		(write_reg ResAddr V.zero ii
		 >>| M.altT
	               (write_reg rr V.one ii)
	               ((write_reg rr V.zero ii
			>>| match t with
			    | YY -> write_mem_atomic a v res ii
			    | LY -> write_mem_atomic_release a v res ii)
		       >>! ())))
	   >>! B.Next

	(* Operations *)
	| I_MOV(_,r,k) ->
	   write_reg r (V.intToV k) ii >>! B.Next

	| I_SXTW(rd,rs) -> 
	   (read_reg_ord rs ii)
	   >>= fun v -> (write_reg rd v ii)
	   >>! B.Next
	
	| I_OP3(_,op,rd,rn,kr) -> 
	   (read_reg_ord rn ii >>|
	      match kr with
	      | K k -> M.add V.zero (V.intToV k)
	      | RV(_,r) -> read_reg_ord r ii
	   ) >>=
	     begin match op with
		   | ADD -> fun (v1,v2) -> M.add v1 v2
		   | EOR -> fun (v1,v2) -> M.op Op.Xor v1 v2
		   | SUBS -> fun (v1,v2) -> M.op Op.Sub v1 v2
	     end
	   >>= (fun v -> (write_reg rd v ii) 
			 >>| (write_reg NZP v ii))
	   >>! B.Next
		 
	(* Barrier *)
	| I_FENCE b -> 
	   (create_barrier b ii) >>! B.Next
        (*  Cannot handle *)
        | (I_LDRBH (_, _, _, _)|I_STRBH (_, _, _, _)) as i ->
            Warn.fatal "illegal instruction: %s\n"
              (AArch64.dump_instruction i)
              
      )
  end

