(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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

    let read_reg ?(stack=[]) r ii = 
      try 
        let v = List.assoc r stack in (M.unitT v)	
      with Not_found -> 
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


    let commit ii =  M.mk_singleton_es (Act.Commit) ii


    let create_call s ii =
      M.mk_singleton_es (Act.CallStart s) ii >>*=
      fun () ->  M.mk_singleton_es (Act.CallEnd s) ii

    let create_barrier b o ii = 
      M.mk_singleton_es (Act.Barrier(b,o)) ii

    let read_roa ?(stack=[]) roa ii = 
      match roa with 
      | BellBase.Rega r -> read_reg ~stack:stack r ii
      | BellBase.Abs a -> (M.unitT (V.maybevToV a))

    let read_roi roi ii = 
      match roi with
      | BellBase.Regi r -> read_reg r ii
      | BellBase.Imm i -> (M.unitT (V.intToV i))	

    let read_iar ?(stack=[]) roi ii = 
      match roi with
      | BellBase.IAR_roa roa -> read_roa ~stack:stack roa ii
      | BellBase.IAR_imm i -> (M.unitT (V.intToV i))	

    let solve_addr_op ao ii = match ao with
      | BellBase.Addr_op_atom roa -> read_roa roa ii
      | BellBase.Addr_op_add(roa,roi) -> (read_roa roa ii >>|
	  read_roi roi ii) >>= 
	(fun (v1,v2) -> M.op Op.Add v1 v2)

    let tr_op ?(stack=[]) ii = function
      | BellBase.Add(x,y) | BellBase.And(x,y) | BellBase.Xor(x,y) 
      | BellBase.Eq(x,y) | BellBase.Neq(x,y) as bell_op -> 
      let op = match bell_op with 
        | BellBase.RAI _ -> assert false
        | BellBase.Xor _ -> Op.Xor
        | BellBase.Add _ -> Op.Add
        | BellBase.And _ -> Op.And 
        | BellBase.Eq _ -> Op.Eq 
        | BellBase.Neq _ -> Op.Ne  
      in
      ((read_iar ~stack:stack x ii) >>| (read_iar ~stack:stack y ii)) >>=
        (fun (v1,v2) -> M.op op v1 v2) 
      | BellBase.RAI(i) -> (read_iar ~stack:stack i ii) 
    
    let tr_mov r op ii = 
      (tr_op ii op) >>= (fun v -> write_reg r v ii) 

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

	| BellBase.Pfence(BellBase.Fence (s,o)) ->
      	  create_barrier s o ii >>! B.Next	  

        | BellBase.Pcall s ->
            create_call s ii >>! B.Next

	| BellBase.Prmw(r,op,addr_op,s) ->
          (solve_addr_op addr_op ii) >>=
          (fun x -> (read_mem_atom x s ii) >>=
            (fun v -> 
                  (tr_op ~stack:[(r,v)] ii op) >>= 
                  (fun v -> write_reg r v ii >>|
                            write_mem_atom x v s ii))) >>!
          B.Next

	| BellBase.Pbranch(Some r,lbl,_) -> 
   	  (read_reg r ii) >>=
            (fun v -> commit ii >>= fun () -> B.bccT v lbl)

	| BellBase.Pbranch(None ,lbl,_) ->  B.branchT lbl
 
        | BellBase.Pmov(r,op) ->
          (tr_mov r op ii) >>! B.Next

      in 
      M.addT (A.next_po_index ii.A.program_order_index) (build_semantics_inner ii)
  end
    
