(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
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


    let commit ii =  M.mk_singleton_es (Act.Commit) ii

    let create_barrier b o ii = 
      M.mk_singleton_es (Act.Barrier(b,o)) ii
	
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


(*    let do_2op v1 v2 op addr = match op with
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
	                     (M.unitT addr)) *)

    let solve_addr_op ao ii = match ao with
      | BellBase.Addr_op_atom roa -> read_roa roa ii
      | BellBase.Addr_op_add(roa,roi) -> (read_roa roa ii >>|
	  read_roi roi ii) >>= 
	(fun (v1,v2) -> M.op Op.Add v1 v2)

    let tr_cond ii lbl = function
      | BellBase.Ne(r,i) | BellBase.Eq(r,i) as cond -> 
        let op = match cond with 
          | BellBase.Ne _ -> Op.Ne
          | BellBase.Eq _ -> Op.Eq
          | _ -> assert false
        in
	  (read_reg r ii) >>|
	      (read_roi i ii) >>=
		(fun (v1,v2) -> M.op op v1 v2 >>=
		  (fun v -> commit ii >>= fun () -> B.bccT v lbl))
      | BellBase.Bal ->  M.unitT (B.Jump lbl)

    let op_of_rmw_op op = match op with
          | BellBase.Add _ -> Op.Add 
          | BellBase.And _ -> Op.And
          | _ -> assert false

(*  jade: j'aurais voulu mettre les calculs de rmw en facteur comme ca, mais du
coup je n'arrivais pas a obtenir de iico entre le R* et le W* d'un rmw, car je
n'arrivais pas a creer de iico_truc entre; pas sure de bien comprendre
pourquoi!

     let tr_op op r roi v ii = 
          (read_iar roi ii) >>=
          (fun v' -> M.op op v v') >>=
          (fun res -> (write_reg r res ii)) (*>>=
          (fun () -> read_reg r ii)*) 

        | BellBase.Mov(r, iar) -> (*jade: todo*) 
        (read_iar iar ii) >>=
        (fun res -> (write_reg r res ii)) >>!
        B.Next *)

    let last_of_rmw_op op = match op with
      | BellBase.Add(_,_,BellBase.IAR_roa(BellBase.Abs _ as x)) 
      | BellBase.And(_,_,BellBase.IAR_roa(BellBase.Abs _ as x)) -> BellBase.Addr_op_atom(x)
(*      | BellBase.Mov jade: todo; or Exch? *)
      | _ -> failwith "the last operand of an rmw op must be an address" 

    let second_of_rmw_op op = match op with
      | BellBase.Add(_,(BellBase.IAR_roa(BellBase.Rega _) as x),_) 
      | BellBase.And(_,(BellBase.IAR_roa(BellBase.Rega _) as x),_)
      | BellBase.Add(_,(BellBase.IAR_imm _ as x),_) 
      | BellBase.And(_,(BellBase.IAR_imm _ as x),_) -> x
(*      | BellBase.Mov jade: todo; or Exch? *)
      | _ -> failwith "the second operand of an rmw op must be an immediate value or a register" 

    let first_of_rmw_op op = match op with
      | BellBase.Add(r,_,_) | BellBase.And(r,_,_) -> r
(*      | BellBase.Mov jade: todo; or Exch? *)
      | _ -> failwith "the first operand of an rmw op must be a register" 

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

	| BellBase.Prmw(op,s) ->
          let addr_op = last_of_rmw_op op in
          let roi = second_of_rmw_op op in
          let r = first_of_rmw_op op in 
          let op = op_of_rmw_op op in
          (solve_addr_op addr_op ii) >>=
          (fun x -> (read_mem_atom x s ii) >>=
            (fun v -> 
              (read_iar roi ii) >>=
                (fun v' -> M.op op v v') >>=
                  (fun res -> (write_reg r res ii) >>| 
                               write_mem_atom x res s ii))) >>!
          B.Next

	| BellBase.Pbranch(cond,lbl,_) -> tr_cond ii lbl cond
	    
	| BellBase.Pfence(BellBase.Fence (s,o)) ->
      	  create_barrier s o ii >>! B.Next	  

      in 
      M.addT (A.next_po_index ii.A.program_order_index) (build_semantics_inner ii)
  end
    
