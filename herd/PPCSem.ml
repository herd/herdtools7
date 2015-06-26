(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Semantics of PPC instructions *)

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct
    open MachSize
    module PPC = PPCArch.Make(C.PC)(V)
    module Act = MachAction.Make(PPC)
    include SemExtra.Make(C)(PPC)(Act) 

(* barrier pretty print *)
    let sync = {barrier=PPC.Sync; pp="sync";}
    let lwsync = {barrier=PPC.Lwsync; pp="lwsync";}
    let eieio = {barrier=PPC.Eieio; pp="eieio";}
    let barriers = [sync;lwsync;eieio;]
        
    let  isync = Some {barrier=PPC.Isync; pp="isync";}         


(****************************)	  
(* Build semantics function *)
(****************************)	  

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>::) = M.(>>::)
    let (>>!) = M.(>>!)

    let mk_read ato loc v = Act.Access (Dir.R, loc, v, ato)
					      
    let read_loc = 
      M.read_loc (mk_read false)
    let read_reg r ii = 
      M.read_loc (mk_read false) (A.Location_reg (ii.A.proc,r)) ii
    let read_mem a ii  = 
      M.read_loc (mk_read false) (A.Location_global a) ii
    let read_mem_atomic a ii = 
      M.read_loc (mk_read true) (A.Location_global a) ii
		 
    let write_loc loc v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, loc, v, false)) ii
    let write_reg r v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false)) ii
    let write_mem a v ii  = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, false)) ii

    let write_mem_atomic a v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, true)) ii

    let write_flag r o v1 v2 ii =
	M.addT
	  (A.Location_reg (ii.A.proc,r))
	  (M.op o v1 v2) >>= (fun (loc,v) -> write_loc loc v ii)

    let create_barrier b ii = 
      M.mk_singleton_es (Act.Barrier b) ii

    let commit ii = 
      M.mk_singleton_es (Act.Commit) ii

(* Now handled by axiomatic model *)
    let with_store_buffer () = false

    let write_addr a v ii =  write_mem a v ii

(*    let write_addr_conditional a v ii =  write_mem_atomic  a v ii *)
    let write_addr_conditional a v rr ar ii =
      let eq =
        [M.VC.Assign (a, M.VC.Atom ar);
         M.VC.Assign (rr,M.VC.Atom V.one)] in
      M.mk_singleton_es_eq
        (Act.Access (Dir.W, A.Location_global a, v, true)) (* a rr ar *) eq ii

    let read_addr a ii = read_mem a ii
    let read_addr_res a ii = read_mem_atomic a ii

    let read_reg_or_zero r ii = match r with
    | PPC.Ireg PPC.GPR0 ->
        M.unitT V.zero
    | _ ->
        read_reg r ii

(**********************)
(* Condition register *)
(**********************)


    let bit_lt = 0
    and bit_gt = 1
    and bit_eq = 2

    let cr0 = PPC.CRField 0

(* ISA notation is 0 for high-order, sigh *)
    let caml_bitof bitidx = 3-bitidx
    and ppc_bitreg cr bit = PPC.CRBit (32+4*cr+bit)

    let read_flag cr bit ii =
      read_reg (PPC.CRField cr) ii >>=
      M.op1 (Op.ReadBit (caml_bitof bit))

          (* Set flags by comparing v1 v2 *)
    let mask bit = V.intToV (1 lsl (caml_bitof bit))

    let ifT bit op v1 v2 k =
      (M.op op v1 v2 >>| k) >>=
      fun (vb,vk) ->
	M.op3 Op.If vb (mask bit) vk

    let write_cr cr v1 v2 ii =
      ifT bit_lt Op.Lt v1 v2
	  (ifT bit_gt Op.Gt v1 v2
	      (M.unitT (mask bit_eq))) >>=
        (* [not Lt /\ not Gt] => Eq *)
	fun v -> write_reg (PPC.CRField cr) v ii

    let flags with_flags cr v1 v2 ii =
      if with_flags then write_cr cr v1 v2 ii >>! ()
      else M.unitT ()

(* sets the CR0[EQ] bit to veq *)
    let flags_res veq ii =
      let cr = 0 in
      write_reg (PPC.CRField cr) (if veq then mask bit_eq else V.zero) ii 

          (* operations RD <- RA op RB *) 
    let op3regs ii op set rD rA rB =
      let with_flags = match set with
      | PPC.SetCR0 -> true
      | PPC.DontSetCR0 -> false in
      let proc = ii.PPC.proc in
      ((read_reg rA ii >>| read_reg rB ii) >>=
       (fun (vA,vB) ->
	 M.addT (PPC.Location_reg (proc,rD)) (M.op op vA vB)
	   >>= (fun (l,v) ->
 	     write_loc l v ii >>|	  
	     flags with_flags 0 v V.zero ii))) >>! B.Next

        (* operations RD <- RA op im *) 
    let op2regi ii op with_flags rD rA im =
      let proc = ii.PPC.proc in
      (read_reg rA ii >>=
       (fun vA ->
	 M.addT (PPC.Location_reg (proc,rD)) (M.op op vA im) >>=
	 (fun (l,v) ->
 	   write_loc l v ii >>|	  
	   flags with_flags 0 v V.zero ii))) >>! B.Next


    let bcc_yes cr bit ii lbl =
      read_flag cr bit ii >>= fun v ->
        commit ii >>= fun () -> B.bccT v lbl  

    let bcc_no cr bit ii lbl =
      read_flag cr bit ii >>=
      M.op1 Op.Not >>=
      fun v ->  commit ii >>= fun () -> B.bccT v lbl

    let build_semantics ii = 
      M.addT (A.next_po_index ii.A.program_order_index)
        begin match ii.A.inst with
(* 3 regs ops *)
    |  PPC.Padd (set,rD,rA,rB) ->
	op3regs ii Op.Add set rD rA rB
    |  PPC.Psub (set,rD,rA,rB) ->
	op3regs ii Op.Sub set rD rA rB 
    |  PPC.Psubf (set,rD,rA,rB) ->
	op3regs ii Op.Sub set rD rB rA (* subtract from -> swap args *)
    | PPC.Por (set,rD,rA,rB) ->
	op3regs ii Op.Or set rD rA rB
    | PPC.Pand (set,rD,rA,rB) ->
	op3regs ii Op.And set rD rA rB
    | PPC.Pxor (set,rD,rA,rB) ->
	op3regs ii Op.Xor set rD rA rB
    | PPC.Pmull (set,rD,rA,rB) ->
	op3regs ii Op.Mul set rD rA rB
    | PPC.Pdiv (set,rD,rA,rB) ->
	op3regs ii Op.Div set rD rA rB
(* A very specific 3 regs op *)
    | PPC.Pmr (rD,rS) ->
	let too_far = false in
	if too_far then
(* Hum, maybe of an exageration, 2 read events, against one *)
	  op3regs ii Op.Or PPC.DontSetCR0 rD rS rS
	else
	  read_reg rS ii >>=
	  fun v -> write_reg rD v ii >>!
	    B.Next
(* 2 reg + immediate *)
    | PPC.Pli (rD,v)
    | PPC.Paddi (rD,PPC.Ireg (PPC.GPR0),v) ->
(* Believe it or not Power ISA, p. 62 says so,
   In addi r,GPR0,v GPR0 is interpreted as constant 0 ! *)
	write_reg rD (V.intToV v) ii >>!
	B.Next
    |  PPC.Paddi (rD,rA,simm) ->
	op2regi ii Op.Add false rD rA (V.intToV simm)
    |  PPC.Pori (rD,rA,simm) ->
	op2regi ii Op.Or false rD rA (V.intToV simm)
    |  PPC.Pxori (rD,rA,simm) ->
	op2regi ii Op.Xor false rD rA (V.intToV simm)
    |  PPC.Pandi (rD,rA,simm) ->
        (* ISA p. 75: CR0 is set *)
	op2regi ii Op.And true rD rA (V.intToV simm)
    |  PPC.Pmulli (rD,rA,simm) ->
	op2regi ii Op.Mul false rD rA (V.intToV simm)

(* Branch *)
    | PPC.Pb lbl ->
	B.branchT lbl
(* Conditional branches, over cr0 only *)
    | PPC.Pbcc(PPC.Lt,lbl) -> bcc_yes 0 bit_lt ii lbl
    | PPC.Pbcc(PPC.Ge,lbl) -> bcc_no 0 bit_lt ii lbl
    | PPC.Pbcc(PPC.Gt,lbl) -> bcc_yes 0 bit_gt ii lbl
    | PPC.Pbcc(PPC.Le,lbl) -> bcc_no  0 bit_gt ii lbl
    | PPC.Pbcc(PPC.Eq,lbl) -> bcc_yes 0 bit_eq ii lbl
    | PPC.Pbcc(PPC.Ne,lbl) -> bcc_no 0 bit_eq ii lbl
(* Compare, to result in any cr *)
    | PPC.Pcmpwi (cr,rA,v) ->
	read_reg rA ii >>=
	fun vA -> flags true cr vA (V.intToV v) ii >>!
	  B.Next
    | PPC.Pcmpw (cr,rA,rB) ->
	(read_reg rA ii >>| read_reg rB ii) >>=
	fun (vA,vB) -> flags true cr vA vB ii >>! B.Next
(* memory loads/stores *)
    | PPC.Pload((Word|Quad),rD,d,rA) ->
        read_reg rA ii >>= 
	fun aA -> 
	  M.add aA (V.intToV d) >>=
	  fun a -> 
	    read_addr a ii >>=
	    fun v ->
	      write_reg rD v ii >>!
	      B.Next
    | PPC.Plwzu (rD,d,rA) ->
        read_reg rA ii >>=
	fun aA ->
	  M.add aA (V.intToV d) >>=
	  (fun a ->
            let load = read_addr a ii >>= fun v ->  write_reg rD v ii in
            if rA <> PPC.r0 && rA <> rD then
              (write_reg rA a ii >>| load) >>! B.Next
            else load >>! B.Next)
    | PPC.Ploadx((Word|Quad),rD,rA,rB) ->
	(read_reg_or_zero rA ii >>| read_reg rB ii) >>=
	fun (aA,aB) -> 
	  M.add aA aB >>=
	  fun a ->
	    read_addr a ii >>=
	    fun v -> write_reg rD v ii >>! B.Next
    | PPC.Pstore((Quad|Word),rS,d,rA) ->
	(read_reg rS ii >>| read_reg rA ii) >>=
	(fun (vS,aA) -> 
	  M.add aA (V.intToV d) >>=
	  fun a -> write_addr a vS ii >>! B.Next)
    | PPC.Pstwu(rS,d,rA) ->
        if rA <> PPC.r0 then
          M.stu
            (read_reg rS ii)
            (read_reg rA ii >>= fun a -> M.add a (V.intToV d))
            (fun a -> write_reg rA a ii)
            (fun (vS,a) ->  write_addr a vS ii) >>! B.Next
        else
	(read_reg rS ii >>| read_reg rA ii) >>=
	(fun (vS,aA) ->
	  M.add aA (V.intToV d) >>=
	  fun a -> write_addr a vS ii >>! B.Next)

    | PPC.Pstorex((Word|Quad),rS,rA,rB) ->
	(read_reg rS ii
	   >>| (* Enforce right associativity of >>| *)
	   (read_reg_or_zero rA ii
              >>| read_reg rB ii)) >>=
	(fun (vS,(aA,aB)) ->
	  M.add aA aB  >>=
	  fun a -> write_addr a vS ii >>! B.Next)
    | PPC.Plwarx(rD,rA,rB) ->
	(read_reg_or_zero rA ii >>| read_reg rB ii) >>=
	fun (aA,aB) -> 
	  M.add aA aB >>=
	  fun a ->
	    read_addr_res a ii >>=
	    (fun v -> write_reg rD v ii >>| write_reg PPC.RES V.one ii >>| write_reg PPC.RESADDR a ii) 
              >>! B.Next
    | PPC.Pstwcx(rS,rA,rB) ->
	((read_reg rS ii >>| read_reg PPC.RES ii >>| read_reg PPC.RESADDR ii)
	   >>| (* Enforce right associativity of >>| *)
	   (read_reg_or_zero rA ii >>| read_reg rB ii)) >>=
	fun (((vS,vR),aR),(aA,aB)) ->
	  M.add aA aB  >>=
	  fun a ->
            M.altT
              ((write_reg PPC.RES V.zero ii >>| flags_res false ii) >>! B.Next)
              (write_reg PPC.RES V.zero ii >>| (write_addr_conditional a vS vR aR ii >>|
              flags_res true ii) >>! B.Next)
    |PPC.Peieio  ->
	create_barrier PPC.Eieio ii >>! B.Next	  
    |PPC.Psync   ->
	create_barrier PPC.Sync ii >>! B.Next
    |PPC.Plwsync -> 
	create_barrier PPC.Lwsync ii >>! B.Next
    |PPC.Pisync  ->
	create_barrier PPC.Isync ii >>! B.Next
    |PPC.Pdcbf (_rA,_rB) ->
        M.unitT B.Next
    | PPC.Pnor (_, _, _, _)
    | PPC.Pneg (_, _, _)
    | PPC.Pslw (_, _, _, _)
    | PPC.Psrawi (_, _, _, _)
    | PPC.Psraw (_, _, _, _)
    | PPC.Pbl _ 
    | PPC.Pblr
    | PPC.Pmtlr _
    | PPC.Pmflr _
    | PPC.Pstmw _
    | PPC.Plmw _
    | PPC.Pload ((Byte|Short),_,_,_)
    | PPC.Ploadx ((Byte|Short),_,_,_)
    | PPC.Pstore ((Byte|Short),_,_,_)
    | PPC.Pstorex ((Byte|Short),_,_,_)
    | PPC.Pcomment _ ->
        Warn.fatal "Instruction %s not implemented"
          (PPC.dump_instruction ii.A.inst)
        end
  end
    
