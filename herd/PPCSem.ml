(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Semantics of PPC instructions *)

module Make (C:Sem.Config)(V:Value.S)
    =
  struct
    open MachSize
    module PPC = PPCArch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = MachAction.Make(C.PC)(PPC)
    include SemExtra.Make(C)(PPC)(Act)

    let mixed = PPC.is_mixed

(* barrier pretty print *)
    let sync = {barrier=PPC.Sync; pp="sync";}
    let lwsync = {barrier=PPC.Lwsync; pp="lwsync";}
    let eieio = {barrier=PPC.Eieio; pp="eieio";}
    let barriers = [sync;lwsync;eieio;]
    let isync = Some {barrier=PPC.Isync; pp="isync";}
    let nat_sz =  V.Cst.Scalar.machsize

    let atomic_pair_allowed _ _ = true

(****************************)
(* Build semantics function *)
(****************************)
    module Mixed(SZ : ByteSize.S) = struct

      module Mixed = M.Mixed(SZ)

      let (>>=) = M.(>>=)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>::) = M.(>>::)
      let (>>!) = M.(>>!)

      let mk_read sz ato loc v = Act.Access (Dir.R, loc, v, ato, sz)

      let read_reg is_data r ii =
        M.read_loc is_data (mk_read nat_sz false) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_data = read_reg true
      and read_reg_ord = read_reg false

      let do_read_mem sz ato a ii =
        if mixed then
          Mixed.read_mixed false sz (fun sz -> mk_read sz ato) a ii
        else
          M.read_loc false (mk_read sz ato) (A.Location_global a) ii


      let read_mem sz a ii = do_read_mem sz false a ii
      let read_mem_atomic sz a ii = do_read_mem sz true a ii


      let write_loc sz loc v ii =
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, false, sz)) ii

      let write_reg r v ii =
        M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, nat_sz)) ii

      let do_write_mem sz ato a v ii =
        if mixed then
          Mixed.write_mixed sz
            (fun sz loc v -> Act.Access (Dir.W, loc , v, ato, sz))
            a v ii
        else
          M.mk_singleton_es
            (Act.Access (Dir.W, A.Location_global a, v, ato, sz)) ii

      let write_mem sz a v ii = do_write_mem sz false a v ii
      let write_mem_atomic sz a v ii = do_write_mem sz true a v ii


      let write_flag r o v1 v2 ii =
        M.addT
          (A.Location_reg (ii.A.proc,r))
          (M.op o v1 v2) >>= (fun (loc,v) -> write_loc nat_sz loc v ii)

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let commit ii =
        M.mk_singleton_es (Act.Commit true) ii

      let write_addr a v ii =  write_mem a v ii

      let write_addr_conditional sz a v rr ar ii =
        let eq =
          [M.VC.Assign (a, M.VC.Atom ar);
           M.VC.Assign (rr,M.VC.Atom V.one)] in
        M.mk_singleton_es_eq
          (Act.Access (Dir.W, A.Location_global a, v, true, sz)) (* a rr ar *) eq ii

      let read_addr a ii = read_mem a ii
      let read_addr_res a ii = read_mem_atomic a ii

      let read_reg_or_zero is_data r ii = match r with
      | PPC.Ireg PPC.GPR0 ->
          M.unitT V.zero
      | _ ->
          read_reg is_data r ii

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
        read_reg_ord (PPC.CRField cr) ii >>=
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
        ((read_reg_ord rA ii >>| read_reg_ord rB ii) >>=
         (fun (vA,vB) ->
           M.addT (PPC.Location_reg (proc,rD)) (M.op op vA vB)
             >>= (fun (l,v) ->
               write_loc nat_sz l v ii >>|
               flags with_flags 0 v V.zero ii))) >>! B.Next

          (* operations RD <- RA op im *)
      let op2regi ii op with_flags rD rA im =
        let proc = ii.PPC.proc in
        (read_reg_ord rA ii >>=
         (fun vA ->
           M.addT (PPC.Location_reg (proc,rD)) (M.op op vA im) >>=
           (fun (l,v) ->
             write_loc nat_sz l v ii >>|
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
          | PPC.Pnop -> M.unitT B.Next
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
                read_reg_ord rS ii >>=
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
              read_reg_ord rA ii >>=
              fun vA -> flags true cr vA (V.intToV v) ii >>!
                B.Next
          | PPC.Pcmpw (cr,rA,rB) ->
              (read_reg_ord rA ii >>| read_reg_ord rB ii) >>=
              fun (vA,vB) -> flags true cr vA vB ii >>! B.Next
(* memory loads/stores *)
          | PPC.Pload(sz,rD,d,rA) ->
              read_reg_ord rA ii >>=
              fun aA ->
                M.add aA (V.intToV d) >>=
                fun a ->
                  read_addr sz a ii >>=
                  fun v ->
                    write_reg rD v ii >>!
                    B.Next
          | PPC.Plwzu (rD,d,rA) ->
              read_reg_ord rA ii >>=
              fun aA ->
                M.add aA (V.intToV d) >>=
                (fun a ->
                  let load = read_addr Word a ii >>= fun v ->  write_reg rD v ii in
                  if rA <> PPC.r0 && rA <> rD then
                    (write_reg rA a ii >>| load) >>! B.Next
                  else load >>! B.Next)
          | PPC.Ploadx(sz,rD,rA,rB) ->
              (read_reg_or_zero false rA ii >>| read_reg_ord rB ii) >>=
              fun (aA,aB) ->
                M.add aA aB >>=
                fun a ->
                  read_addr sz a ii >>=
                  fun v -> write_reg rD v ii >>! B.Next
          | PPC.Pstore(sz,rS,d,rA) ->
              (read_reg_data rS ii >>| read_reg_ord rA ii) >>=
              (fun (vS,aA) ->
                M.add aA (V.intToV d) >>=
                fun a -> write_addr sz a vS ii >>! B.Next)
          | PPC.Pstwu(rS,d,rA) ->
              if rA <> PPC.r0 then
                M.stu
                  (read_reg_data rS ii)
                  (read_reg_ord rA ii >>= fun a -> M.add a (V.intToV d))
                  (fun a -> write_reg rA a ii)
                  (fun (vS,a) -> write_addr Word a vS ii) >>! B.Next
              else
                (read_reg_data rS ii >>| read_reg_ord rA ii) >>=
                (fun (vS,aA) ->
                  M.add aA (V.intToV d) >>=
                  fun a -> write_addr Word a vS ii >>! B.Next)

          | PPC.Pstorex(sz,rS,rA,rB) ->
              (read_reg_data rS ii
                 >>| (* Enforce right associativity of >>| *)
                 (read_reg_or_zero false rA ii
                    >>| read_reg_ord rB ii)) >>=
              (fun (vS,(aA,aB)) ->
                M.add aA aB  >>=
                fun a -> write_addr sz a vS ii >>! B.Next)
          | PPC.Plwarx(rD,rA,rB) ->
              (read_reg_or_zero false rA ii >>| read_reg_ord rB ii) >>=
              fun (aA,aB) ->
                M.add aA aB >>=
                (fun a ->
                  write_reg PPC.RES V.one ii >>| write_reg PPC.RESADDR a ii >>|
                  (read_addr_res Word a ii >>=  fun v -> write_reg rD v ii))
                  >>! B.Next
          | PPC.Pstwcx(rS,rA,rB) ->
              ((read_reg_data rS ii >>|
              read_reg_data PPC.RES ii >>| read_reg_data PPC.RESADDR ii)
                 >>| (* Enforce right associativity of >>| *)
                 (read_reg_or_zero false rA ii >>| read_reg_ord rB ii)) >>=
              fun (((vS,vR),aR),(aA,aB)) ->
                M.add aA aB  >>=
                fun a ->
                  M.altT
                    ((write_reg PPC.RES V.zero ii >>| flags_res false ii) >>! B.Next)
                    (write_reg PPC.RES V.zero ii >>| (write_addr_conditional Word a vS vR aR ii >>|
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
          | PPC.Pcomment _ ->
              Warn.warn_always "Instruction %s interpreted as a NOP"
                (PPC.dump_instruction ii.A.inst);
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
            ->
              Warn.fatal "Instruction %s not implemented"
                (PPC.dump_instruction ii.A.inst)
          end
    end
  end
