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

module
  Make
    (C:Sem.Config)
    (V:Value.S with type Cst.Instr.exec = PPCBase.instruction)
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

    let () = assert (nat_sz = MachSize.Quad)

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

      let mk_read sz ato loc v =
        let ac = Act.access_of_location_std loc in
        Act.Access (Dir.R, loc, v, ato, (), sz, ac)

      let read_reg port r ii =
        M.read_loc port (mk_read nat_sz false) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg Port.No
      and read_reg_data = read_reg Port.Data
      and read_reg_addr = read_reg Port.Addr

      let do_read_mem sz ato a ii =
        if mixed then
          Mixed.read_mixed Port.No sz (fun sz -> mk_read sz ato) a ii
        else
          M.read_loc Port.No (mk_read sz ato) (A.Location_global a) ii


      let read_mem sz a ii = do_read_mem sz false a ii
      let read_mem_atomic sz a ii = do_read_mem sz true a ii


      let write_loc sz loc v ii =
        let ac = Act.access_of_location_std loc in
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, false, (), sz, ac)) ii

      let write_reg r v ii =
        M.mk_singleton_es
          (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, (), nat_sz,Access.REG)) ii

      let do_write_mem sz ato a v ii =
        if mixed then
          Mixed.write_mixed sz
            (fun sz loc v -> Act.Access (Dir.W, loc , v, ato, (), sz, Access.VIR))
            a v ii
        else
          M.mk_singleton_es
            (Act.Access (Dir.W, A.Location_global a, v, ato, (), sz, Access.VIR)) ii

      let write_mem sz a v ii = do_write_mem sz false a v ii
      let write_mem_atomic sz a v ii = do_write_mem sz true a v ii


      let write_flag r o v1 v2 ii =
        M.addT
          (A.Location_reg (ii.A.proc,r))
          (M.op o v1 v2) >>= (fun (loc,v) -> write_loc nat_sz loc v ii)

      let create_barrier b ii =
        M.mk_singleton_es (Act.Barrier b) ii

      let commit ii =
        M.mk_singleton_es (Act.Commit (Act.Bcc,None)) ii

      let write_addr a v ii =  write_mem a v ii

      let write_addr_conditional sz a v rr ar ii =
        let eq =
          [M.VC.Assign (a, M.VC.Atom ar);
           M.VC.Assign (rr,M.VC.Atom V.one)] in
        M.mk_singleton_es_eq
          (Act.Access (Dir.W, A.Location_global a, v, true, (), sz, Access.VIR)) (* a rr ar *) eq ii

      let read_addr a ii = read_mem a ii
      let read_addr_res a ii = read_mem_atomic a ii

      let read_reg_or_zero_addr r ii = match r with
      | PPC.Ireg PPC.GPR0 ->  M.unitT V.zero
      | _ -> read_reg_addr r ii

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
                 flags with_flags 0 v V.zero ii)))
        >>= B.next2T

          (* operations RD <- RA op im *)
      let op2regi ii op with_flags rD rA im =
        let proc = ii.PPC.proc in
        (read_reg_ord rA ii >>=
         (fun vA ->
           M.addT (PPC.Location_reg (proc,rD)) (M.op op vA im) >>=
           (fun (l,v) ->
             write_loc nat_sz l v ii >>|
               flags with_flags 0 v V.zero ii)))
        >>= B.next2T


      let bcc_yes cr bit ii lbl =
        read_flag cr bit ii >>= fun v ->
          commit ii >>= fun () -> B.bccT v lbl

      let bcc_no cr bit ii lbl =
        read_flag cr bit ii >>=
        M.op1 Op.Not >>=
        fun v ->  commit ii >>= fun () -> B.bccT v lbl
(**********)
(* Rotate *)
(**********)

      let rot64 x n =
        let n = n mod 64 in
        begin
          M.op1 (Op.LeftShift n) x >>|
          M.op1 (Op.LogicalRightShift (63-n)) x
        end >>= fun (x,y) ->
        M.op Op.Or x y

      let rot32 x n =
        M.op1 (Op.Mask MachSize.Word) x >>= fun x ->
        M.op1 (Op.LeftShift 32) x >>= fun y ->
        M.op Op.Or x y >>= fun x ->
        rot64 x n

(****************************)
(* Mask auxiliary functions *)
(****************************)

      let op_and_mask msk = M.op1 (Op.AndK ("0b" ^ msk))

      let mask k1 k2 =
        let msk =
          if k1 <= k2 then
            let msk = Bytes.make 64 '0' in
            for k=k1 to k2 do
              Bytes.set msk k '1'
            done ;
            msk
          else
            let msk = Bytes.make 64 '1' in
            for k = k2+1 to k1-1 do
              Bytes.set msk k '0'
            done ;
            msk in
        Bytes.to_string msk

      let not_mask msk =
        let len = String.length msk in
        let r = Bytes.create len in
        for k=0 to len-1 do
          let c =
            match msk.[k] with
            | '0' -> '1'
            | '1' -> '0'
            | _ -> assert false in
          Bytes.set r k c
        done ;
        Bytes.to_string r

      let build_semantics _ ii =
        M.addT (A.next_po_index ii.A.program_order_index)
          begin match ii.A.inst with
          | PPC.Pnop -> B.nextT
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
                fun v -> write_reg rD v ii >>= B.next1T
(* rotate instructions *)
          | PPC.Pextsw (rD,rA) ->
            read_reg_ord rA ii >>=
            M.op1 (Op.Sxt Word) >>=
            fun v -> write_reg rD v ii >>=
            B.next1T
          | PPC.Prlwinm (rD,rA,k1,k2,k3) ->
            (* rotate left word immediate and mask *)
             let m = mask (k2+32) (k3+32) in
             read_reg_ord rA ii >>= fun a ->
             rot32 a k1 >>=
             op_and_mask m >>= fun v ->
             write_reg rD v ii >>= B.next1T
          | PPC.Prlwimi (rD,rA,k1,k2,k3) ->
            (* rotate left word immediate mask insert*)
             let m = mask (k2+32) (k3+32) in
             let not_m = not_mask m in
             (read_reg_ord rD ii >>| read_reg_ord rA ii) >>= fun (d,a) ->
             rot32 a k1 >>= fun a ->
             op_and_mask m a >>| op_and_mask not_m d >>= fun (d,a) ->
            (* unlike RLWINM, this instruction preserves bits*)
            M.op Op.Or d a >>= fun v ->
            write_reg rD v ii >>= B.next1T
          | PPC.Pclrldi (rD,rA,k) ->
            (* create 1 mask from 0-(k-1), 0 mask for the rest *)
            (* AND with contents of rA, store in rD *)
            let m = mask k 63  in
            read_reg_ord rA ii >>=
            op_and_mask m >>= fun v ->
            (* unlike RLWINM, this instruction preserves bits*)
            (* We can simply OR (shifted rA) and (masked rD) *)
            write_reg rD v ii >>= B.next1T
(* 2 reg + immediate *)
          | PPC.Plis (rD,v) ->
              M.op Op.ShiftLeft (V.intToV v) (V.intToV 16)
              >>= fun v -> write_reg rD v ii
              >>= B.next1T
          | PPC.Pli (rD,v)
          | PPC.Paddi (rD,PPC.Ireg (PPC.GPR0),v) ->
(* Believe it or not Power ISA, p. 62 says so,
   In addi r,GPR0,v GPR0 is interpreted as constant 0 ! *)
              write_reg rD (V.intToV v) ii >>= B.next1T
          |  PPC.Paddi (rD,rA,simm) ->
              op2regi ii Op.Add false rD rA (V.intToV simm)
          |  PPC.Paddis (rD,PPC.Ireg PPC.GPR0,simm) ->
              write_reg rD (V.intToV (simm lsl 16)) ii >>= B.next1T
          |  PPC.Paddis (rD,rA,simm) ->
              op2regi ii Op.Add false rD rA (V.intToV (simm lsl 16))
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
          | PPC.Pblr when C.variant Variant.Telechat -> (* We assume a jump to the callee*)
            (* in the link register is an exit from the program*)
            M.unitT () >>! B.Exit
(* Compare, to result in any cr *)
          | PPC.Pcmpwi (cr,rA,v) ->
              read_reg_ord rA ii >>=
              fun vA -> flags true cr vA (V.intToV v) ii >>= B.next1T
          | PPC.Pcmpw (cr,rA,rB) ->
              (read_reg_ord rA ii >>| read_reg_ord rB ii) >>=
              fun (vA,vB) -> flags true cr vA vB ii >>= B.next1T
          | PPC.Pcmplwi (cr,rA,v) ->
              (read_reg_ord rA ii) >>= M.op1 (Op.Mask MachSize.Word) >>=
              fun vA ->
                flags true cr vA (V.intToV (v land 0xffff)) ii >>= B.next1T
          | PPC.Pmfcr rA ->
              read_reg_ord (PPC.CRField 0) ii >>=
              fun v -> write_reg rA v ii >>= B.next1T
(* memory loads/stores *)
          | PPC.Pload(sz,rD,d,rA) ->
              read_reg_addr rA ii >>=
              fun aA ->
                M.add aA (V.intToV d) >>=
                fun a ->
                  read_addr sz a ii >>=
                  fun v -> write_reg rD v ii >>= B.next1T
          | PPC.Plwa(rD,d,rA) ->
              read_reg_addr rA ii >>= fun aA ->
              M.add aA (V.intToV d) >>= fun a ->
              read_addr Word a ii >>=
              M.op1 (Op.Sxt Word) >>= fun v ->
              write_reg rD v ii >>= B.next1T
          | PPC.Plwzu (rD,d,rA) ->
              read_reg_addr rA ii >>=
              fun aA ->
                M.add aA (V.intToV d) >>=
                (fun a ->
                  let load = read_addr Word a ii >>= fun v ->  write_reg rD v ii in
                  if rA <> PPC.r0 && rA <> rD then
                    (write_reg rA a ii >>| load) >>= B.next2T
                  else load >>= B.next1T)
          | PPC.Plwax(sz,rD,rA,rB)
          | PPC.Ploadx(sz,rD,rA,rB) as i ->
              (read_reg_or_zero_addr rA ii >>| read_reg_addr rB ii) >>=
              fun (aA,aB) ->
                M.add aA aB >>=
                fun a ->
                  read_addr sz a ii >>=
                  (match i with
                   | PPC.Plwax _ -> M.op1 (Op.Sxt sz)
                   | _ -> M.unitT) >>=
                  fun v -> write_reg rD v ii >>= B.next1T
          | PPC.Pstore(sz,rS,d,rA) ->
              (read_reg_data rS ii >>| read_reg_addr rA ii) >>=
              (fun (vS,aA) ->
                M.add aA (V.intToV d) >>=
                fun a -> write_addr sz a vS ii >>= B.next1T)
          | PPC.Pstwu(rS,d,rA) ->
              if rA <> PPC.r0 then
                M.stu
                  (read_reg_data rS ii)
                  (read_reg_addr rA ii >>= fun a -> M.add a (V.intToV d))
                  (fun a -> write_reg rA a ii)
                  (fun (vS,a) -> write_addr Word a vS ii) >>= B.next1T
              else
                (read_reg_data rS ii >>| read_reg_addr rA ii) >>=
                (fun (vS,aA) ->
                  M.add aA (V.intToV d) >>=
                  fun a -> write_addr Word a vS ii >>= B.next1T)

          | PPC.Pstorex(sz,rS,rA,rB) ->
              (read_reg_data rS ii
                 >>| (* Enforce right associativity of >>| *)
                 (read_reg_or_zero_addr rA ii
                    >>| read_reg_addr rB ii)) >>=
              (fun (vS,(aA,aB)) ->
                M.add aA aB  >>=
                fun a -> write_addr sz a vS ii >>= B.next1T)
          | PPC.Plwarx(rD,rA,rB) ->
              (read_reg_or_zero_addr rA ii >>| read_reg_addr rB ii) >>=
              fun (aA,aB) ->
                M.add aA aB >>=
                (fun a ->
                  write_reg PPC.RES V.one ii >>| write_reg PPC.RESADDR a ii >>|
                  (read_addr_res Word a ii >>=  fun v -> write_reg rD v ii))
                  >>= fun (((),()),()) -> B.nextT
          | PPC.Pstwcx(rS,rA,rB) ->
              ((read_reg_data rS ii >>|
              read_reg_ord PPC.RES ii >>| read_reg_ord PPC.RESADDR ii)
                 >>| (* Enforce right associativity of >>| *)
                 (read_reg_or_zero_addr rA ii >>| read_reg_addr rB ii)) >>=
              fun (((vS,vR),aR),(aA,aB)) ->
                M.add aA aB  >>=
                fun a ->
                  M.altT
                    ((write_reg PPC.RES V.zero ii >>| flags_res false ii)
                     >>= B.next2T)
                    (write_reg PPC.RES V.zero ii
                     >>| (write_addr_conditional Word a vS vR aR ii
                          >>| flags_res true ii)
                     >>= fun ((),((),())) -> B.nextT)
          |PPC.Peieio  ->
              create_barrier PPC.Eieio ii >>= B.next1T
          |PPC.Psync   ->
              create_barrier PPC.Sync ii >>= B.next1T
          |PPC.Plwsync ->
              create_barrier PPC.Lwsync ii >>= B.next1T
          |PPC.Pisync  ->
              create_barrier PPC.Isync ii >>= B.next1T
          |PPC.Pdcbf (_rA,_rB) ->
              B.nextT
          | PPC.Pcomment _ ->
              Warn.warn_always "Instruction %s interpreted as a NOP"
                (PPC.dump_instruction ii.A.inst);
              B.nextT
          | PPC.Pnor (_, _, _, _)
          | PPC.Pneg (_, _, _)
          | PPC.Pslw (_, _, _, _)
          | PPC.Psrawi (_, _, _, _)
          | PPC.Psraw (_, _, _, _)
          | PPC.Pbl _
          | PPC.Pmtlr _
          | PPC.Pmflr _
          | PPC.Pstmw _
          | PPC.Plmw _
          | PPC.Pblr
            ->
              Warn.fatal "Instruction %s not implemented"
                (PPC.dump_instruction ii.A.inst)
          end

      let spurious_setaf _ = assert false

    end
  end
