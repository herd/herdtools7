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

module type Config = sig
  include Arch_litmus.Config
  val word : Word.t
  val syncmacro : int option
  val syncconst : int
end

module Make(V:Constant.S)(C:Config) =
  struct
    module A = PPCArch_litmus.Make(C)(V)
    open MachSize
    open A
    open A.Out
    open Printf

    let is_ret _ = assert false

(* Ready for template compilation *)
    let op3regs memo set rD rA rB =
      let memo = match set with
      | SetCR0 -> memo ^"."
      | DontSetCR0 -> memo in
      { empty_ins with
        memo=memo^ " ^o0,^i0,^i1";
        inputs=[rA; rB];
        outputs=[rD]; }

    let op2regs memo set rD rS =
      let memo = match set with
      | SetCR0 -> memo ^"."
      | DontSetCR0 -> memo in
      { empty_ins with
        memo=memo^ " ^o0,^i0";
        inputs=[rS];
        outputs=[rD]; }

    let op3or =  op3regs "or" DontSetCR0

    let op2regsI memo rD rA i =
      { empty_ins with
        memo= sprintf "%s ^o0,^i0,%i" memo i;
        inputs=[rA];
        outputs=[rD]; }

    let justOp memo = { empty_ins with memo=memo ; }

    let tr_ins = match C.word with
    | Word.W64 -> fun i -> i
    | Word.W32|Word.WXX ->
        fun i -> match i with
        | Pload (Quad,a1,a2,a3) -> Pload (Word,a1,a2,a3)
        | Pstore (Quad,a1,a2,a3) -> Pstore (Word,a1,a2,a3)
        | Ploadx (Quad,a1,a2,a3) -> Ploadx (Word,a1,a2,a3)
        | Pstorex (Quad,a1,a2,a3) -> Pstorex (Word,a1,a2,a3)
        | _ -> i

    let emit_lbl lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label lbl) ;
        label = Some lbl ; branch=[Next] ; }
    
    let next_label =
      let count = ref 0 in
      fun () ->
        let lbl = sprintf "__L%i" !count in
        incr count ;
        lbl

    let li rD v =
      { empty_ins with
        memo=sprintf "li ^o0,%i" v;
        inputs=[];
        outputs=[rD]; }
      
    let mr rD rS =
      { empty_ins with
        memo="mr ^o0,^i0" ;
        inputs=[rS];
        outputs=[rD]; }

    let bcc tr_lab cond lbl =
      { empty_ins with
        memo = sprintf "b%s %s" (pp_cond cond) (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Next; Branch lbl] ; }

    let jump tr_lab lbl =
      { empty_ins with
        memo = sprintf "b %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Branch lbl] ; }

(* This assumes the called subroutine does not alter any register...
    Must be wrong *)
    let jump_and_link tr_lab lbl =
      { empty_ins with
        memo = sprintf "bl %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Next] ;
        outputs = [A.LR]; }

    let incr r i = op2regsI "addi" r r i
    let decr r i =  incr r (-i)

    let cmpwi rS v = 
      { empty_ins with
        memo=sprintf "cmpwi ^i0,%i" v;
        inputs=[rS];
        outputs=[]; } (* no modeling of cc needed here *)

    let cmpw rA rB =
        { empty_ins with
          memo="cmpw ^i0,^i1" ;
          inputs=[rA;rB];
          outputs=[]; }

    let cmpld rA rB =
        { empty_ins with
          memo="cmpld ^i0,^i1" ;
          inputs=[rA;rB];
          outputs=[]; }

    let dcbf rA rB =
      { empty_ins with
        memo = "dcbf ^i0,^i1" ;
        inputs=[rA;rB] ; }

    let ld rD d rA =
      { empty_ins with
        memo = sprintf "ld ^o0,%i(^i0)"  d;
        inputs = [rA];
        outputs= [rD]; }

    let std rS d rA =
      { empty_ins with
        memo = sprintf "std ^i0,%i(^i1)" d;
        inputs=[rS;rA];
        outputs=[]; }


    let storex sz rS rA rB =
      let memo = memo_storex sz in
      { empty_ins with
        memo = memo ^ " ^i0,^i1,^i2";
        inputs=[rS;rA;rB];
        outputs=[]; }

    let stwx rS rA rB = storex Word rS rA rB

    let stw update rS d rA =
      { empty_ins with
        memo = sprintf "%s ^i0,%i(^i1)" (if update then "stwu" else "stw") d;
        inputs=[rS;rA];
        outputs=(if update && rA <> r0 then [rA;] else []); }

    let stwsz sz rS d rA =
      { empty_ins with
        memo = sprintf "%s ^i0,%i(^i1)" (memo_store sz) d;
        inputs=[rS;rA];
        outputs=[]; }

    let mftb r =
      { empty_ins with
        memo = "mftb ^o0";
        inputs=[];
        outputs=[r]; }

    let mftbu r =
      { empty_ins with
        memo = "mftbu ^o0";
        inputs=[];
        outputs=[r]; }

    let tr_nolab lbl = lbl
      
    let emit_sync_macro k =
      let delta = C.syncconst  in
      let lbl_loop = next_label () in
      let lbl_out = next_label () in
      li idx 0::
      li ephemeral 1::
      cmpwi max_idx 0::
      bcc tr_nolab Gt lbl_loop::
      justOp "sync"::
      jump tr_nolab lbl_out::
      emit_lbl lbl_loop::
      dcbf idx base::
      justOp "sync"::
      stwx ephemeral idx base::
      incr idx delta::
      cmpw idx max_idx::
      bcc tr_nolab Lt lbl_loop::
      emit_lbl lbl_out::k

    let emit_loop k = 
        let lbl1 = next_label () in
        let lbl2 = next_label () in
        jump tr_nolab lbl2::
        emit_lbl lbl1::
        k@
        [ decr loop_idx 1;
          emit_lbl lbl2 ;
          cmpwi loop_idx 0;
          bcc tr_nolab Gt lbl1; ]

    let do_compile_ins tr_lab ins k = match tr_ins ins with
    | Pnop -> { empty_ins with memo="nop"; }::k
    | Pmr (rD,rS) -> mr rD rS::k
    | Padd(set,rD,rA,rB) -> op3regs  "add" set rD rA rB::k
    | Psub(set,rD,rA,rB) -> op3regs  "sub" set rD rA rB::k
    | Psubf(set,rD,rA,rB)  -> op3regs  "subf" set rD rA rB::k
    | Pand(set,rD,rA,rB) -> op3regs  "and" set rD rA rB::k
    | Por(set,rD,rA,rB) ->  op3regs  "or" set rD rA rB::k
    | Pxor(set,rD,rA,rB) -> op3regs  "xor" set rD rA rB::k
    | Pmull(set,rD,rA,rB) -> op3regs  "mullw" set rD rA rB::k
    | Pdiv(set,rD,rA,rB) -> op3regs  "divw" set rD rA rB::k

    | Paddi(rD,rA,simm) -> op2regsI "addi" rD rA simm::k
    | Pandi(rD,rA,simm) -> op2regsI "andi." rD rA simm::k
    | Pori(rD,rA,simm) -> op2regsI "ori" rD rA simm::k
    | Pxori(rD,rA,simm) -> op2regsI "xori" rD rA simm::k
    | Pmulli(rD,rA,simm) -> op2regsI "mulli" rD rA simm::k

    | Pli(rD,v) -> li rD v::k
    | Pcmpwi (0,rS,v) -> cmpwi rS v::k
    | Pcmpwi (crf,rS,v) ->
        { empty_ins with
          memo=sprintf "cmpwi cr%i,^i0,%i" crf v;
          inputs=[rS];
          outputs=[]; }::k
    | Pcmpw(0,rA,rB) -> cmpw rA rB::k
    | Pcmpw(crf,rA,rB) ->
        { empty_ins with
          memo=sprintf "cmpw cr%i,^i0,^i1" crf;
          inputs=[rA;rB];
          outputs=[]; }::k
    | Pb lbl -> jump tr_lab lbl::k
    | Pbcc(cond, lbl) -> bcc tr_lab cond lbl::k
    | Pload (sz,rD,d,rA) ->
         { empty_ins with
          memo = sprintf "%s ^o0,%i(^i0)"  (memo_load sz) d;
          inputs = [rA];
          outputs= [rD]; }::k
    | Plwzu (rD,d,rA) ->
        let outs =
          if rA <> r0 && rA <> rD then [rD;rA;] else [rD;] in
        { empty_ins with
          memo = sprintf "lwzu ^o0,%i(^i0)"  d;
          inputs = [rA];
          outputs= outs; }::k
    | Ploadx (sz,rD,rA,rB) ->
        let memo = memo_loadx sz in
        begin match rA with
        | A.Ireg A.GPR0 -> (* Yes, it's this way cf. ISA p. 48 *)
            { empty_ins with
              memo = memo ^ " ^o0,0,^i0";
              inputs = [rB];
              outputs= [rD]; }::k
        | _ -> 
            { empty_ins with
              memo = memo ^ " ^o0,^i0,^i1";
              inputs = [rA;rB];
              outputs= [rD]; }::k
        end
    | Pstore (sz,rS,d,rA) -> stwsz sz rS d rA::k
    | Pstwu(rS,d,rA) -> stw true rS d rA::k
    | Pstorex (sz,rS,rA,rB) ->
        begin match rA with
        | A.Ireg A.GPR0 -> (* ISA p. 53 *)
            let memo = memo_storex sz in
            { empty_ins with
              memo = memo ^ " ^i0,0,^i1";
              inputs=[rS;rB];
              outputs=[]; }::k
        | _ -> storex sz rS rA rB::k
        end
    | Psync ->
        begin match C.syncmacro with
        | None  ->justOp "sync"::k
        | Some _ -> emit_sync_macro k
        end
    | Plwsync -> justOp "lwsync"::k
    | Pisync -> justOp "isync"::k
    | Peieio -> justOp "eieio"::k
    | Pdcbf (rA,rB) ->
        begin match rA with
        | A.Ireg A.GPR0 -> (* Yes, it's this way cf. ISA p. 415 *)
            { empty_ins with
              memo = "dcbf 0,^i0" ;
              inputs=[rB] ; }
        | _ -> dcbf rA rB
        end::k

    | Plwarx (rT,rA,rB) ->
        begin match rA with
        | A.Ireg A.GPR0 ->
            { empty_ins with
              memo = "lwarx ^o0,0,^i0" ;
              inputs=[rB] ;
              outputs=[rT] ; }
        | _ ->
            { empty_ins with
              memo = "lwarx ^o0,^i0,^i1" ;
              inputs=[rA; rB] ;
              outputs=[rT] ; }
        end::k
    | Pstwcx (rS,rA,rB) ->
        begin match rA with
        | A.Ireg A.GPR0 ->
            { empty_ins with
              memo = "stwcx. ^i0,0,^i1" ;
              inputs=[rS;rB] ;
              outputs=[] ; }
        | _ ->
            { empty_ins with
              memo = "stwcx. ^i0,^i1,^i2" ;
              inputs=[rS ;rA; rB] ;
              outputs=[] ; }
        end::k
(* R. Bornat additions *)
    | Pnor (set, rD, rA, rB) -> op3regs  "nor" set rD rA rB::k
    | Pneg (set, rD, rS) -> op2regs "neg" set rD rS::k
    | Pslw (set, rD, rA, rB) -> op3regs "slw" set rD rA rB::k
    | Psrawi (_set, rD, rS, i) -> op2regsI "srawi" rD rS i::k
    | Psraw (set, rD, rA, rB) -> op3regs "sraw" set rD rA rB::k
    | Pbl lbl -> jump_and_link  tr_lab lbl::k
    | Pblr ->
        { empty_ins with
          memo = "blr" ;
          inputs = [A.LR];
          branch = []; (* Hum *) }::k
    | Pmtlr rS ->
        { empty_ins with
          memo = "mtlr ^i0";
          inputs=[rS];
          outputs=[A.LR]; }::k
    | Pmflr rD ->
        { empty_ins with
          memo = "mflr ^o0";
          outputs=[rD];
          inputs=[A.LR]; }::k
    | Pmfcr rD ->
        { empty_ins with
          memo = "mfcr ^o0";
          outputs=[rD];
          inputs=[]; }::k
    | Plmw (rD,d,rA) ->
        { empty_ins with
          memo =
          begin
            if rA = A.Ireg A.GPR0
            then sprintf "lmw ^o0,%i(r0)" d
            else sprintf "lmw ^o0,%i(^i0)" d
          end ;
          outputs = A.regs_interval rD;
          inputs = begin
            if rA = A.Ireg A.GPR0
            then []
            else [rA]
          end; }::k
    | Pstmw (rS,d,rA) ->
        { empty_ins with
          memo =
          begin
            if rA = A.Ireg A.GPR0
            then sprintf "stmw ^i0,%i(r0)" d
            else sprintf "stmw ^i1,%i(^i0)" d
          end ;
          inputs=
          begin
            let rs = A.regs_interval rS in
            if rA = A.Ireg A.GPR0 then rs else rA::rs;
          end; }::k
    | Pcomment c ->
        { empty_ins with memo = c; comment = true; }::
        (match c with
        | "attn" ->
            { empty_ins with memo = ".long 0x00000200"; }::k
        | _ -> k )

    let extract_addrs _ins = Global_litmus.Set.empty

    let stable_regs ins = match ins with
    | Pstmw (r1,_,_)
    | Plmw (r1,_,_) ->
        A.RegSet.of_list (A.regs_interval r1)
    | _ -> A.RegSet.empty

    let compile_ins is_before ins = do_compile_ins is_before ins

    let branch_diffw r1 r2 lab k = cmpw r1 r2::bcc tr_nolab Ne lab::k
    let branch_neq r i lab k = cmpwi r i::bcc tr_nolab Ne lab::k
    let branch_eq r i lab k = cmpwi r i::bcc tr_nolab Eq lab::k
    let signaling_write i k = li ephemeral i::stw false ephemeral 0 A.signal::k


(*
sldi 9,9,32
rldicl 3,3,0,32
or 3,3,9
*)
    let sldi rD rS i =
      {empty_ins with
       memo = sprintf "sldi ^o0,^i0,%i" i ;
       inputs = [rS] ;
       outputs = [rD] ;}

    let rldicl rD rS i1 i2 =
      {empty_ins with
       memo = sprintf "rldicl ^o0,^i0,%i,%i" i1 i2 ;
       inputs = [rS] ;
       outputs = [rD] ;}


      
    let emit_mftb =
      match C.word with
      | Word.W64 ->
          fun _lbl _r k -> mftb tb1::k
      | Word.W32|Word.WXX ->
          fun lbl r k ->
            let r1 = base
            and r2 = max_idx
            and r3 = idx in
            mftbu r1::
            mftb r2::
            mftbu r3::
            branch_diffw r1 r3 lbl
              (sldi r3 r3 32::
               rldicl r2 r2 0 32::
               op3or r r3 r2::k)
                
              
          
      
    let emit_tb_wait k =
      let lbl_loop = next_label () in
      ld tb0 0 tb_addr0::
      emit_lbl lbl_loop::
      emit_mftb lbl_loop tb1
        (cmpld tb1 tb0::
         bcc tr_nolab Le lbl_loop::
         std tb1 0 tb_addr1::k)

  end
