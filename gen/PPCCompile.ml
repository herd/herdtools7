(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Code

module Make(O:CompileCommon.Config)(C:PPCArch.Config) : XXXCompile.S =
  struct

    module PPC = PPCArch.Make(C) 
    include CompileCommon.Make(O)(PPC)

    let do_sta = O.sta
    let r0 = PPC.Ireg PPC.GPR0

(* PPO *)
    open E
    open R
    let as_opt = function Some x -> x | None -> assert false

    let dprd = as_opt PPC.ddr_default
    let dpwd = as_opt PPC.ddw_default
    let ctrlrd = as_opt PPC.ctrlr_default
    let ctrlwd = as_opt PPC.ctrlw_default

    let dpr f = f (Dp (dprd,Diff,Dir R))      
    and dpw f = f (Dp (dpwd,Diff,Dir W))      
    and ctrlw f = f (Dp (ctrlwd,Diff,Dir W))
    and ctrlr f = f (Dp (ctrlrd,Diff,Dir R))
    and poswr f = f (Po(Same,Dir W,Dir R))

    let dp f k = dpr f (dpw f k)
    and ctrl f k = ctrlr f (ctrlw f k)


    let cons r rs = r (fun r -> r::rs)

    let single f r = f (ERS [plain_edge r])
    let seq rs f = f (ERS (List.map plain_edge rs))

    let do_ppo f k =
      let k = dp (single f) k in
      let k = ctrl (single f) k in
      let k = seq (cons dpr (cons ctrlr [])) f k in
      let k = seq (cons dpr (cons ctrlw [])) f k in
      let k = seq (cons dpr (cons dpr [])) f k in
      let k = seq (cons dpr (cons dpw [])) f k in
      let k = seq (cons dpw (cons poswr [])) f k in
      let k = seq (cons dpw (cons poswr (cons dpr []))) f k in
      let k = seq (cons dpw (cons poswr (cons dpw []))) f k in
      let k = seq (cons dpw (cons poswr (cons ctrlr []))) f k in
      let k = seq (cons dpw (cons poswr (cons ctrlw []))) f k in
(*
  let k = seq (cons ctrlw (cons poswr [])) f k in
  let k = seq (cons ctrlw (cons poswr (cons dpr []))) f k in
 *)
      k 

    let ppoext = false

    let ppo f k =
      do_ppo f
        (if ppoext then
          do_ppo
            (fun r k -> match r with
            | ERS rs -> f (ERS (rs@[plain_edge (Rf Ext)])) k
            | PPO -> assert false)
            k
        else k)

    let () =
      if O.verbose > 0 then begin
        eprintf "PPO is:\n" ;
        ppo (fun r () -> eprintf "%s\n" (R.pp_relax r)) ()
      end 

(*******)


    open C

    let next_reg x = PPC.alloc_reg x

    let next_init st p init loc =
      let rec find_rec = function
        | (PPC.Reg (p0,r0),loc0)::_ when loc0 = loc && p = p0 ->
            r0,init,st
        | _::rem -> find_rec rem
        | [] ->
            let r,st = next_reg st in
            r,(PPC.Reg (p,r),loc)::init,st in
      find_rec init

    let pseudo = List.map (fun i -> PPC.Instruction i)

    let sym = PPC.Symbolic_reg "sta"

    let loop_rmw r1 r2 idx addr =
      let lab = Label.next_label "Loop" in
      PPC.Label (lab,PPC.Nop)::
      pseudo
        [PPC.Plwarx (r1,idx,addr);
         PPC.Pstwcx (r2,idx,addr);
         PPC.Pbcc (PPC.Ne,lab)]
      
    let unroll_rmw p r1 r2 idx addr u  =
      if u <= 0 then
        pseudo
          [PPC.Plwarx (r1,idx,addr);
           PPC.Pstwcx (r2,idx,addr)]
      else if u = 1 then
        pseudo
          [PPC.Plwarx (r1,idx,addr);
           PPC.Pstwcx (r2,idx,addr);
           PPC.Pbcc (PPC.Ne,Label.fail p)]
      else
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              PPC.Plwarx (r1,idx,addr)::
              PPC.Pstwcx (r2,idx,addr)::
              PPC.Pbcc (PPC.Ne,Label.fail p)::[]
          | u ->
              PPC.Plwarx (r1,idx,addr)::
              PPC.Pstwcx (r2,idx,addr)::
              PPC.Pbcc (PPC.Eq,out)::
              do_rec (u-1) in
        pseudo (do_rec u)@[PPC.Label (out,PPC.Nop)]


    let unroll_sta u st p init x idx rA =
      let rB,init,st = next_init st p init x in
      let unroll = unroll_rmw p sym rA idx rB in
      init,unroll u,st

    let loop_sta_reg st p init x rA =
      let rB,init,st = next_init st p init x in
      init,loop_rmw sym rA r0 rB,st

    let emit_sta_reg = match O.unrollatomic with
    | None -> loop_sta_reg
    | Some u ->
        fun st p init x rA -> unroll_sta u st p init x PPC.r0 rA

    let emit_store_reg =
      if do_sta then emit_sta_reg
      else
        fun st p init x rA ->
          let rB,init,st = next_init st p init x in
          init,[PPC.Instruction (PPC.Pstw (rA,0,rB))],st

    let loop_sta_idx_reg st p init x idx rA =
      let rB,init,st = next_init st p init x in
      init,loop_rmw sym rA idx rB,st

    let emit_sta_idx_reg = match O.unrollatomic with
    | None -> loop_sta_idx_reg
    | Some u -> unroll_sta u

    let emit_store_idx_reg =
      if do_sta then emit_sta_idx_reg
      else
        fun st p init x idx rA ->
          let rB,init,st = next_init st p init x in
          init,[PPC.Instruction (PPC.Pstwx (rA,idx,rB))],st


    let emit_store st p init x v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_reg st p init x rA in
      init,PPC.Instruction (PPC.Pli (rA,v))::cs,st

    let emit_sta  st p init x v =
      let rA,st = next_reg st in
      let init,cs,st = emit_sta_reg st p init x rA in
      init,PPC.Instruction (PPC.Pli (rA,v))::cs,st

    let emit_store_idx st p init x idx v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,PPC.Instruction (PPC.Pli (rA,v))::cs,st

    let emit_sta_idx st p init x idx v =
      let rA,st = next_reg st in
      let init,cs,st = emit_sta_idx_reg st p init x idx rA in
      init,PPC.Instruction (PPC.Pli (rA,v))::cs,st

    let emit_load st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [PPC.Plwz (rA,0,rB)],st

    let emit_load_not_zero st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      PPC.Label (lab,PPC.Nop)::
      pseudo
        [PPC.Plwz (rA,0,rB) ; PPC.Pcmpwi (0,rA,0) ; PPC.Pbcc (PPC.Eq,lab)],
      st

    let emit_load_one st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      PPC.Label (lab,PPC.Nop)::
      pseudo
        [PPC.Plwz (rA,0,rB) ; PPC.Pcmpwi (0,rA,1) ; PPC.Pbcc (PPC.Ne,lab)],
      st

    let emit_load_not st p init x cmp =
      let rA,st = next_reg st in
      let rC,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      PPC.Instruction (PPC.Pli (rC,200))::
      (* 200 X about 5 ins looks for a typical memory delay *)
      PPC.Label (lab,PPC.Nop)::
      pseudo
        [
         PPC.Plwz (rA,0,rB) ; cmp rA ;
         PPC.Pbcc (PPC.Ne,out) ; PPC.Paddi (rC,rC,-1) ;
         PPC.Pcmpwi (0,rC,0) ; PPC.Pbcc (PPC.Ne,lab) ;
       ]@
      [PPC.Label (out,PPC.Nop)],
      st

    let emit_load_not_eq st p init x rP =
      emit_load_not st p init x (fun r -> PPC.Pcmpw (0,r,rP))

    let emit_load_not_value st p init x v =
      emit_load_not st p init x (fun r -> PPC.Pcmpwi (0,r,v))

    let unroll_fno u st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let unroll = unroll_rmw p rA rA idx rB in
      rA,init,unroll u,st

    let loop_fno st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,loop_rmw rA rA r0 rB,st

    let emit_fno = match O.unrollatomic with
    | None -> loop_fno
    | Some u -> fun st p init x -> unroll_fno u st p init x PPC.r0

    let emit_fno2  st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init, [PPC.Macro ("FNO2",[rA;rB])],st

    let do_emit_open_fno st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "DIY" in
      rA,init,
      [ PPC.Label (lab,PPC.Instruction (PPC.Plwarx (rA,PPC.r0,rB))) ],
      lab,st

    let unroll_open_fno st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,
      [PPC.Instruction (PPC.Plwarx (rA,PPC.r0,rB)) ],
      "No",st

    let emit_open_fno = match O.unrollatomic with
    | None -> do_emit_open_fno
    | Some _ -> unroll_open_fno

    let do_emit_close_fno st p init lab r x =
      let rB,init,st = next_init st p init x in
      init,
      pseudo [PPC.Pstwcx (r,PPC.r0,rB); PPC.Pbcc (PPC.Ne,lab)],
      st

    let unroll_close_fno u st p init _lab r x =
      let rB,init,st = next_init st p init x in
      let rec do_rec = function
        | 0 -> []
        | u ->
            PPC.Instruction (PPC.Pstwcx (r,PPC.r0,rB))::
            PPC.Instruction (PPC.Pbcc (PPC.Ne,Label.fail p))::
            do_rec (u-1) in
      init,do_rec u,st

    let emit_close_fno = match O.unrollatomic with
    | None -> do_emit_close_fno
    | Some u -> unroll_close_fno u 

    let emit_load_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [PPC.Plwzx (rA,idx,rB)],st

    let emit_lwarx_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [PPC.Plwarx (rA,idx,rB)],st

    let emit_lwarx st p init x =  emit_lwarx_idx st p init x PPC.r0

    let emit_one_stwcx_idx st p init x idx v =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      init,
      pseudo
        [PPC.Pli (rA,v); PPC.Pstwcx (rA,idx,rB);
         PPC.Pbcc (PPC.Ne,Label.fail p)],
      st

    let emit_one_stwcx st p init x v = emit_one_stwcx_idx st p init x PPC.r0 v

    let loop_fno_idx st  p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,loop_rmw rA rA idx rB,st

    let emit_fno_idx = match O.unrollatomic with
    | None -> loop_fno_idx
    | Some u -> unroll_fno u

    let emit_access st p init e = match e.dir,e.atom with
    | R,None ->
        let emit = if e.rmw then emit_lwarx else emit_load in
        let r,init,cs,st = emit st p init e.loc  in
        Some r,init,cs,st
    | W,None ->
        let init,cs,st = emit_store st p init e.loc e.v in
        None,init,cs,st
    | R,Some PPC.Atomic ->
        let r,init,cs,st = emit_fno st p init e.loc in
        Some r,init,cs,st
    | W,Some PPC.Atomic ->
        let init,cs,st = emit_sta st p init e.loc e.v in
        None,init,cs,st
    | R,Some PPC.Reserve ->
        let r,init,cs,st = emit_lwarx st p init e.loc  in
        Some r,init,cs,st
    | W,Some PPC.Reserve ->
        Warn.fatal "No store with reservation"

    let emit_exch st p init er ew =
      let r,init,csr,st = emit_lwarx st p init er.loc  in
      let init,csw,st = emit_one_stwcx st p init ew.loc ew.v in
      r,init,csr@csw,st

    let emit_access_dep_addr st p init e  r1 =
      let r2,st = next_reg st in
      let c = PPC.Pxor(PPC.DontSetCR0,r2,r1,r1) in
      match e.dir,e.atom with
      | R,None ->
          let r,init,cs,st = emit_load_idx st p init e.loc r2 in
          Some r,init, PPC.Instruction c::cs,st
      | R,Some PPC.Reserve ->
          let r,init,cs,st = emit_lwarx_idx st p init e.loc r2 in
          Some r,init, PPC.Instruction c::cs,st
      | W,None ->
          let init,cs,st = emit_store_idx st p init e.loc r2 e.v in
          None,init,PPC.Instruction c::cs,st
      | R,Some PPC.Atomic ->
          let r,init,cs,st = emit_fno_idx st p init e.loc r2 in
          Some r,init, PPC.Instruction c::cs,st
      | W,Some PPC.Atomic ->
          let init,cs,st = emit_sta_idx st p init e.loc r2 e.v in
          None,init,PPC.Instruction c::cs,st
      | W,Some PPC.Reserve ->
          Warn.fatal "No store with reservation"        

    let emit_exch_dep_addr st  p init er ew rd =
      let idx,st = next_reg st in
      let c = PPC.Pxor(PPC.DontSetCR0,idx,rd,rd) in
      let r,init,csr,st = emit_lwarx_idx st p init er.loc idx  in
      let init,csw,st = emit_one_stwcx_idx st p init ew.loc idx ew.v in
      r,init,PPC.Instruction c::csr@csw,st


    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R ->Warn.fatal "data dependency to load"
      | W ->
          let r2,st = next_reg st in
          let cs2 =
            [PPC.Instruction (PPC.Pxor(PPC.DontSetCR0,r2,r1,r1)) ;
             PPC.Instruction (PPC.Paddi (r2,r2,e.v)) ; ] in
          let emit = match e.atom with
          | None -> emit_store_reg
          | Some PPC.Atomic -> emit_sta_reg
          | Some PPC.Reserve -> Warn.fatal "No store with reservation" in
          let init,cs,st = emit st p init e.loc r2 in
          None,init,cs2@cs,st

    let insert_isync cs1 cs2 = cs1@[PPC.Instruction PPC.Pisync]@cs2

    let emit_access_ctrl isync st p init e r1 =      
      let lab = Label.next_label "LC" in
      let c =
        [PPC.Instruction (PPC.Pcmpw (0,r1,r1));
         PPC.Instruction (PPC.Pbcc (PPC.Eq,lab));
         PPC.Label (lab,PPC.Nop);] in
      match e.dir with
      | R ->
          let emit = match e.atom with
          | None -> emit_load
          | Some PPC.Reserve ->emit_lwarx
          | Some PPC.Atomic -> emit_fno in
          let r,init,cs,st = emit st p init e.loc in
          Some r,init,(if isync then insert_isync c cs else c@cs),st
      | W ->
          let emit = match e.atom with
          | None -> emit_store
          | Some PPC.Reserve -> Warn.fatal "No store with reservation"
          | Some PPC.Atomic -> emit_sta in
          let init,cs,st = emit st p init e.loc e.v in
          None,init,(if isync then insert_isync c cs else c@cs),st

    let emit_exch_ctrl isync st p init er ew rd =
      let lab = Label.next_label "LC" in
      let c =
        [PPC.Instruction (PPC.Pcmpw (0,rd,rd));
         PPC.Instruction (PPC.Pbcc (PPC.Eq,lab));
         PPC.Label (lab,PPC.Nop);] in
      let r,init,csr,st = emit_lwarx st p init er.loc  in
      let init,csw,st = emit_one_stwcx st p init ew.loc ew.v in
      let cs = csr@csw in
      let cs = if isync then insert_isync c cs else c@cs in
      r,init,cs,st

    let emit_access_dep st p init e dp r1 = match dp with
    | PPC.ADDR -> emit_access_dep_addr st p init e r1
    | PPC.DATA -> emit_access_dep_data st p init e r1
    | PPC.CTRL -> emit_access_ctrl false st p init e r1
    | PPC.CTRLISYNC -> emit_access_ctrl true st p init e r1

    let emit_exch_dep st p init er ew dp rd = match dp with
    | PPC.ADDR -> emit_exch_dep_addr st p init er ew rd
    | PPC.DATA -> Warn.fatal "not data dependency to RMW"
    | PPC.CTRL -> emit_exch_ctrl false st p init er ew rd
    | PPC.CTRLISYNC -> emit_exch_ctrl true st p init er ew rd


(* Fences *)

    let emit_fence f =
      PPC.Instruction
        (match f with
        | PPC.Sync -> PPC.Psync
        | PPC.LwSync -> PPC.Plwsync
        | PPC.ISync -> PPC.Pisync
        | PPC.Eieio -> PPC.Peieio)

    let stronger_fence = PPC.Sync

    let does_fail p cs =
      let lab = Label.fail p in
      List.exists
        (fun i -> match i with
        | PPC.Instruction (PPC.Pb lab0|PPC.Pbcc (_,lab0)) ->
            (lab0:string) = lab
        | _ -> false)
        cs

    let some_postlude st p init cs =
      if does_fail p cs then
        let init,okcs,st = emit_store st p init Code.ok 0 in
        init,
        cs@
        PPC.Instruction (PPC.Pb (Label.exit p))::
        PPC.Label (Label.fail p,PPC.Nop)::
        okcs@
        [PPC.Label (Label.exit p,PPC.Nop)],
        st
      else init,cs,st


    let postlude = some_postlude
        
  end
