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

module Make(O:CompileCommon.Config)(C:sig val eieio : bool end) : XXXCompile.S =
  struct

    let naturalsize = TypBase.get_size O.typ
    module PPC =
      PPCArch.Make
        (struct include C let naturalsize = naturalsize end)

    include CompileCommon.Make(O)(PPC)

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

    let next_const st p init v =
      let r,st = next_reg st in
      r,(PPC.Reg (p,r),sprintf "0x%x" v)::init,st

    let pseudo = List.map (fun i -> PPC.Instruction i)

    let emit_loop_pair _p r1 r2 idx addr =
      let lab = Label.next_label "Loop" in
      PPC.Label (lab,PPC.Nop)::
      pseudo
        [PPC.Plwarx (r1,idx,addr);
         PPC.Pstwcx (r2,idx,addr);
         PPC.Pbcc (PPC.Ne,lab)]
      
    let emit_unroll_pair u p r1 r2 idx addr =
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

    let emit_pair = match O.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u


(* STA *)

    let emit_sta_idx_reg st p init x idx rW =
      let rA,init,st = next_init st p init x in
      let rR,st = next_reg st in
      let cs = emit_pair p rR rW idx rA in
      rR,init,cs,st

    let emit_sta_idx  st p init x idx  v =
      let rW,st = next_reg st in
      let r,init,cs,st = emit_sta_idx_reg st p init x idx rW in
      r,init,PPC.Instruction (PPC.Pli (rW,v))::cs,st
      
    let emit_sta_reg st p init x rW = emit_sta_idx_reg st p init x r0 rW

    let emit_sta  st p init x v =
      let rA,st = next_reg st in
      let r,init,cs,st = emit_sta_reg st p init x rA in
      r,init,PPC.Instruction (PPC.Pli (rA,v))::cs,st


(* STORE *)

    let emit_store_reg_mixed sz o st p init x rA =
      let rB,init,st = next_init st p init x in
      init,[PPC.Instruction (PPC.Pstore (sz,rA,o,rB))],st

    let emit_store_reg st p init x rA =
      emit_store_reg_mixed naturalsize 0 st p init x rA


    let emit_store_idx_reg  st p init x idx rA =
      let rB,init,st = next_init st p init x in
      init,[PPC.Instruction (PPC.Pstwx (rA,idx,rB))],st


    let emit_const st p init v =
      if 0 <= v && v < 0xffff then
        None,init,st
      else
        let rA,init,st = next_const st p init v in
        Some rA,init,st

    let emit_li st p init v = match emit_const st p init v with
    | None,init,st ->
        let rA,st = next_reg st in
        rA,init,[PPC.Instruction (PPC.Pli (rA,v))],st
    | Some rA,init,st ->
        rA,init,[],st

    let emit_store_mixed sz o st p init x v =
      let rA,init,csi,st = emit_li st p init v in
      let init,cs,st = emit_store_reg_mixed sz o st p init x rA in
      init,csi@cs,st

    let emit_store st p init x v =
      emit_store_mixed naturalsize 0 st p init x v

    let emit_store_idx st p init x idx v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,PPC.Instruction (PPC.Pli (rA,v))::cs,st

(* LDA *)

    let emit_lda_idx st p init x idx =
      let rA,init,st = next_init st p init x in
      let rR,st = next_reg st in
      let cs = emit_pair p rR rR idx rA in
      rR,init,cs,st

    let emit_lda st p init x = emit_lda_idx st p init x r0

(* Load *)

    let emit_load_mixed sz o st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [PPC.Pload (sz,rA,o,rB)],st

    let emit_load st p init x = emit_load_mixed naturalsize 0 st p init x

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

    let emit_load_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [PPC.Ploadx (naturalsize,rA,idx,rB)],st

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

    let emit_access st p init e = match e.dir,e.atom with
    | R,None ->
        let emit = if e.rmw then emit_lwarx else emit_load in
        let r,init,cs,st = emit st p init e.loc  in
        Some r,init,cs,st
    | W,None ->
        let init,cs,st = emit_store st p init e.loc e.v in
        None,init,cs,st
    | R,Some PPC.Atomic ->
        let r,init,cs,st = emit_lda st p init e.loc in
        Some r,init,cs,st
    | W,Some PPC.Atomic ->
        let r,init,cs,st = emit_sta st p init e.loc e.v in
        Some r,init,cs,st
    | R,Some PPC.Reserve ->
        let r,init,cs,st = emit_lwarx st p init e.loc  in
        Some r,init,cs,st
    | W,Some PPC.Reserve ->
        Warn.fatal "No store with reservation"
    | R,Some (PPC.Mixed (sz,o)) ->
        let r,init,cs,st = emit_load_mixed sz o st p init e.loc  in
        Some r,init,cs,st
    | W,Some (PPC.Mixed (sz,o)) ->
        let init,cs,st = emit_store_mixed sz o st p init e.loc e.v in
        None,init,cs,st

    let emit_exch_idx st p init er ew idx =
      let rA,init,st = next_init st p init er.loc in
      let rR,st = next_reg st in
      let rW,st = next_reg st in
      let cs = emit_pair p rR rW idx rA in
      rR,init,
      PPC.Instruction (PPC.Pli (rW,ew.v))::cs,
      st

    let emit_exch st p init er ew  = emit_exch_idx st p init er ew r0

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
          let r,init,cs,st = emit_lda_idx st p init e.loc r2 in
          Some r,init, PPC.Instruction c::cs,st
      | W,Some PPC.Atomic ->
          let r,init,cs,st = emit_sta_idx st p init e.loc r2 e.v in
          Some r,init,PPC.Instruction c::cs,st
      | W,Some PPC.Reserve ->
          Warn.fatal "No store with reservation"        
      | _,Some (PPC.Mixed _) ->
          Warn.fatal "addr dep with mixed"

    let emit_exch_dep_addr st  p init er ew rd =
      let idx,st = next_reg st in
      let c = PPC.Pxor(PPC.DontSetCR0,idx,rd,rd) in
      let r,init,cs,st = emit_exch_idx st p init er ew idx in
      r,init,PPC.Instruction c::cs,st


    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R ->Warn.fatal "data dependency to load"
      | W ->
          let rW,st = next_reg st in
          let ro,init,st = emit_const st p init e.v in
          let cs2 = match ro with
          | None ->
              [PPC.Instruction (PPC.Pxor(PPC.DontSetCR0,rW,r1,r1)) ;
               PPC.Instruction (PPC.Paddi (rW,rW,e.v)) ; ]
          | Some rC ->
               [PPC.Instruction (PPC.Pxor(PPC.DontSetCR0,rW,r1,r1)) ;
               PPC.Instruction (PPC.Padd (PPC.DontSetCR0,rW,rW,rC)) ; ] in
          let ro,init,cs,st =
            match e.atom with
            | None ->
                let init,cs,st = emit_store_reg st p init e.loc rW in
                None,init,cs,st
            | Some PPC.Atomic ->
                let r,init,cs,st = emit_sta_reg st p init e.loc rW in
                Some r,init,cs,st
            | Some (PPC.Mixed (sz,o)) ->
                let init,cs,st = emit_store_reg_mixed sz o st p init e.loc rW in
                None,init,cs,st
            | Some PPC.Reserve -> Warn.fatal "No store with reservation" in
          ro,init,cs2@cs,st

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
          | Some (PPC.Mixed (sz,o)) -> emit_load_mixed sz o
          | Some PPC.Reserve ->emit_lwarx
          | Some PPC.Atomic -> emit_lda in
          let r,init,cs,st = emit st p init e.loc in
          Some r,init,(if isync then insert_isync c cs else c@cs),st
      | W ->
          let ro,init,cs,st =
            match e.atom with
            | None ->
                let init,cs,st = emit_store st p init e.loc e.v in
                None,init,cs,st
            | Some (PPC.Mixed (sz,o)) ->
                let init,cs,st = emit_store_mixed sz o st p init e.loc e.v in
                None,init,cs,st
            | Some PPC.Reserve -> Warn.fatal "No store with reservation"
            | Some PPC.Atomic ->
                let r,init,cs,st = emit_sta st p init e.loc e.v in
                Some r,init,cs,st in
          ro,init,(if isync then insert_isync c cs else c@cs),st

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

(* Check load *)

    let check_load p r e =
      let lab = Label.exit p in
      fun k ->
        PPC.Instruction (PPC.Pcmpwi (0,r,e.v))::
        PPC.Instruction (PPC.Pbcc (PPC.Ne,lab))::
        k

(* Postlude *)

    let does_jump lab cs =
      List.exists
        (fun i -> match i with
        | PPC.Instruction (PPC.Pb lab0|PPC.Pbcc (_,lab0)) ->
            (lab0:string) = lab
        | _ -> false)
        cs

    let does_fail p cs = does_jump (Label.fail p) cs
    let does_exit p cs = does_jump (Label.exit p) cs

    let postlude st p init cs =
      if does_fail p cs then
        let init,okcs,st = emit_store st p init Code.ok 0 in
        init,
        cs@
        PPC.Instruction (PPC.Pb (Label.exit p))::
        PPC.Label (Label.fail p,PPC.Nop)::
        okcs@
        [PPC.Label (Label.exit p,PPC.Nop)],
        st
      else if does_exit p cs then
        init,cs@[PPC.Label (Label.exit p,PPC.Nop)],st
      else init,cs,st


        
  end
