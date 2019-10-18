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

open Printf
open Code
module type Config = sig
  include CompileCommon.Config
  val realdep : bool
end

module Make(O:Config)(C:sig val eieio : bool end) : XXXCompile_gen.S =
  struct
    open MachSize

    let naturalsize = TypBase.get_size O.typ
    module PPC =
      PPCArch_gen.Make
        (struct
          include C
          let naturalsize = naturalsize
          let moreedges = O.moreedges
        end)

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


    let emit_loop_pair _p st r1 r2 idx addr =
      let lab = Label.next_label "Loop" in
      PPC.Label (lab,PPC.Nop)::
      PPC.lift_code
        [PPC.Plwarx (r1,idx,addr);
         PPC.Pstwcx (r2,idx,addr);
         PPC.Pbcc (PPC.Ne,lab)],
      st

    let emit_unroll_pair u p st r1 r2 idx addr =
      if u <= 0 then
        PPC.lift_code
          [PPC.Plwarx (r1,idx,addr);
           PPC.Pstwcx (r2,idx,addr)],
        st
      else if u = 1 then
        PPC.lift_code
          [PPC.Plwarx (r1,idx,addr);
           PPC.Pstwcx (r2,idx,addr);
           PPC.Pbcc (PPC.Ne,Label.fail p (PPC.current_label st))],
        PPC.next_label_st st
      else
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              PPC.Plwarx (r1,idx,addr)::
              PPC.Pstwcx (r2,idx,addr)::
              PPC.Pbcc (PPC.Ne,Label.fail p (PPC.current_label st))::[]
          | u ->
              PPC.Plwarx (r1,idx,addr)::
              PPC.Pstwcx (r2,idx,addr)::
              PPC.Pbcc (PPC.Eq,out)::
              do_rec (u-1) in
        PPC.lift_code (do_rec u)@[PPC.Label (out,PPC.Nop)],
        PPC.next_label_st st

    let emit_pair = match O.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u


    module Extra = struct
      let use_symbolic = false
      type reg = PPC.reg
      type instruction = PPC.pseudo
      let mov r v = PPC.Instruction (PPC.Pli (r,v))
      let mov_mixed _sz _r _i = assert false
      let mov_reg r1 r2 = PPC.Instruction (PPC.Pori (r1,r2,0))
      let mov_reg_mixed _sz _r1 _r2 = assert false
    end

    module U = GenUtils.Make(O)(PPC)(Extra)

(* STA *)

    let emit_sta_idx_reg st p init x idx rW =
      let rA,init,st = U.next_init st p init x in
      let rR,st = next_reg st in
      let cs,st = emit_pair p st rR rW idx rA in
      rR,init,cs,st

    let emit_sta_idx  st p init x idx  v =
      let rW,init,csi,st = U.emit_mov st p init v in
      let r,init,cs,st = emit_sta_idx_reg st p init x idx rW in
      r,init,csi@cs,st

    let emit_sta_reg st p init x rW = emit_sta_idx_reg st p init x r0 rW

    let emit_sta  st p init x v =
      let rA,init,csi,st = U.emit_mov st p init v in
      let r,init,cs,st = emit_sta_reg st p init x rA in
      r,init,csi@cs,st


(* STORE *)

    let emit_store_reg_mixed sz o st p init x rA =
      let rB,init,st = U.next_init st p init x in
      init,[PPC.Instruction (PPC.Pstore (sz,rA,o,rB))],st

    let emit_store_reg st p init x rA =
      emit_store_reg_mixed naturalsize 0 st p init x rA


    let emit_store_idx_reg  st p init x idx rA =
      let rB,init,st = U.next_init st p init x in
      init,[PPC.Instruction (PPC.Pstorex (Word,rA,idx,rB))],st

    let emit_store_mixed sz o st p init x v =
      let rA,init,csi,st = U.emit_mov st p init v in
      let init,cs,st = emit_store_reg_mixed sz o st p init x rA in
      init,csi@cs,st

    let emit_store st p init x v =
      emit_store_mixed naturalsize 0 st p init x v

    let emit_store_idx st p init x idx v =
      let rA,init,csi,st = U.emit_mov st p init v in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,csi@cs,st

(* LDA *)

    let emit_lda_idx st p init x idx =
      let rA,init,st = U.next_init st p init x in
      let rR,st = next_reg st in
      let cs,st = emit_pair p st rR rR idx rA in
      rR,init,cs,st

    let emit_lda st p init x = emit_lda_idx st p init x r0

(* Load *)

    let emit_load_mixed sz o st p init x =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,PPC.lift_code [PPC.Pload (sz,rA,o,rB)],st

    let emit_load st p init x = emit_load_mixed naturalsize 0 st p init x

    let emit_obs = emit_load_mixed naturalsize 0

    let emit_obs_not_zero st p init x =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      PPC.Label (lab,PPC.Nop)::
      PPC.lift_code
        [PPC.Pload (naturalsize,rA,0,rB) ; PPC.Pcmpwi (0,rA,0) ; PPC.Pbcc (PPC.Eq,lab)],
      st

    let emit_load_one st p init x =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      PPC.Label (lab,PPC.Nop)::
      PPC.lift_code
        [PPC.Pload (Word,rA,0,rB) ; PPC.Pcmpwi (0,rA,1) ; PPC.Pbcc (PPC.Ne,lab)],
      st

    let emit_obs_not st p init x cmp =
      let rA,st = next_reg st in
      let rC,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      PPC.Instruction (PPC.Pli (rC,200))::
      (* 200 X about 5 ins looks for a typical memory delay *)
      PPC.Label (lab,PPC.Nop)::
      PPC.lift_code
        [
         PPC.Pload (naturalsize,rA,0,rB) ; cmp rA ;
         PPC.Pbcc (PPC.Ne,out) ; PPC.Paddi (rC,rC,-1) ;
         PPC.Pcmpwi (0,rC,0) ; PPC.Pbcc (PPC.Ne,lab) ;
       ]@
      [PPC.Label (out,PPC.Nop)],
      st

    let emit_obs_not_eq st p init x rP =
      emit_obs_not st p init x (fun r -> PPC.Pcmpw (0,r,rP))

    let emit_obs_not_value st p init x v =
      emit_obs_not st p init x (fun r -> PPC.Pcmpwi (0,r,v))

    let emit_load_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,PPC.lift_code [PPC.Ploadx (naturalsize,rA,idx,rB)],st

    let emit_lwarx_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,PPC.lift_code [PPC.Plwarx (rA,idx,rB)],st

    let emit_lwarx st p init x =  emit_lwarx_idx st p init x PPC.r0

    let emit_one_stwcx_idx st p init x idx v =
      let rA,init,csi,st = U.emit_mov st p init v in
      let rB,init,st = U.next_init st p init x in
      init,
      csi@PPC.lift_code
             [PPC.Pstwcx (rA,idx,rB);
              PPC.Pbcc (PPC.Ne,Label.fail p (PPC.current_label st))],
      PPC.next_label_st st

    let emit_one_stwcx st p init x v = emit_one_stwcx_idx st p init x PPC.r0 v

    let emit_joker st init = None,init,[],st

    let emit_access st p init e = match e.dir with
    | None -> Warn.fatal "TODO"
    | Some d ->
        begin match e.loc with
        | Code _ -> Warn.fatal "No code location for PPC"
        | Data loc ->
            begin match d,e.atom with
            | R,None ->
                let emit = if e.rmw then emit_lwarx else emit_load in
                let r,init,cs,st = emit st p init loc  in
                Some r,init,cs,st
            | W,None ->
                let init,cs,st = emit_store st p init loc e.v in
                None,init,cs,st
            | R,Some PPC.Atomic ->
                let r,init,cs,st = emit_lda st p init loc in
                Some r,init,cs,st
            | W,Some PPC.Atomic ->
                let r,init,cs,st = emit_sta st p init loc e.v in
                Some r,init,cs,st
            | R,Some PPC.Reserve ->
                let r,init,cs,st = emit_lwarx st p init loc  in
                Some r,init,cs,st
            | W,Some PPC.Reserve ->
                Warn.fatal "No store with reservation"
            | R,Some (PPC.Mixed (sz,o)) ->
                let r,init,cs,st = emit_load_mixed sz o st p init loc  in
                Some r,init,cs,st
            | W,Some (PPC.Mixed (sz,o)) ->
                let init,cs,st = emit_store_mixed sz o st p init loc e.v in
                None,init,cs,st
            | J,_ -> emit_joker st init
            end
        end

    let emit_exch_idx st p init er ew idx =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      let rR,st = next_reg st in
      let rW,init,csi,st = U.emit_mov st p init ew.v in
      let cs,st = emit_pair p st rR rW idx rA in
      rR,init,csi@cs,st

    let emit_exch st p init er ew  = emit_exch_idx st p init er ew r0

    let emit_rmw () st p init er ew  =
      let rR,init,cs,st = emit_exch st p init er ew in
      Some rR,init,cs,st

    let calc_zero =
      if O.realdep then fun dst src ->  PPC.Pandi(dst,src,kbig)
      else fun dst src -> PPC.Pxor(PPC.DontSetCR0,dst,src,src)

    let emit_access_dep_addr st p init e  r1 =
      let r2,st = next_reg st in
      let c = calc_zero r2 r1 in
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | _,Code _ -> Warn.fatal "No code location for PPC"
      | Some d,Data loc ->
          begin match d,e.atom with
          | R,None ->
              let r,init,cs,st = emit_load_idx st p init loc r2 in
              Some r,init, PPC.Instruction c::cs,st
          | R,Some PPC.Reserve ->
              let r,init,cs,st = emit_lwarx_idx st p init loc r2 in
              Some r,init, PPC.Instruction c::cs,st
          | W,None ->
              let init,cs,st = emit_store_idx st p init loc r2 e.v in
              None,init,PPC.Instruction c::cs,st
          | R,Some PPC.Atomic ->
              let r,init,cs,st = emit_lda_idx st p init loc r2 in
              Some r,init, PPC.Instruction c::cs,st
          | W,Some PPC.Atomic ->
              let r,init,cs,st = emit_sta_idx st p init loc r2 e.v in
              Some r,init,PPC.Instruction c::cs,st
          | W,Some PPC.Reserve ->
              Warn.fatal "No store with reservation"
          | _,Some (PPC.Mixed _) ->
              Warn.fatal "addr dep with mixed"
          | J,_ -> emit_joker st init
          end

    let emit_exch_dep_addr st  p init er ew rd =
      let idx,st = next_reg st in
      let c = calc_zero idx rd in
      let r,init,cs,st = emit_exch_idx st p init er ew idx in
      r,init,PPC.Instruction c::cs,st


    let emit_access_dep_data st p init e  r1 =
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some R,_ ->Warn.fatal "data dependency to load"
      | _,Code _ -> Warn.fatal "No code location for PPC"
      | Some W,Data loc ->
          let rW,st = next_reg st in
          let ro,init,st = U.emit_const st p init e.v in
          let cs2 = match ro with
          | None ->
              [PPC.Instruction (calc_zero rW r1) ;
               PPC.Instruction (PPC.Paddi (rW,rW,e.v)) ; ]
          | Some rC ->
              [PPC.Instruction (calc_zero rW r1) ;
               PPC.Instruction (PPC.Padd (PPC.DontSetCR0,rW,rW,rC)) ; ] in
          let ro,init,cs,st =
            match e.atom with
            | None ->
                let init,cs,st = emit_store_reg st p init loc rW in
                None,init,cs,st
            | Some PPC.Atomic ->
                let r,init,cs,st = emit_sta_reg st p init loc rW in
                Some r,init,cs,st
            | Some (PPC.Mixed (sz,o)) ->
                let init,cs,st = emit_store_reg_mixed sz o st p init loc rW in
                None,init,cs,st
            | Some PPC.Reserve -> Warn.fatal "No store with reservation" in
          ro,init,cs2@cs,st
      | Some J,_ -> emit_joker st init

    let insert_isync cs1 cs2 = cs1@[PPC.Instruction PPC.Pisync]@cs2

    let emit_access_ctrl isync st p init e r1 v1 =
      let c =
        if O.realdep then
          let lab = Label.exit p (PPC.current_label st) in
          [PPC.Instruction (PPC.Pcmpwi (0,r1,v1));
           PPC.Instruction (PPC.Pbcc (PPC.Ne,lab))]
        else
          let lab = Label.next_label "LC" in
          [PPC.Instruction (PPC.Pcmpw (0,r1,r1));
           PPC.Instruction (PPC.Pbcc (PPC.Eq,lab));
           PPC.Label (lab,PPC.Nop);] in
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some R,Data loc ->
          let emit = match e.atom with
          | None -> emit_load
          | Some (PPC.Mixed (sz,o)) -> emit_load_mixed sz o
          | Some PPC.Reserve ->emit_lwarx
          | Some PPC.Atomic -> emit_lda in
          let r,init,cs,st = emit st p init loc in
          Some r,init,(if isync then insert_isync c cs else c@cs),st
      | Some W,Data loc ->
          let ro,init,cs,st =
            match e.atom with
            | None ->
                let init,cs,st = emit_store st p init loc e.v in
                None,init,cs,st
            | Some (PPC.Mixed (sz,o)) ->
                let init,cs,st = emit_store_mixed sz o st p init loc e.v in
                None,init,cs,st
            | Some PPC.Reserve -> Warn.fatal "No store with reservation"
            | Some PPC.Atomic ->
                let r,init,cs,st = emit_sta st p init loc e.v in
                Some r,init,cs,st in
          ro,init,(if isync then insert_isync c cs else c@cs),st
      | Some J,_ -> emit_joker st init
      | _,Code _ -> Warn.fatal "No code location for PPC"

    let emit_exch_ctrl isync st p init er ew rd =
      let lab = Label.next_label "LC" in
      let c =
        [PPC.Instruction (PPC.Pcmpw (0,rd,rd));
         PPC.Instruction (PPC.Pbcc (PPC.Eq,lab));
         PPC.Label (lab,PPC.Nop);] in
      let r,init,csr,st = emit_lwarx st p init (as_data er.loc)  in
      let init,csw,st = emit_one_stwcx st p init (as_data ew.loc) ew.v in
      let cs = csr@csw in
      let cs = if isync then insert_isync c cs else c@cs in
      r,init,cs,st

    let emit_access_dep st p init e dp r1 v1 = match dp with
    | PPC.ADDR -> emit_access_dep_addr st p init e r1
    | PPC.DATA -> emit_access_dep_data st p init e r1
    | PPC.CTRL -> emit_access_ctrl false st p init e r1 v1
    | PPC.CTRLISYNC -> emit_access_ctrl true st p init e r1 v1

    let emit_exch_dep st p init er ew dp rd = match dp with
    | PPC.ADDR -> emit_exch_dep_addr st p init er ew rd
    | PPC.DATA -> Warn.fatal "not data dependency to RMW"
    | PPC.CTRL -> emit_exch_ctrl false st p init er ew rd
    | PPC.CTRLISYNC -> emit_exch_ctrl true st p init er ew rd

    let emit_rmw_dep () st p init er ew dp rd =
      let r,init,cs,st = emit_exch_dep  st p init er ew dp rd in
      Some r,init,cs,st

(* Fences *)

    let emit_fence _ _ _ f =
      [PPC.Instruction
        (match f with
        | PPC.Sync -> PPC.Psync
        | PPC.LwSync -> PPC.Plwsync
        | PPC.ISync -> PPC.Pisync
        | PPC.Eieio -> PPC.Peieio)]
    let full_emit_fence = GenUtils.to_full emit_fence
    let stronger_fence = PPC.Sync

(* Check load *)

    let do_check_load p st r e =
      let lab = Label.exit p (PPC.current_label st) in
      (fun k ->
        PPC.Instruction (PPC.Pcmpwi (0,r,e.v))::
        PPC.Instruction (PPC.Pbcc (PPC.Ne,lab))::
        k),
      PPC.next_label_st st

    let check_load  p r e init st =
      let cs,st = do_check_load p st r e in
      init,cs,st

(* Postlude *)

    let list_of_fail_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = PPC.Instruction (PPC.Pb (Label.exit p n))::
            PPC.Label (Label.fail p n,PPC.Nop)::k
        in do_rec (i-1) k'
      in
      do_rec (PPC.current_label st) []

    let list_of_exit_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = PPC.Label (Label.exit p n,PPC.Nop)::k
        in do_rec (i-1) k'
      in
      do_rec (PPC.current_label st) []

    let does_fail p st =
      let l = list_of_fail_labels p st in
      match l with [] -> false | _ -> true

    let does_exit p st =
      let l = list_of_exit_labels p st in
      match l with [] -> false | _ -> true

    let postlude st p init cs =
      if does_fail p st then
        let init,okcs,st = emit_store st p init Code.ok_str 0 in
        init,
        cs@
        (list_of_fail_labels p st)@
        okcs@
        (list_of_exit_labels p st),
        st
      else if does_exit p st then
        init,cs@(list_of_exit_labels p st),st
      else init,cs,st


    let get_xstore_results _ = []

  end
