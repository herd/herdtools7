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

open Code

module type Config = sig
  include CompileCommon.Config
  val moreedges : bool
  val realdep : bool
end

module Make(Cfg:Config) : XXXCompile_gen.S =
  struct

(* Common *)
    let naturalsize = TypBase.get_size Cfg.typ
    module A64 =
      AArch64Arch_gen.Make
        (struct
          let naturalsize = naturalsize
          let moreedges = Cfg.moreedges
        end)
    include CompileCommon.Make(Cfg)(A64)

    let ppo _f k = k

    open A64
    open C


(* Utilities *)
    let next_reg x = A64.alloc_reg x
    let pseudo = List.map (fun i -> Instruction i)

    let tempo1 st = A.alloc_trashed_reg "T1" st (* May be used for address *)
    let tempo3 st = A.alloc_trashed_reg "T3" st (* May be used for STRX flag *)

(******************)
(* Idiosyncrasies *)
(******************)

    let vloc = let open TypBase in
    match Cfg.typ with
    | Std (_,MachSize.Quad) -> V64
    | Int |Std (_,MachSize.Word) -> V32
    | t -> Warn.user_error "AArch64, illegal base type: %s" (pp t)

    let sz2v =
      let open MachSize in
      function
        | Byte|Short|Word -> V32
        | Quad -> V64

    let mov r i = I_MOV (vloc,r,K i)
    let mov_mixed sz r i = let v = sz2v sz in I_MOV (v,r,i)

    module Extra = struct
      let use_symbolic = false
      type reg = A64.reg
      type instruction = A64.pseudo
      let mov r i = Instruction (mov r i)
      let mov_mixed sz r i = Instruction (mov_mixed sz r (K i))
    end

    module U = GenUtils.Make(Cfg)(A)(Extra)

    let cbz r1 lbl = I_CBZ (vloc,r1,lbl)
    let cbnz r1 lbl = I_CBNZ (vloc,r1,lbl)
    let cmpi r i = I_OP3 (vloc,SUBS,ZR,r,K i)
    let cmp r1 r2 = I_OP3 (vloc,SUBS,ZR,r1,RV (vloc,r2))
    let bne lbl = I_BC (NE,lbl)
    let eor r1 r2 r3 = I_OP3 (vloc,EOR,r1,r2,RV (vloc,r3))
    let andi r1 r2 k = I_OP3 (vloc,AND,r1,r2,K k)
    let addi r1 r2 k = I_OP3 (vloc,ADD,r1,r2,K k)
    let addi_64 r1 r2 k = I_OP3 (V64,ADD,r1,r2,K k)
(*    let add r1 r2 r3 = I_OP3 (vloc,ADD,r1,r2,r3) *)
    let add v r1 r2 r3 = I_OP3 (v,ADD,r1,r2,RV (v,r3))
    let add64 r1 r2 r3 = I_OP3 (V64,ADD,r1,r2,RV (vloc,r3))

    let ldr_mixed r1 r2 sz o =
      let open MachSize in
      match sz with
      | Byte -> I_LDRBH (B,r1,r2,K o)
      | Short -> I_LDRBH (H,r1,r2,K o)
      | Word -> I_LDR (V32,r1,r2,K o)
      | Quad -> I_LDR (V64,r1,r2,K o)

    let ldr r1 r2 = I_LDR (vloc,r1,r2,K 0)
    let ldar r1 r2 = I_LDAR (vloc,AA,r1,r2)
    let ldapr r1 r2 = I_LDAR (vloc,AQ,r1,r2)
    let ldxr r1 r2 = I_LDAR (vloc,XX,r1,r2)
    let ldaxr r1 r2 = I_LDAR (vloc,AX,r1,r2)
    let sxtw r1 r2 = I_SXTW (r1,r2)
    let ldr_idx r1 r2 idx = I_LDR (vloc,r1,r2,RV (vloc,idx))

    let ldr_mixed_idx v r1 r2 idx sz  =
      let open MachSize in
      match sz with
      | Byte -> I_LDRBH (B,r1,r2,RV (v,idx))
      | Short -> I_LDRBH (H,r1,r2,RV (v,idx))
      | Word -> I_LDR (V32,r1,r2,RV (v,idx))
      | Quad -> I_LDR (V64,r1,r2,RV (v,idx))

    let str_mixed sz o r1 r2 =
      let open MachSize in
      match sz with
      | Byte -> I_STRBH (B,r1,r2,K o)
      | Short -> I_STRBH (H,r1,r2,K o)
      | Word -> I_STR (V32,r1,r2,K o)
      | Quad -> I_STR (V64,r1,r2,K o)

    let str r1 r2 = I_STR (vloc,r1,r2,K 0)
    let stlr r1 r2 = I_STLR (vloc,r1,r2)
    let str_idx r1 r2 idx = I_STR (vloc,r1,r2,RV (vloc,idx))
    let stxr r1 r2 r3 = I_STXR (vloc,YY,r1,r2,r3)
    let stlxr r1 r2 r3 = I_STXR (vloc,LY,r1,r2,r3)

    let stxr_sz t sz r1 r2 r3 =
      let open MachSize in
      match sz with
      | Byte -> I_STXRBH (B,t,r1,r2,r3)
      | Short -> I_STXRBH (H,t,r1,r2,r3)
      | Word -> I_STXR (V32,t,r1,r2,r3)
      | Quad -> I_STXR (V64,t,r1,r2,r3)

    let ldxr_sz t sz r1 r2 =
      let open MachSize in
      match sz with
      | Byte -> I_LDARBH (B,t,r1,r2)
      | Short -> I_LDARBH (H,t,r1,r2)
      | Word -> I_LDAR (V32,t,r1,r2)
      | Quad -> I_LDAR (V64,t,r1,r2)

    let sumi_addr st rA o = match o with
    | 0 -> rA,[],st
    | _ ->
        let r,st = tempo1 st in
        r,[addi_64 r rA o],st

    let str_mixed_idx sz v r1 r2 idx  =
      let open MachSize in
      match sz with
      | Byte -> I_STRBH (B,r1,r2,RV (v,idx))
      | Short -> I_STRBH (H,r1,r2,RV (v,idx))
      | Word -> I_STR (V32,r1,r2,RV (v,idx))
      | Quad -> I_STR (V64,r1,r2,RV (v,idx))

(* Compute address in tempo1 *)
    let _sxtw r k = match vloc with
    | V64 -> k
    | V32 -> sxtw r r::k

    let sum_addr st rA idx =
      let r,st = tempo1 st in
      r,[add64 r rA idx],st

    let stlr_mixed sz o st r1 r2 =
      let open MachSize in
      let rA,cs_sum,st = sumi_addr st r2 o in
      let str = match sz with
      | Byte  -> I_STLRBH (B,r1,rA)
      | Short -> I_STLRBH (H,r1,rA)
      | Word -> I_STLR (V32,r1,rA)
      | Quad -> I_STLR (V64,r1,rA) in
      cs_sum@[str],st

    let stlr_mixed_idx sz r1 r2 idx  =
      let open MachSize in
      match sz with
      | Byte -> I_STRBH (B,r1,r2,RV (V64,idx))
      | Short -> I_STRBH (H,r1,r2,RV (V64,idx))
      | Word -> I_STR (V32,r1,r2,RV (V64,idx))
      | Quad -> I_STR (V64,r1,r2,RV (V64,idx))

    let ldar_mixed t sz o st r1 r2 =
      let rA,cs,st = sumi_addr st r2 o in
      let ld =
        let open MachSize in
        match sz  with
        | Byte -> I_LDARBH (B,t,r1,rA)
        | Short -> I_LDARBH (H,t,r1,rA)
        | Word -> I_LDAR (V32,t,r1,rA)
        | Quad -> I_LDAR (V64,t,r1,rA) in
      cs@[ld],st

    let ldar_mixed_idx t sz o st r1 r2 idx =
      let rA,cs1,st = sumi_addr st r2 o in
      let rA,cs2,st = sum_addr st rA idx in
      let ld =
        let open MachSize in
        match sz  with
        | Byte -> I_LDARBH (B,t,r1,rA)
        | Short -> I_LDARBH (H,t,r1,rA)
        | Word -> I_LDAR (V32,t,r1,rA)
        | Quad -> I_LDAR (V64,t,r1,rA) in
      cs1@cs2@[ld],st

(*********)
(* loads *)
(*********)

    module type L = sig
      val load : A.st -> reg -> reg -> instruction list * A.st
      val load_idx : A.st -> reg -> reg -> reg -> instruction list * A.st
    end

    let emit_load_mixed sz o st p init x =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,lift_code [ldr_mixed rA rB sz o],st

    module LOAD(L:L) =
      struct

        let emit_load st p init x =
          let rA,st = next_reg st in
          let rB,init,st = U.next_init st p init x in
          let ld,st = L.load st rA rB in
          rA,init,lift_code ld,st


        let emit_load_not_zero st p init x =
          let rA,st = next_reg st in
          let rB,init,st = U.next_init st p init x in
          let ld,st = L.load st rA rB in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          lift_code (ld@[cbz rA lab]),
          st

        let emit_load_one st p init x =
          let rA,st = next_reg st in
          let rB,init,st = U.next_init st p init x in
          let ld,st = L.load st rA rB in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          pseudo (ld@[cmpi rA 1; bne lab]),
          st

        let emit_load_not st p init x cmp =
          let rA,st = next_reg st in
          let rC,st = next_reg st in
          let rB,init,st = U.next_init st p init x in
          let ld,st = L.load st rA rB in
          let lab = Label.next_label "L" in
          let out = Label.next_label "L" in
          rA,init,
          Instruction (mov rC 200)::
          (* 200 X about 5 ins looks for a typical memory delay *)
          Label (lab,Nop)::
          pseudo
            (ld@
             [cmp rA;
              bne out; I_OP3 (vloc,SUBS,rC,rC,K 1) ;
              cbnz rC lab ;
            ])@
          [Label (out,Nop)],st

        let emit_load_not_eq st p init x rP =
          emit_load_not st p init x (fun r -> cmp r rP)

        let emit_load_not_value st p init x v =
          emit_load_not st p init x (fun r -> cmpi r v)

        let emit_load_idx st p init x idx =
          let rA,st = next_reg st in
          let rB,init,st = U.next_init st p init x in
          let ins,st = L.load_idx st rA rB idx in
          rA,init,pseudo ins ,st

      end

    let wrap_st emit st r1 r2 =
      let c = emit r1 r2 in
      [c],st

    module LDR =
      LOAD
        (struct
          let load = wrap_st ldr
          let load_idx st rA rB idx = [ldr_idx rA rB idx],st
        end)

(* For export *)
    let emit_load_one = LDR.emit_load_one
    let emit_load = LDR.emit_load
    let emit_load_not_value = LDR.emit_load_not_value
    let emit_load_not_eq = LDR.emit_load_not_eq
    let emit_load_not_zero = LDR.emit_load_not_zero

    module LDAR = LOAD
        (struct
          let load = wrap_st ldar
          let load_idx st rA rB idx =
            let r,ins,st = sum_addr st rB idx in
            ins@[ldar rA r],st
        end)

    module LDAPR = LOAD
        (struct
          let load = wrap_st ldapr
          let load_idx st rA rB idx =
            let r,ins,st = sum_addr st rB idx in
            ins@[ldapr rA r],st
        end)

(**********)
(* Stores *)
(**********)

    module type S = sig
      val store : A.st -> reg -> reg -> instruction list * A.st
      val store_idx : A.st -> reg -> reg -> reg -> instruction list * A.st
    end

    let emit_store_reg_mixed sz o st p init x rA =
      let rB,init,st = U.next_init st p init x in
      init,[Instruction (str_mixed sz o rA rB)],st

    let emit_store_mixed sz o st p init x v =
      let rA,init,csi,st = U.emit_mov_sz sz st p init v in
      let init,cs,st = emit_store_reg_mixed sz o st p init x rA in
      init,csi@cs,st

    module STORE(S:S) =
      struct

        let emit_store_reg st p init x rA =
          let rB,init,st = U.next_init st p init x in
          let cs,st = S.store st rA rB in
          init,pseudo cs,st

        let emit_store st p init x v =
          let rA,init,csi,st = U.emit_mov st p init v in
          let init,cs,st = emit_store_reg st p init x rA in
          init,csi@cs,st

        let emit_store_idx_reg st p init x idx rA =
          let rB,init,st = U.next_init st p init x in
          let ins,st = S.store_idx st rA rB idx in
          init,pseudo ins,st

        let emit_store_idx st p init x idx v =
          let rA,init,csi,st = U.emit_mov st p init v in
          let init,cs,st = emit_store_idx_reg st p init x idx rA in
          init,csi@cs,st

      end

    module STR =
      STORE
        (struct
          let store = wrap_st str
          let store_idx st rA rB idx = [str_idx rA rB idx],st
        end)

    module STLR =
      STORE
        (struct
          let store = wrap_st stlr
          let store_idx  st rA rB idx =
            let r,ins,st = sum_addr st rB idx in
            ins@[stlr rA r],st
        end)



(***************************)
(* Atomic loads and stores *)
(***************************)

    let get_xload = function
      | PP|PL -> ldxr
      | AP|AL -> ldaxr

    and get_xstore = function
      | PP|AP -> stxr
      | PL|AL -> stlxr

    let emit_loop_pair rw _p st rR rW rA =
      let lbl = Label.next_label "Loop" in
      let r,st = tempo3 st in
      let cs =
        [
         Label (lbl,Instruction (get_xload rw rR rA));
         Instruction (get_xstore rw r rW rA);
         Instruction (cbnz r lbl);
       ] in
      cs,st

    let emit_one_pair rw p r rR rW rA k =
      Instruction (get_xload rw rR rA)::
      Instruction (get_xstore rw r rW rA)::
      Instruction (cbnz r (Label.fail p))::k

    let emit_unroll_pair u rw p st rR rW rA =
      if u <= 0 then
        let r,st = next_reg st in
        pseudo
          [get_xload rw rR rA;
           get_xstore rw r rW rA;],st
      else if u = 1 then
        let r,st = tempo3 st in
        emit_one_pair rw p r rR rW rA [],st
      else
        let r,st = tempo3 st in
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              emit_one_pair
                rw p r rR rW rA [Label (out,Nop)]
          | u ->
              Instruction (get_xload rw rR rA)::
              Instruction (get_xstore rw r rW rA)::
              Instruction (cbz r out)::
              do_rec (u-1) in
        do_rec u,st

    let emit_pair = match Cfg.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u


(********************)
(* Mixed size pairs *)
(********************)
    let get_xload_mixed rw = match rw with
    | PP|PL -> ldxr_sz XX
    | AP|AL -> ldxr_sz AX

    let get_xstore_mixed rw = match rw with
    | PP|AP -> stxr_sz YY
    | PL|AL -> stxr_sz LY

    let emit_loop_pair_mixed sz o rw _p st rR rW rA =
      let rA,ci,st = sumi_addr st rA o in
      let lbl = Label.next_label "Loop" in
      let r,st = tempo3 st in
      let cs =
        [
         Label (lbl,Instruction (get_xload_mixed rw sz rR rA));
         Instruction (get_xstore_mixed rw sz r rW rA);
         Instruction (cbnz r lbl);
       ] in
      pseudo ci@cs,st

    let emit_one_pair_mixed sz rw p r rR rW rA k =
      Instruction (get_xload_mixed rw sz rR rA)::
      Instruction (get_xstore_mixed rw sz r rW rA)::
      Instruction (cbnz r (Label.fail p))::k

    let emit_unroll_pair_mixed sz o u rw p st rR rW rA =
      let rA,ci,st = sumi_addr st rA o in
      if u <= 0 then
        let r,st = next_reg st in
        pseudo
          (ci@
           [get_xload_mixed rw sz rR rA;
            get_xstore_mixed rw sz r rW rA;]),st
      else if u = 1 then
        let r,st = tempo3 st in
        pseudo ci@emit_one_pair_mixed sz rw p r rR rW rA [],st
      else
        let r,st = tempo3 st in
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              emit_one_pair_mixed sz rw p r rR rW rA [Label (out,Nop)]
          | u ->
              Instruction (get_xload_mixed rw sz rR rA)::
              Instruction (get_xstore_mixed rw sz r rW rA)::
              Instruction (cbz r out)::
              do_rec (u-1) in
        pseudo ci@do_rec u,st

    let emit_pair_mixed sz o = match Cfg.unrollatomic with
    | None -> emit_loop_pair_mixed sz o
    | Some u -> emit_unroll_pair_mixed sz o u

    let emit_lda_reg rw st p rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair rw p st rR rR rA in
      rR,cs,st

    let emit_lda rw st p init loc =
      let rA,init,st = U.next_init st p init loc in
      let r,cs,st =  emit_lda_reg rw st p rA in
      r,init,cs,st

    let emit_lda_idx rw st p init loc idx =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let r,cs2,st =  emit_lda_reg rw st p rA in
      r,init,pseudo cs1@cs2,st

    let emit_lda_mixed_reg sz o rw st p rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair_mixed sz o  rw p st rR rR rA in
      rR,cs,st

    let emit_lda_mixed sz o rw st p init loc =
      let rA,init,st = U.next_init st p init loc in
      let r,cs,st =  emit_lda_mixed_reg sz o rw st p rA in
      r,init,cs,st

    let emit_lda_mixed_idx sz o rw st p init loc idx =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let r,cs2,st =  emit_lda_mixed_reg sz o rw st p rA in
      r,init,pseudo cs1@cs2,st


    let do_emit_sta rw st p rW rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair rw p st rR rW rA in
      rR,cs,st

    let emit_sta rw st p init loc v =
      let rA,init,st = U.next_init st p init loc in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs,st = do_emit_sta rw st p rW rA in
      rR,init,csi@cs,st

    let emit_sta_reg rw st p init loc rW =
      let rA,init,st = U.next_init st p init loc in
      let rR,cs,st = do_emit_sta rw st p rW rA in
      rR,init,cs,st

    let emit_sta_idx rw st p init loc idx v =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs2,st = do_emit_sta rw st p rW rA in
      rR,init,csi@pseudo cs1@cs2,st


    let do_emit_sta_mixed sz o rw st p rW rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair_mixed sz o rw p st rR rW rA in
      rR,cs,st

    let emit_sta_mixed sz o rw st p init loc v =
      let rA,init,st = U.next_init st p init loc in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs,st = do_emit_sta_mixed sz o rw st p rW rA in
      rR,init,csi@cs,st

    let emit_sta_mixed_reg sz o rw st p init loc rW =
      let rA,init,st = U.next_init st p init loc in
      let rR,cs,st = do_emit_sta_mixed sz o rw st p rW rA in
      rR,init,cs,st

    let emit_sta_mixed_idx sz o rw st p init loc idx v =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs2,st = do_emit_sta_mixed sz o rw st p rW rA in
      rR,init,csi@pseudo cs1@cs2,st


(**********)
(* Access *)
(**********)

    let emit_access  st p init e = match e.dir with
    | None -> Warn.fatal "AArchCompile.emit_access"
    | Some d ->
        match d,e.atom with
        | R,None ->
            let r,init,cs,st = LDR.emit_load st p init e.loc in
            Some r,init,cs,st
        | R,Some (Acq,None) ->
            let r,init,cs,st = LDAR.emit_load st p init e.loc  in
            Some r,init,cs,st
        | R,Some (Acq,Some (sz,o)) ->
            let module L =
              LOAD
                (struct
                  let load = ldar_mixed AA sz o
                  let load_idx = ldar_mixed_idx AQ sz o
                end) in
            let r,init,cs,st = L.emit_load st p init e.loc in
            Some r,init,cs,st
        | R,Some (AcqPc,None) ->
            let r,init,cs,st = LDAPR.emit_load st p init e.loc  in
            Some r,init,cs,st
        | R,Some (AcqPc,Some (sz,o)) ->
            let module L =
              LOAD
                (struct
                  let load = ldar_mixed AQ sz o
                  let load_idx = ldar_mixed_idx AQ sz o
                end) in
            let r,init,cs,st = L.emit_load st p init e.loc in
            Some r,init,cs,st
        | R,Some (Rel,_) ->
            Warn.fatal "No load release"
        | R,Some (Atomic rw,None) ->
            let r,init,cs,st = emit_lda rw st p init e.loc  in
            Some r,init,cs,st
        | R,Some (Atomic rw,Some (sz,o)) ->
            let r,init,cs,st = emit_lda_mixed sz o rw st p init e.loc  in
            Some r,init,cs,st
        | R,Some (Plain,Some (sz,o)) ->
            let r,init,cs,st = emit_load_mixed sz o st p init e.loc in
            Some r,init,cs,st
        | W,None ->
            let init,cs,st = STR.emit_store st p init e.loc e.v in
            None,init,cs,st
        | W,Some (Rel,None) ->
            let init,cs,st = STLR.emit_store st p init e.loc e.v in
            None,init,cs,st
        | W,Some (Acq,_) -> Warn.fatal "No store acquire"
        | W,Some (AcqPc,_) -> Warn.fatal "No store acquirePc"
        | W,Some (Atomic rw,None) ->
            let r,init,cs,st = emit_sta rw st p init e.loc e.v in
            Some r,init,cs,st
        | W,Some (Atomic rw,Some (sz,o)) ->
            let r,init,cs,st = emit_sta_mixed sz o rw st p init e.loc e.v in
            Some r,init,cs,st
        | W,Some (Plain,Some (sz,o)) ->
            let init,cs,st = emit_store_mixed sz o st p init e.loc e.v in
            None,init,cs,st
        | W,Some (Rel,Some (sz,o)) ->
            let module S =
              STORE
                (struct
                  let store = stlr_mixed sz o
                  let store_idx st r1 r2 idx =
                    let cs = [stlr_mixed_idx sz r1 r2 idx] in
                    let cs = match o with
                    | 0 -> cs
                    | _ -> addi_64 idx idx o::cs in
                    cs,st
                end) in
            let init,cs,st = S.emit_store st p init e.loc e.v in
            None,init,cs,st
        | _,Some (Plain,None) -> assert false


    let tr_a ar aw = match ar,aw with
    | None,None -> PP
    | Some (Acq,None),None -> AP
    | None,Some (Rel,None) -> PL
    | Some (Acq,None),Some (Rel,None) -> AL
    | _,_ ->
        Warn.fatal
          "bad atomicity in rmw, %s%s"
          (E.pp_atom_option ar)
          (E.pp_atom_option aw)

    let emit_exch st p init er ew =
      let rA,init,st = U.next_init st p init er.loc in
      let rR,st = next_reg st in
      let rW,init,csi,st = U.emit_mov st p init ew.v in
      let arw = tr_a er.C.atom ew.C.atom in
      let cs,st = emit_pair arw p st rR rW rA in
      rR,init,csi@cs,st

(* Fences *)

    let emit_fence f =  Instruction (I_FENCE f)

    let stronger_fence = strong


(* Dependencies *)
    let calc0 =
      if Cfg.realdep then
        fun dst src ->  andi dst src 128
      else
        fun dst src -> eor dst src src

    let emit_access_dep_addr st p init e rd =
      let r2,st = next_reg st in
      let c =  calc0 r2 rd in
      match e.dir with
      | None -> Warn.fatal "TODO"
      | Some d ->
          match d,e.atom with
          | R,None ->
              let r,init,cs,st = LDR.emit_load_idx st p init e.loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Acq,None) ->
              let r,init,cs,st = LDAR.emit_load_idx st p init e.loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Acq,Some (sz,o)) ->
              let module L =
                LOAD
                  (struct
                    let load = ldar_mixed AA sz o
                    let load_idx = ldar_mixed_idx AA sz o
                  end) in
              let r,init,cs,st = L.emit_load_idx st p init e.loc r2 in
              Some r,init,Instruction c::cs,st
          | R,Some (AcqPc,None) ->
              let r,init,cs,st = LDAPR.emit_load_idx st p init e.loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (AcqPc,Some (sz,o)) ->
              let module L =
                LOAD
                  (struct
                    let load = ldar_mixed AQ sz o
                    let load_idx = ldar_mixed_idx AQ sz o
                  end) in
              let r,init,cs,st = L.emit_load_idx st p init e.loc r2 in
              Some r,init,Instruction c::cs,st
          | R,Some (Rel,_) ->
              Warn.fatal "No load release"
          | R,Some (Atomic rw,None) ->
              let r,init,cs,st = emit_lda_idx rw st p init e.loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_lda_mixed_idx sz o rw st p init e.loc r2 in
              Some r,init, Instruction c::cs,st
          | W,None ->
              let init,cs,st = STR.emit_store_idx st p init e.loc r2 e.v in
              None,init,Instruction c::cs,st
          | W,Some (Rel,None) ->
              let init,cs,st = STLR.emit_store_idx st p init e.loc r2 e.v in
              None,init,Instruction c::cs,st
          | W,Some (Rel,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store = stlr_mixed sz o
                    let store_idx st r1 r2 idx =
                      let cs = [stlr_mixed_idx sz r1 r2 idx] in
                      let cs = match o with
                      | 0 -> cs
                      | _ -> addi_64 idx idx o::cs in
                      cs,st
                  end) in
              let init,cs,st = S.emit_store_idx st p init e.loc r2 e.v in
              None,init,Instruction c::cs,st
          | W,Some (Acq,_) -> Warn.fatal "No store acquire"
          | W,Some (AcqPc,_) -> Warn.fatal "No store acquirePc"
          | W,Some (Atomic rw,None) ->
              let r,init,cs,st = emit_sta_idx rw st p init e.loc r2 e.v in
              Some r,init,Instruction c::cs,st
          | W,Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_sta_mixed_idx sz o rw st p init e.loc r2 e.v in
              Some r,init,Instruction c::cs,st
          | R,Some (Plain,Some (sz,o)) ->
              let module L =
                LOAD
                  (struct
                    let load st r1 r2 = [ldr_mixed r1 r2 sz o],st
                    let load_idx st r1 r2 idx =
                      let cs = [ldr_mixed_idx V64 r1 r2 idx sz] in
                      let cs = match o with
                      | 0 -> cs
                      | _ -> addi_64 idx idx o::cs in
                      cs,st
                  end) in
              let r,init,cs,st = L.emit_load_idx st p init e.loc r2 in
              Some r,init,Instruction c::cs,st
          | W,Some (Plain,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store = wrap_st (str_mixed sz o)
                    let store_idx st r1 r2 idx =
                      let cs = [str_mixed_idx sz V64 r1 r2 idx] in
                      let cs = match o with
                      | 0 -> cs
                      | _ -> addi_64 idx idx o::cs in
                      cs,st
                  end) in
              let rA,init,cs_mov,st = U.emit_mov_sz sz st p init e.v in
              let init,cs,st = S.emit_store_idx_reg st p init e.loc r2 rA in
              None,init,Instruction c::cs_mov@cs,st
          | _,Some (Plain,None) -> assert false


    let emit_exch_dep_addr st p init er ew rd =
      let r2,st = next_reg st in
      let c = calc0 r2 rd in
      let rA,init,st = U.next_init st p init er.loc in
      let rA,csum,st = sum_addr st rA r2 in
      let rR,st = next_reg st in
      let rW,init,csi,st = U.emit_mov st p init ew.v in
      let arw = tr_a er.C.atom ew.C.atom in
      let cs,st = emit_pair arw p st rR rW rA in
      rR,init,
      csi@pseudo (c::csum)@cs,
      st

    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | None -> Warn.fatal "TODO"
      | Some R -> Warn.fatal "data dependency to load"
      | Some W ->
          let r2,cs2,init,st = match e.atom with
          | Some (_,Some (sz,_)) ->
              let rA,init,csA,st = U.emit_mov_sz sz st p init e.v in
              let r2,st = next_reg st in
              let cs2 =
                [Instruction (calc0 r2 r1) ;
                 Instruction (add (sz2v sz) r2 r2 rA); ] in
              r2,csA@cs2,init,st
          | _ ->
              let r2,st = next_reg st in
              let cs2 =
                [Instruction (calc0 r2 r1) ;
                 Instruction (addi r2 r2 e.v); ] in
              r2,cs2,init,st in
          begin match e.atom with
          | None ->
              let init,cs,st = STR.emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some (Rel,None) ->
              let init,cs,st = STLR.emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some (Rel,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store = stlr_mixed sz o
                    let store_idx _st _r1 _r2 _idx = assert false
                  end) in
              let init,cs,st = S.emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some (Atomic rw,None) ->
              let r,init,cs,st = emit_sta_reg rw st p init e.loc r2 in
              Some r,init,cs2@cs,st
          | Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_sta_mixed_reg sz o rw st p init e.loc r2 in
              Some r,init,cs2@cs,st
          | Some (Acq,_) ->
              Warn.fatal "No store acquire"
          | Some (AcqPc,_) ->
              Warn.fatal "No store acquirePc"
          | Some (Plain,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store  = wrap_st (str_mixed sz o)
                    let store_idx st r1 r2 idx =
                      let cs = [str_mixed_idx sz V64 r1 r2 idx] in
                      let cs = match o with
                      | 0 -> cs
                      | _ -> addi_64 idx idx o::cs in
                      cs,st
                  end) in
              let init,cs,st = S.emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some (Plain,None) -> assert false
          end

    let insert_isb isb cs1 cs2 =
      if isb then cs1@[emit_fence ISB]@cs2
      else cs1@cs2

    let emit_access_ctrl isb st p init e r1 =
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (cbnz r1 lab);
         Label (lab,Nop);] in
      let ropt,init,cs,st = emit_access st p init e in
      ropt,init,insert_isb isb c cs,st

    let emit_exch_ctrl isb st p init er ew r1 =
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (cbnz r1 lab);
         Label (lab,Nop);] in
      let ropt,init,cs,st = emit_exch st p init er ew in
      ropt,init,insert_isb isb c cs,st


    let emit_access_dep st p init e dp r1 _v1 = match dp with
    | ADDR -> emit_access_dep_addr st p init e r1
    | DATA -> emit_access_dep_data st p init e r1
    | CTRL -> emit_access_ctrl false st p init e r1
    | CTRLISYNC -> emit_access_ctrl true st p init e r1

    let emit_exch_dep st p init er ew dp rd = match dp with
    | ADDR -> emit_exch_dep_addr   st p init er ew rd
    | DATA -> Warn.fatal "not data dependency to RMW"
    | CTRL -> emit_exch_ctrl false st p init er ew rd
    | CTRLISYNC -> emit_exch_ctrl true st p init er ew rd


    let do_check_load p r e =
      let lab = Label.exit p in
      (fun k ->
        Instruction (cmpi r e.v)::
        Instruction (bne lab)::
        k)
    let check_load  p r e init st = init,do_check_load p r e,st

(* Postlude *)
    let does_jump lab cs =
      List.exists
        (fun i -> match i with
        | Instruction (I_B lab0|I_BC (_,lab0)
        | I_CBZ (_,_,lab0)|I_CBNZ (_,_,lab0)) ->
            (lab0:string) = lab
        | _ -> false)
        cs

    let does_fail p cs = does_jump (Label.fail p) cs
    let does_exit p cs = does_jump (Label.exit p) cs

    let postlude st p init cs =
      if does_fail p cs then
        let init,okcs,st = STR.emit_store st p init Code.ok 0 in
        init,
        cs@
        Instruction (I_B (Label.exit p))::
        Label (Label.fail p,Nop)::
        okcs@
        [Label (Label.exit p,Nop)],
        st
      else if does_exit p cs then
        init,cs@[Label (Label.exit p,Nop)],st
      else
        init,cs,st


    let get_strx_result k = function
      | I_STXR (_,_,r,_,_)  -> r::k
      | _ -> k

    let get_strx_result_pseudo k = pseudo_fold  get_strx_result k

    let get_xstore_results = match Cfg.unrollatomic with
    | Some x when x <= 0 ->
        fun cs ->
          let rs = List.fold_left get_strx_result_pseudo [] cs in
          List.rev_map (fun r -> r,0) rs
    | Some _|None -> fun _ -> []

  end
