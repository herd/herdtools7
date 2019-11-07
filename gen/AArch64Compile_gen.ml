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
          let fullmixed = Cfg.variant Variant_gen.FullMixed
          let variant = Cfg.variant
        end)
    include CompileCommon.Make(Cfg)(A64)

    let ppo _f k = k

    open A64
    open C

(* Nop instr code *)
    let nop = "0x14000001"

(* Utilities *)
    let next_reg x = A64.alloc_reg x
    let pseudo = List.map (fun i -> Instruction i)

    let tempo1 st = A.alloc_trashed_reg "T1" st (* May be used for address *)
    let tempo2 st = A.alloc_trashed_reg "T2" st (* May be used for second address *)
    let tempo3 st = A.alloc_trashed_reg "T3" st (* May be used for STRX flag *)

(******************)
(* Idiosyncrasies *)
(******************)

    let vloc = let open TypBase in
    match Cfg.typ with
    | Std (_,MachSize.Quad) -> V64
    | Int |Std (_,MachSize.Word) -> V32
    | Std (_,MachSize.(Short|Byte)) -> V32


    let sz2v =
      let open MachSize in
      function
        | Byte|Short|Word -> V32
        | Quad -> V64

    let mov r i = I_MOV (vloc,r,K i)
    let mov_mixed sz r i = let v = sz2v sz in I_MOV (v,r,i)

    let mov_reg r1 r2 = I_MOV (vloc,r1,RV (vloc,r2))
    let mov_reg_mixed sz r1 r2 = let v = sz2v sz in I_MOV (v,r1,RV (v,r2))

    module Extra = struct
      let use_symbolic = false
      type reg = A64.reg
      type instruction = A64.pseudo

      let mov r i = Instruction (mov r i)
      let mov_mixed sz r i = Instruction (mov_mixed sz r (K i))
      let mov_reg r1 r2 = Instruction (mov_reg r1 r2)
      let mov_reg_mixed sz r1 r2 = Instruction (mov_reg_mixed sz r1 r2)

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
    let ldg r1 r2 = I_LDG (r1,r2,K 0)
    let ldar r1 r2 = I_LDAR (vloc,AA,r1,r2)
    let ldapr r1 r2 = I_LDAR (vloc,AQ,r1,r2)
    let ldxr r1 r2 = I_LDAR (vloc,XX,r1,r2)
    let ldaxr r1 r2 = I_LDAR (vloc,AX,r1,r2)
    let sxtw r1 r2 = I_SXTW (r1,r2)
    let ldr_idx r1 r2 idx = I_LDR (vloc,r1,r2,RV (vloc,idx))
    let ldg_idx r1 r2 idx = I_LDG (r1,r2,RV (vloc,idx))

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
    let stg r1 r2 = I_STG (r1,r2,K 0)
    let stlr r1 r2 = I_STLR (vloc,r1,r2)
    let str_idx r1 r2 idx = I_STR (vloc,r1,r2,RV (vloc,idx))
    let stg_idx r1 r2 idx = I_STG (r1,r2,RV (vloc,idx))
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

    let sumi_addr_gen tempo st rA o = match o with
    | 0 -> rA,[],st
    | _ ->
        let r,st = tempo st in
        r,[addi_64 r rA o],st

    let sumi_addr st rA o = sumi_addr_gen tempo1 st rA o

    let str_mixed_idx sz v r1 r2 idx  =
      let open MachSize in
      match sz with
      | Byte -> I_STRBH (B,r1,r2,RV (v,idx))
      | Short -> I_STRBH (H,r1,r2,RV (v,idx))
      | Word -> I_STR (V32,r1,r2,RV (v,idx))
      | Quad -> I_STR (V64,r1,r2,RV (v,idx))

    let swp_mixed sz a rS rT rN =
      let open MachSize in
      match sz with
      | Byte -> I_SWPBH (B,a,rS,rT,rN)
      | Short ->  I_SWPBH (H,a,rS,rT,rN)
      | Word ->  I_SWP (V32,a,rS,rT,rN)
      | Quad ->  I_SWP (V64,a,rS,rT,rN)

    let swp a rS rT rN =  I_SWP (vloc,a,rS,rT,rN)

    let cas_mixed sz a rS rT rN =
      let open MachSize in
      match sz with
      | Byte -> I_CASBH (B,a,rS,rT,rN)
      | Short ->  I_CASBH (H,a,rS,rT,rN)
      | Word ->  I_CAS (V32,a,rS,rT,rN)
      | Quad ->  I_CAS (V64,a,rS,rT,rN)

    let cas a rS rT rN =  I_CAS (vloc,a,rS,rT,rN)

    let ldop_mixed op sz a rS rT rN =
      let open MachSize in
      match sz with
      | Byte -> I_LDOPBH (op,B,a,rS,rT,rN)
      | Short ->  I_LDOPBH (op,H,a,rS,rT,rN)
      | Word ->  I_LDOP (op,V32,a,rS,rT,rN)
      | Quad ->  I_LDOP (op,V64,a,rS,rT,rN)

    let ldop op a rS rT rN =  I_LDOP (op,vloc,a,rS,rT,rN)

    let stop_mixed op sz a rS rN =
      let open MachSize in
      match sz with
      | Byte -> I_STOPBH (op,B,a,rS,rN)
      | Short ->  I_STOPBH (op,H,a,rS,rN)
      | Word ->  I_STOP (op,V32,a,rS,rN)
      | Quad ->  I_STOP (op,V64,a,rS,rN)

    let stop op a rS rN =  I_STOP (op,vloc,a,rS,rN)

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

        let emit_fetch st _p init lab =
          let rA,st = next_reg st in
          let lab0 = Label.next_label "L" in
          let lab1 = Label.next_label "L" in
          let cs =
            Label (lab,Instruction (I_B lab0))::
            Instruction (mov rA 2)::
            Instruction (I_B lab1)::
            Label (lab0,Instruction (mov rA 1))::
            Label (lab1,Nop)::
            [] in
          rA,init,cs,st

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

    module LDG =
      LOAD
        (struct
          let load = wrap_st ldg
          let load_idx st rA rB idx = [ldg_idx rA rB idx],st
        end)

    module OBS =
      LOAD
        (struct
          let load st rA rB = [ldr_mixed rA rB naturalsize 0],st
          let load_idx st rA rB idx =
            [ldr_mixed_idx vloc rA rB idx naturalsize],st
        end)

(* For export *)
    let emit_load_one = LDR.emit_load_one
    let emit_load = LDR.emit_load

    let emit_obs = emit_load_mixed naturalsize 0
    let emit_obs_not_value = OBS.emit_load_not_value
    let emit_obs_not_eq = OBS.emit_load_not_eq
    let emit_obs_not_zero = OBS.emit_load_not_zero

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

        let emit_store_nop st p init lab =
          let rA,init,st = U.emit_nop st p init nop in
          let rB,init,st = U.next_init st p init lab in
          let cs,st = S.store st rA rB in
          init,pseudo cs,st
      end

    module STR =
      STORE
        (struct
          let store = wrap_st str
          let store_idx st rA rB idx = [str_idx rA rB idx],st
        end)

    module STG =
      STORE
        (struct
          let store = wrap_st stg
          let store_idx st rA rB idx = [stg_idx rA rB idx],st
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
      | (Plain,None) ->ldxr
      | (Plain,Some (sz,_)) -> ldxr_sz XX sz
      | (Acq,None)   -> ldaxr
      | (Acq,Some (sz,_)) -> ldxr_sz AX sz
      | (AcqPc,_) -> Warn.fatal "AcqPC annotation on xload"
      | _ -> assert false

    and get_xstore = function
      | (Plain,None) -> stxr
      | (Plain,Some (sz,_)) -> stxr_sz YY sz
      | (Rel,None) -> stlxr
      | (Rel,Some (sz,_)) -> stxr_sz LY sz
      | _ -> assert false

    let get_rmw_addrs arw st rA = match arw with
    | (_,(None|Some (_,0))),(_,(None|Some (_,0)))
        -> rA,rA,[],st
    | (_,Some (_,o1)),(_,Some (_,o2)) when o1=o2 ->
        let r,cs,st = sumi_addr st rA o1 in
        r,r,cs,st
    |  (_,(None|Some (_,0))),(_,Some (_,o)) ->
        let  r,cs,st = sumi_addr st rA o in
        rA,r,cs,st
    |  (_,Some (_,o)),(_,(None|Some (_,0))) ->
        let  r,cs,st = sumi_addr st rA o in
        r,rA,cs,st
    | (_,Some (_,o1)),(_,Some (_,o2)) ->
        let  r1,cs1,st = sumi_addr_gen tempo1 st rA o1 in
        let  r2,cs2,st = sumi_addr_gen tempo2 st rA o2 in
        r1,r2,cs1@cs2,st

    let emit_loop_pair (ar,aw as arw) _p st init rR rW rA =
      let rAR,rAW,cs0,st = get_rmw_addrs arw st rA in
      let lbl = Label.next_label "Loop" in
      let r,st = tempo3 st in
      let cs =
        [
         Label (lbl,Instruction (get_xload ar rR rAR));
         Instruction (get_xstore aw r rW rAW);
         Instruction (cbnz r lbl);
       ] in
      init,pseudo cs0@cs,st

(*    let emit_one_pair (ar,aw) p st r rR rW rAR rAW k n =
      Instruction (get_xload ar rR rAR)::
      Instruction (get_xstore aw r rW rAW)::
      Instruction (cbnz r (Label.fail p n))::k

    let emit_unroll_pair u (ar,aw as arw) p st init rR rW rA =
      let n = current_label st in
      let rAR,rAW,cs0,st = get_rmw_addrs arw st rA in
      let cs0 = pseudo cs0 in
      if u <= 0 then
        let r,st = next_reg st in
        init,cs0@pseudo
           [get_xload ar rR rAR;
           get_xstore aw r rW rAW;],st
      else if u = 1 then
        let r,st = tempo3 st in
        let cs = (emit_one_pair arw p st r rR rW rAR rAW [] n) in
        init,cs0@cs,st*)

    let emit_one_pair (ar, aw) p st init r rR rW rA k =
      let n = current_label st in
      let loc_ok = Code.as_data (Code.myok p n) in
      let init,cs,st =
        STR.emit_store st p init loc_ok 0 in
      (A.Loc loc_ok,Some "1")::init,
      Instruction (get_xload ar rR rA)::
      Instruction (get_xstore aw r rW rA)::
      Instruction (cbnz r (Label.fail p n))::
      Instruction (I_B (Label.exit p n))::
      Label (Label.fail p n,Nop)::
      cs@(Label (Label.exit p n,Nop))::k,
      next_label_st st

    let emit_unroll_pair u (ar, aw as arw) p st init rR rW rA =
      let rAR,rAW,cs0,st = get_rmw_addrs arw st rA in
      let cs0 = pseudo cs0 in
      if u <= 0 then
        let r,st = next_reg st in
        init,cs0@pseudo
          [get_xload ar rR rAR;
           get_xstore aw r rW rAW;],
        st
      else if u = 1 then
        let r,st = tempo3 st in
        let init,cs,st = emit_one_pair arw p st init r rR rW rA [] in
        init,cs0@cs,st
      else
        let r,st = tempo3 st in
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              let init,cs,st = emit_one_pair
                arw p st init r rR rW rA [Label (out,Nop)] in
              init,cs0@cs,st
          | u ->
              let init,cs,st = do_rec (u-1) in
              init,
              (Instruction (get_xload ar rR rA)::
              Instruction (get_xstore aw r rW rA)::
              Instruction (cbz r out)::
              cs0@cs),st in
        do_rec u

    let emit_pair = match Cfg.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u

(* Translate annotations *)

    let tr_rw = function
      | PP -> (Plain,None),(Plain,None)
      | PL -> (Plain,None),(Rel,None)
      | AP -> (Acq,None),(Plain,None)
      | AL -> (Acq,None),(Rel,None)

    let tr_none = function
      | None -> Plain,None
      | Some p -> p


(********************)
(* Mixed size pairs *)
(********************)

    let emit_pair_mixed sz o rw =
      let arw = match tr_rw rw with
      | (a1,_),(a2,_) -> (a1,Some (sz,o)),(a2,Some (sz,o)) in
      emit_pair arw

(********************************)
(* Individual loads and strores *)
(********************************)

    let emit_lda_reg rw st init p rA =
      let rR,st = next_reg st in
      let _,cs,st = emit_pair rw p st init rR rR rA in
      rR,cs,st

    let emit_lda rw st p init loc =
      let rA,init,st = U.next_init st p init loc in
      let r,cs,st =  emit_lda_reg rw st init p rA in
      r,init,cs,st

    let emit_lda_idx rw st p init loc idx =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let r,cs2,st =  emit_lda_reg rw st init p rA in
      r,init,pseudo cs1@cs2,st

    let emit_lda_mixed_reg sz o rw st p init rA =
      let rR,st = next_reg st in
      let _,cs,st = emit_pair_mixed sz o  rw p st init rR rR rA in
      rR,cs,st

    let emit_lda_mixed sz o rw st p init loc =
      let rA,init,st = U.next_init st p init loc in
      let r,cs,st =  emit_lda_mixed_reg sz o rw st p init rA in
      r,init,cs,st

    let emit_lda_mixed_idx sz o rw st p init loc idx =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let r,cs2,st =  emit_lda_mixed_reg sz o rw st p init rA in
      r,init,pseudo cs1@cs2,st


    let do_emit_sta rw st p init rW rA =
      let rR,st = next_reg st in
      let _,cs,st = emit_pair rw p st init rR rW rA in
      rR,cs,st

    let emit_sta rw st p init loc v =
      let rA,init,st = U.next_init st p init loc in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs,st = do_emit_sta rw st p init rW rA in
      rR,init,csi@cs,st

    let emit_sta_reg rw st p init loc rW =
      let rA,init,st = U.next_init st p init loc in
      let rR,cs,st = do_emit_sta rw st p init rW rA in
      rR,init,cs,st

    let emit_sta_idx rw st p init loc idx v =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs2,st = do_emit_sta rw st p init rW rA in
      rR,init,csi@pseudo cs1@cs2,st

    let do_emit_sta_mixed sz o rw st p init rW rA =
      let rR,st = next_reg st in
      let _,cs,st = emit_pair_mixed sz o rw p st init rR rW rA in
      rR,cs,st

    let emit_sta_mixed sz o rw st p init loc v =
      let rA,init,st = U.next_init st p init loc in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs,st = do_emit_sta_mixed sz o rw st p init rW rA in
      rR,init,csi@cs,st

    let emit_sta_mixed_reg sz o rw st p init loc rW =
      let rA,init,st = U.next_init st p init loc in
      let rR,cs,st = do_emit_sta_mixed sz o rw st p init rW rA in
      rR,init,cs,st

    let emit_sta_mixed_idx sz o rw st p init loc idx v =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,cs2,st = do_emit_sta_mixed sz o rw st p init rW rA in
      rR,init,csi@pseudo cs1@cs2,st

(**********)
(* Access *)
(**********)
    let emit_joker st init = None,init,[],st

    let emit_access  st p init e = match e.dir,e.loc with
    | None,_ -> Warn.fatal "AArchCompile.emit_access"
    | Some d,Code lab ->
        begin match d,e.atom with
        | R,None ->
            let r,init,cs,st = LDR.emit_fetch st p init lab in
            Some r,init,cs,st
        | W,None ->
            let init,cs,st = STR.emit_store_nop st p init lab in
            None,init,cs,st
        | _,_ -> Warn.fatal "Not Yet (%s,%s)!!!"
              (pp_dir d) (C.debug_evt e)
        end
    | Some d,Data loc ->
        begin match d,e.atom with
        | R,None ->
            let r,init,cs,st = LDR.emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (Acq,None) ->
            let r,init,cs,st = LDAR.emit_load st p init loc  in
            Some r,init,cs,st
        | R,Some (Acq,Some (sz,o)) ->
            let module L =
              LOAD
                (struct
                  let load = ldar_mixed AA sz o
                  let load_idx = ldar_mixed_idx AQ sz o
                end) in
            let r,init,cs,st = L.emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (AcqPc,None) ->
            let r,init,cs,st = LDAPR.emit_load st p init loc  in
            Some r,init,cs,st
        | R,Some (AcqPc,Some (sz,o)) ->
            let module L =
              LOAD
                (struct
                  let load = ldar_mixed AQ sz o
                  let load_idx = ldar_mixed_idx AQ sz o
                end) in
            let r,init,cs,st = L.emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (Rel,_) ->
            Warn.fatal "No load release"
        | R,Some (Atomic rw,None) ->
            let r,init,cs,st = emit_lda (tr_rw rw) st p init loc  in
            Some r,init,cs,st
        | R,Some (Atomic rw,Some (sz,o)) ->
            let r,init,cs,st = emit_lda_mixed sz o rw st p init loc  in
            Some r,init,cs,st
        | R,Some (Plain,Some (sz,o)) ->
            let r,init,cs,st = emit_load_mixed sz o st p init loc in
            Some r,init,cs,st
        | R,Some (Tag,None) ->
            let r,init,cs,st = LDG.emit_load st p init loc  in
            Some r,init,cs,st
        | W,None ->
            let init,cs,st = STR.emit_store st p init loc e.v in
            None,init,cs,st
        | W,Some (Rel,None) ->
            let init,cs,st = STLR.emit_store st p init loc e.v in
            None,init,cs,st
        | W,Some (Acq,_) -> Warn.fatal "No store acquire"
        | W,Some (AcqPc,_) -> Warn.fatal "No store acquirePc"
        | W,Some (Atomic rw,None) ->
            let r,init,cs,st = emit_sta (tr_rw rw) st p init loc e.v in
            Some r,init,cs,st
        | W,Some (Atomic rw,Some (sz,o)) ->
            let r,init,cs,st = emit_sta_mixed sz o rw st p init loc e.v in
            Some r,init,cs,st
        | W,Some (Plain,Some (sz,o)) ->
            let init,cs,st = emit_store_mixed sz o st p init loc e.v in
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
            let init,cs,st = S.emit_store st p init loc e.v in
            None,init,cs,st
        | W,Some (Tag,None) ->
            let init,cs,st = STG.emit_store st p init loc e.v in
            None,init,cs,st
        | _,Some (Plain,None) -> assert false
        | _,Some (Tag,_) -> assert false
        | J,_ -> emit_joker st init
        end

    let emit_exch st p init er ew =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      let rR,st = next_reg st in
      let rW,init,csi,st = U.emit_mov st p init ew.v in
      let arw = (tr_none er.C.atom, tr_none ew.C.atom) in
      let init,cs,st = emit_pair arw p st init rR rW rA in
      rR,init,csi@cs,st

    let do_sz sz1 sz2 = match sz1,sz2 with
    | None,None -> None
    | Some s1,Some s2 when s1 = s2 -> sz1
    | _,_ ->
        Warn.fatal "Amo instructions with difference sizes"

    let do_rmw_type a1 a2 = match a1,a2 with
    | Plain,Plain -> RMW_P
    | Acq,Plain   -> RMW_A
    | Plain,Rel   -> RMW_L
    | Acq,Rel   -> RMW_AL
    | _,_ ->
        Warn.fatal "Bad annotation for Amo: R=%s, W=%s"
          (pp_atom_acc a1) (pp_atom_acc a2)

    let do_rmw_annot (ar,szr) (aw,szw) =
      let sz =  do_sz szr szw in
      let a = do_rmw_type ar aw in
      sz,a

    let mk_emit_mov sz = match sz with
    | None ->  U.emit_mov
    | Some (sz,_) ->  U.emit_mov_sz sz

    let mk_emit_mov_fresh sz = match sz with
    | None ->  U.emit_mov_fresh
    | Some (sz,_) ->  U.emit_mov_sz_fresh sz

    let do_emit_ldop_rA  ins ins_mixed st p init er ew rA =
      let sz,a = do_rmw_annot (tr_none er.C.atom) (tr_none ew.C.atom) in
      let rR,st = next_reg st in
      let rW,init,csi,st = mk_emit_mov sz st p init ew.v in
      let cs,st = match sz with
      | None -> [ins a rW rR rA],st
      | Some (sz,o) ->
          let rA,cs,st = sumi_addr st rA o in
          cs@[ins_mixed sz a rW rR rA],st in
      rR,init,csi@pseudo cs,st

    let do_emit_ldop ins ins_mixed st p init er ew =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      do_emit_ldop_rA ins ins_mixed st p init er ew rA

    let emit_swp =  do_emit_ldop swp swp_mixed
    and emit_ldop op = do_emit_ldop (ldop op) (ldop_mixed op)

    let emit_cas_rA st p init er ew rA =
      let sz,a = do_rmw_annot (tr_none er.C.atom) (tr_none ew.C.atom) in
      let rS,init,csS,st = mk_emit_mov_fresh sz st p init er.v in
      let rT,init,csT,st = mk_emit_mov sz st p init ew.v in
      let cs,st = match sz with
      | None -> [cas a rS rT rA],st
      | Some (sz,o) ->
          let rA,cs,st = sumi_addr st rA o in
          cs@[cas_mixed sz a rS rT rA],st in
      rS,init,csS@csT@pseudo cs,st

    let emit_cas  st p init er ew =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      emit_cas_rA st p init er ew rA

    let emit_stop_rA op st p init _er ew rA =
      let a,sz = tr_none ew.C.atom in
      let a = match a with
      | Plain -> W_P
      | Rel -> W_L
      | _ ->
          Warn.fatal "Unexpected atom in STOP instruction: %s"
            (pp_atom_acc a) in
      let rW,init,csi,st = mk_emit_mov sz st p init ew.v in
      let cs,st = match sz with
      | None -> [stop op a rW rA],st
      | Some (sz,o) ->
          let rA,cs,st = sumi_addr st rA o in
          cs@[stop_mixed op sz a rW rA],st in
      None,init,csi@pseudo cs,st

    let emit_stop  op st p init er ew =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      emit_stop_rA op st p init er ew rA

    let map_some f st p init er ew =
      let r,init,cs,st = f  st p init er ew in
      Some r,init,cs,st

    let emit_rmw rmw = match rmw with
    | LrSc -> map_some emit_exch
    | Swp -> map_some emit_swp
    | Cas -> map_some emit_cas
    | LdOp op -> map_some (emit_ldop op)
    | StOp op -> emit_stop op

(* Fences *)
    let emit_cachesync s isb r =
      pseudo
            (I_DC ((match s with Strong -> DC.civac | Weak -> DC.cvau),r)::
             I_FENCE (DSB (ISH,FULL))::
             I_IC (IC.ivau,r)::
             I_FENCE (DSB (ISH,FULL))::
             (if isb then [I_FENCE ISB] else []))

    let emit_fence p init n f = match f with
    | Barrier f -> [Instruction (I_FENCE f)]
    | CacheSync (s,isb) ->
        try
          let lab = C.find_prev_code_write n in
          let r = U.find_init p init lab in
          emit_cachesync s isb r
        with Not_found -> Warn.user_error "No code write before CacheSync"


    let full_emit_fence st p init n f = match f with
    | Barrier f -> init,[Instruction (I_FENCE f)],st
    | CacheSync (s,isb) ->
        try
          let lab = C.find_prev_code_write n in
          let r,init,st = U.next_init st p init lab in
          init,emit_cachesync s isb r,st
        with Not_found ->
          Warn.user_error "No code write before CacheSync"

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
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some d,Data loc ->
          begin match d,e.atom with
          | R,None ->
              let r,init,cs,st = LDR.emit_load_idx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Acq,None) ->
              let r,init,cs,st = LDAR.emit_load_idx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Acq,Some (sz,o)) ->
              let module L =
                LOAD
                  (struct
                    let load = ldar_mixed AA sz o
                    let load_idx = ldar_mixed_idx AA sz o
                  end) in
              let r,init,cs,st = L.emit_load_idx st p init loc r2 in
              Some r,init,Instruction c::cs,st
          | R,Some (AcqPc,None) ->
              let r,init,cs,st = LDAPR.emit_load_idx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (AcqPc,Some (sz,o)) ->
              let module L =
                LOAD
                  (struct
                    let load = ldar_mixed AQ sz o
                    let load_idx = ldar_mixed_idx AQ sz o
                  end) in
              let r,init,cs,st = L.emit_load_idx st p init loc r2 in
              Some r,init,Instruction c::cs,st
          | R,Some (Rel,_) ->
              Warn.fatal "No load release"
          | R,Some (Atomic rw,None) ->
              let r,init,cs,st = emit_lda_idx (tr_rw rw) st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_lda_mixed_idx sz o rw st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Tag,None) ->
              let r,init,cs,st = LDG.emit_load_idx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some (Tag,Some _) -> assert false
          | W,None ->
              let init,cs,st = STR.emit_store_idx st p init loc r2 e.v in
              None,init,Instruction c::cs,st
          | W,Some (Rel,None) ->
              let init,cs,st = STLR.emit_store_idx st p init loc r2 e.v in
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
              let init,cs,st = S.emit_store_idx st p init loc r2 e.v in
              None,init,Instruction c::cs,st
          | W,Some (Acq,_) -> Warn.fatal "No store acquire"
          | W,Some (AcqPc,_) -> Warn.fatal "No store acquirePc"
          | W,Some (Atomic rw,None) ->
              let r,init,cs,st =
                emit_sta_idx (tr_rw rw) st p init loc r2 e.v in
              Some r,init,Instruction c::cs,st
          | W,Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_sta_mixed_idx sz o rw st p init loc r2 e.v in
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
              let r,init,cs,st = L.emit_load_idx st p init loc r2 in
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
              let init,cs,st = S.emit_store_idx_reg st p init loc r2 rA in
              None,init,Instruction c::cs_mov@cs,st
          | W,Some (Tag, None) ->
              let init,cs,st = STG.emit_store_idx st p init loc r2 e.v in
              None,init,Instruction c::cs,st
          | W,Some (Tag,Some _) -> assert false
          | J,_ -> emit_joker st init
          | _,Some (Plain,None) -> assert false
          end
      | _,Code _ -> Warn.fatal "No dependency to code location"

    let emit_addr_dep  st p init loc rd =
      let r2,st = next_reg st in
      let c = calc0 r2 rd in
      let rA,init,st = U.next_init st p init loc in
      let rA,csum,st = sum_addr st rA r2 in
      rA,init,pseudo (c::csum),st

    let emit_exch_dep_addr st p init er ew rd =
      let rA,init,caddr,st =  emit_addr_dep  st p init (as_data er.loc) rd in
      let rR,st = next_reg st in
      let rW,init,csi,st = U.emit_mov st p init ew.v in
      let arw = (tr_none  er.C.atom, tr_none ew.C.atom) in
      let init,cs,st = emit_pair arw p st init rR rW rA in
      rR,init,
      csi@caddr@cs,
      st

    let emit_access_dep_data st p init e  r1 =
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some R,_ -> Warn.fatal "data dependency to load"
      | Some W,Data loc ->
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
              let init,cs,st = STR.emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some (Rel,None) ->
              let init,cs,st = STLR.emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some (Rel,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store = stlr_mixed sz o
                    let store_idx _st _r1 _r2 _idx = assert false
                  end) in
              let init,cs,st = S.emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some (Atomic rw,None) ->
              let r,init,cs,st = emit_sta_reg (tr_rw rw) st p init loc r2 in
              Some r,init,cs2@cs,st
          | Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_sta_mixed_reg sz o rw st p init loc r2 in
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
              let init,cs,st = S.emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some (Tag, None) ->
              let init,cs,st = STG.emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some (Plain,None) -> assert false
          | Some (Tag,Some _) -> assert false
          end
      | Some J,_ -> emit_joker st init
      | _,Code _ -> Warn.fatal "Not Yet (%s,dep_data)" (C.debug_evt e)

    let is_ctrlisync = function
      | CTRLISYNC -> true
      | _ -> false

    let insert_isb isb cs1 cs2 =
      if isb then cs1@[Instruction (I_FENCE ISB)]@cs2
      else cs1@cs2

    let emit_ctrl r =
      let lab = Label.next_label "LC" in
      let c = [Instruction (cbnz r lab); Label (lab,Nop);] in c

    let emit_access_ctrl isb st p init e r1 =
      let c = emit_ctrl r1 in
      let ropt,init,cs,st = emit_access st p init e in
      ropt,init,insert_isb isb c cs,st

    let emit_exch_ctrl isb st p init er ew r1 =
      let c = emit_ctrl r1 in
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

    let emit_ldop_dep ins ins_mixed  st p init er ew dp rd = match dp with
    | ADDR ->
        let rA,init,caddr,st = emit_addr_dep st p init (as_data er.loc) rd in
        let rR,init,cs,st = do_emit_ldop_rA ins ins_mixed st p init er ew rA in
        rR,init,caddr@cs,st
    | CTRL|CTRLISYNC ->
        let c = emit_ctrl rd in
        let rR,init,cs,st = do_emit_ldop ins ins_mixed st p init er ew in
        rR,init,insert_isb (is_ctrlisync dp) c cs,st
    | DATA -> Warn.fatal "Data dependency to LDOP"

    let emit_cas_dep  st p init er ew dp rd = match dp with
    | ADDR ->
        let rA,init,caddr,st = emit_addr_dep st p init (as_data er.loc) rd in
        let rR,init,cs,st = emit_cas_rA st p init er ew rA in
        rR,init,caddr@cs,st
    | CTRL|CTRLISYNC ->
        let c = emit_ctrl rd in
        let rR,init,cs,st = emit_cas st p init er ew in
        rR,init,insert_isb (is_ctrlisync dp) c cs,st
    | DATA -> Warn.fatal "Data dependency to CAS"

    let emit_stop_dep  op st p init er ew dp rd = match dp with
    | ADDR ->
        let rA,init,caddr,st = emit_addr_dep st p init (as_data er.loc) rd in
        let rR,init,cs,st = emit_stop_rA op st p init er ew rA in
        rR,init,caddr@cs,st
    | CTRL|CTRLISYNC ->
        let c = emit_ctrl rd in
        let rR,init,cs,st = emit_stop op st p init er ew in
        rR,init,insert_isb (is_ctrlisync dp) c cs,st
    | DATA -> Warn.fatal "Data dependency to STOP"


    let map_some_dp f st p init er ew dp rd =
      let r,init,cs,st = f  st p init er ew dp rd in
      Some r,init,cs,st

    let emit_rmw_dep rmw = match rmw with
    | LrSc -> map_some_dp emit_exch_dep
    | LdOp op -> map_some_dp (emit_ldop_dep (ldop op) (ldop_mixed op))
    | Swp ->  map_some_dp (emit_ldop_dep swp swp_mixed)
    | Cas -> map_some_dp emit_cas_dep
    | StOp op -> emit_stop_dep op


    let do_check_load p st r e =
      let lab = Label.exit p (current_label st) in
      (fun k ->
        Instruction (cmpi r e.v)::
        Instruction (bne lab)::
        k),
      next_label_st st
    let check_load  p r e init st =
      let cs,st = do_check_load p st r e in
      init,cs,st

(* Postlude *)

    let list_of_fail_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = Instruction (I_B (Label.exit p n))::
                        Label (Label.fail p n,Nop)::k
               in do_rec (i-1) k'
      in
    do_rec (current_label st) []

    let list_of_exit_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = Label (Label.exit p n,Nop)::k
               in do_rec (i-1) k'
      in
    do_rec (current_label st) []

   let does_fail p st =
     let l = list_of_fail_labels p st in
     match l with [] -> false | _ -> true

   let does_exit p st =
     let l = list_of_exit_labels p st in
     match l with [] -> false | _ -> true

   let postlude st p init cs =
      if does_fail p st then
       init,
       cs,
       st
      else if does_exit p st then
        init,cs@(list_of_exit_labels p st),st
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
