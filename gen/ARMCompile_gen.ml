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

open Code

module type Config = sig
  include CompileCommon.Config
  val realdep : bool
end

module Make(Cfg:Config) : XXXCompile_gen.S =
  struct

    module ARM = ARMArch_gen.Make(Cfg)

    include CompileCommon.Make(Cfg)(ARM)


(******)
    let ppo _f k = k

    open ARM
    open C


(* Utilities *)
    let next_reg x = ARM.alloc_reg x

   module Extra = struct
     let use_symbolic = true
     type reg = ARM.reg
     type instruction = ARM.pseudo
     let mov r v = Instruction (I_MOVI (r,v,AL))
     let mov_mixed _sz _r _v = Warn.fatal "No mixed size for ARM"
     let mov_reg r1 r2 = Instruction (I_MOV (r1,r2,AL))
     let mov_reg_mixed _sz _r _v = Warn.fatal "No mixed size for ARM"
   end

    module U = GenUtils.Make(Cfg)(ARM)(Extra)

(* RMW utilities *)
    let tempo1 = ARM.Symbolic_reg "T1" (* May be used for address *)
    let tempo2 = ARM.Symbolic_reg "T2" (* utility *)


    let emit_loop_pair _p st r1 r2 addr =
      let lab = Label.next_label "Loop" in
      Label (lab,Nop)::
      lift_code
        [I_LDREX (r1,addr) ;
         I_STREX (tempo2,r2,addr,AL);
         I_CMPI (tempo2,0);
         I_BNE (lab);
       ],st

    let emit_unroll_pair u p st r1 r2 addr =
      if u <= 0 then
        lift_code
          [I_LDREX (r1,addr);
           I_STREX (tempo2,r2,addr,AL);],
        st
      else if u = 1 then
        lift_code
          [I_LDREX (r1,addr);
           I_STREX (tempo2,r2,addr,AL);
           I_CMPI (tempo2,0);
           I_BNE (Label.fail p (current_label st));],
        next_label_st st
      else
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              [I_LDREX (r1,addr);
               I_STREX (tempo2,r2,addr,AL);
               I_CMPI (tempo2,0);
               I_BNE (Label.fail p (current_label st));]
          | u ->
              I_LDREX (r1,addr)::
              I_STREX (tempo2,r2,addr,AL)::
              I_CMPI (tempo2,0)::
              I_BEQ (out)::
              do_rec (u-1) in
        lift_code (do_rec u)@[Label (out,Nop)],
        next_label_st st

    let emit_pair = match Cfg.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u

(*********)
(* loads *)
(*********)

    let emit_load st p init x =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,lift_code [I_LDR (rA,rB,AL)],st

    let emit_obs = emit_load

    let emit_obs_not_zero st p init x =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::lift_code [I_LDR (rA,rB,AL); I_CMPI (rA,0); I_BEQ (lab)],
      st

    let emit_load_one st p init x =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::lift_code [I_LDR (rA,rB,AL); I_CMPI (rA,1); I_BNE (lab)],
      st

    let emit_load_not st p init x cmp =
      let rA,st = next_reg st in
      let rC,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      Instruction (I_MOVI (rC,200,AL))::
      (* 200 X about 5 ins looks for a typical memory delay *)
      Label (lab,Nop)::
      lift_code
        [
         I_LDR (rA,rB,AL); cmp rA ;
         I_BNE out; I_ADD (DontSetFlags,rC,rC,-1) ;
         I_CMPI (rC,0) ; I_BNE lab ;
       ]@
      [Label (out,Nop)],
      st

    let emit_obs_not_eq st p init x rP =
      emit_load_not st p init x (fun r -> I_CMP (r,rP))

    let emit_obs_not_value st p init x v =
      emit_load_not st p init x (fun r -> I_CMPI (r,v))

    let emit_load_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,lift_code [I_LDR3 (rA,idx,rB,AL)],st

(**********)
(* Stores *)
(**********)

    let emit_store_reg st p init x rA =
      let rB,init,st = U.next_init st p init x in
      init,[Instruction (I_STR (rA,rB,AL))],st

    let emit_store_idx_reg st p init x idx rA =
      let rB,init,st = U.next_init st p init x in
      init,[Instruction (I_STR3 (rA,idx,rB,AL))],st

    let emit_store st p init x v =
      let rA,init,csi,st = U.emit_mov st p init v in
      let init,cs,st = emit_store_reg st p init x rA in
      init,csi@cs,st

    let emit_store_idx st p init x idx v =
      let rA,init,csi,st = U.emit_mov st p init v in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,csi@cs,st

    let emit_one_strex_reg st p init rA v =
      let rV,init,csi,st = U.emit_mov st p init v in
      init,
      csi@lift_code
        [I_STREX (tempo2,rV,rA,AL);
         I_CMPI (tempo2,0);
         I_BNE (Label.fail p (current_label st));],
      next_label_st st

    let emit_ldrex_reg st _p init rB =
      let rA,st = next_reg st in
      rA,init,lift_code [I_LDREX (rA,rB)],st

    let emit_ldrex st p init x =
      let rB,init,st = U.next_init st p init x in
      emit_ldrex_reg st p init rB

    let emit_ldrex_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,
      lift_code [I_ADD3 (DontSetFlags,tempo1,idx,rB); I_LDREX (rA,tempo1)],st


(* LDA *)
    let emit_lda st p init x =
      let rA,init,st = U.next_init st p init x in
      let rR,st = next_reg st in
      let cs,st = emit_pair p st rR rR rA in
      rR,init,cs,st

    let emit_lda_idx st p init x idx =
      let rA,init,st = U.next_init st p init x in
      let rR,st = next_reg st in
      let cs,st = emit_pair p st rR rR tempo1 in
      rR,init,
      Instruction (I_ADD3 (DontSetFlags,tempo1,idx,rA))::cs,
      st

(* STA *)

    let emit_sta_reg st p init x rW =
      let rA,init,st = U.next_init st p init x in
      let rR,st = next_reg st in
      let cs,st = emit_pair p st rR rW rA in
      Some rR,init,cs,st

    let emit_sta st p init x v =
      let rW,init,csi,st = U.emit_mov st p init v in
      let ro,init,cs,st = emit_sta_reg st p init x rW in
      ro,init,csi@cs,st

    let emit_sta_idx st p init x idx v =
      let rA,init,st = U.next_init st p init x in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,st = next_reg st in
      let cs,st = emit_pair p st rR rW tempo1 in
      Some rR,
      init,
      csi@Instruction (I_ADD3 (DontSetFlags,tempo1,idx,rA))::cs,st


(*************)
(* Acccesses *)
(*************)

    let emit_joker st init = None,init,[],st

    let emit_access  st p init e =  match e.dir with
    | None ->  Warn.fatal "ARMCompile.emit_access"
    | Some d ->
        match d,e.atom,e.loc with
        | R,None,Data loc ->
            let r,init,cs,st = emit_load st p init loc in
            Some r,init,cs,st
        | R,Some Reserve,Data loc ->
            let r,init,cs,st = emit_ldrex st p init loc  in
            Some r,init,cs,st
        | R,Some Atomic,Data loc ->
            let r,init,cs,st = emit_lda st p init loc  in
            Some r,init,cs,st
        | W,None,Data loc ->
            let init,cs,st = emit_store st p init loc e.v in
            None,init,cs,st
        | W,Some Reserve,Data _ -> Warn.fatal "No store with reservation"
        | W,Some Atomic,Data loc ->
            let ro,init,cs,st = emit_sta st p init loc e.v in
            ro,init,cs,st
        | _,Some (Mixed _),Data _ -> assert false
        | Code.J,_,Data _ -> emit_joker st init
        | _,_,Code _ -> Warn.fatal "No code location in ARM"

    let emit_exch st p init er ew =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      let rW,init,csi,st = U.emit_mov st p init ew.v in
      let rR,st = next_reg st in
      let cs,st = emit_pair p st rR rW rA in
      Some rR,init,csi@cs,st

    let emit_rmw () = emit_exch

    let calc0 =
      if Cfg.realdep then
        fun r2 r1 -> I_AND (DontSetFlags,r2,r1,128)
      else
        fun r2 r1 -> I_XOR (DontSetFlags,r2,r1,r1)

    let emit_access_dep_addr st p init e  r1 =
      let r2,st = next_reg st in
      let c =  calc0 r2 r1 in
      match Misc.as_some e.dir,e.atom,e.loc with
      | R,None,Data loc ->
          let r,init,cs,st = emit_load_idx st p init loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Reserve,Data loc ->
          let r,init,cs,st = emit_ldrex_idx st p init loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Atomic,Data loc ->
          let r,init,cs,st = emit_lda_idx st p init loc r2 in
          Some r,init, Instruction c::cs,st
      | W,None,Data loc ->
          let init,cs,st = emit_store_idx st p init loc r2 e.v in
          None,init,Instruction c::cs,st
      | W,Some Reserve,Data _ -> Warn.fatal "No store with reservation"
      | W,Some Atomic,Data loc ->
          let ro,init,cs,st = emit_sta_idx st p init loc r2 e.v in
          ro,init,Instruction c::cs,st
      | _,Some (Mixed _),Data _ -> assert false
      | Code.J,_,Data _ -> emit_joker st init
      | _,_,Code _ -> Warn.fatal "No code location for arch ARM"

    let emit_exch_dep_addr st p init er ew rd =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      let c =
        [Instruction (calc0 tempo1 rd);
         Instruction (I_ADD3 (DontSetFlags,tempo1,rA,tempo1));] in
      let r,init,csr,st = emit_ldrex_reg st p init tempo1 in
      let init,csw,st = emit_one_strex_reg st p init tempo1 ew.v in
      r,init,c@csr@csw,st


    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | None -> Warn.fatal "TODO"
      | Some R -> Warn.fatal "data dependency to load"
      | Some W ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (calc0 r2 r1) ;
             Instruction (I_ADD (DontSetFlags,r2,r2,e.v)) ; ] in
          begin match e.atom,e.loc with
          | None,Data loc ->
              let init,cs,st = emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some Atomic,Data loc ->
              let ro,init,cs,st = emit_sta_reg st p init loc r2 in
              ro,init,cs2@cs,st
          | Some Reserve,Data _ ->
              Warn.fatal "No store with reservation"
          | Some (Mixed _),Data _ -> assert false
          | _,Code _ -> Warn.fatal "No code location for arch ARM"
          end
     | Some Code.J -> assert false

    let insert_isb isb cs1 cs2 =
      if isb then cs1@[Instruction I_ISB]@cs2
      else cs1@cs2

    let emit_access_ctrl isb st p init e r1 =
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (I_CMP (r1,r1));
         Instruction (I_BNE lab);
         Label (lab,Nop);] in
      let ropt,init,cs,st = emit_access st p init e in
      ropt,init,insert_isb isb c cs,st

    let emit_exch_ctrl isb st p init er ew r1 =
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (I_CMP (r1,r1));
         Instruction (I_BNE lab);
         Label (lab,Nop);] in
      let ropt,init,cs,st = emit_exch st p init er ew in
      Misc.as_some ropt,init,insert_isb isb c cs,st


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

    let emit_rmw_dep () st p init er ew dp rd =
      let r,init,cs,st = emit_exch_dep  st p init er ew dp rd in
      Some r,init,cs,st

(* Fences *)

    let emit_fence _ _ _ f =
      [Instruction
        (match f with
        | DMB o -> I_DMB o
        | DSB o -> I_DSB o
        | ISB -> I_ISB)]
    let full_emit_fence = GenUtils.to_full emit_fence
    let stronger_fence = DMB SY

    let do_check_load p st r e =
      let lab = Label.exit p (current_label st) in
      (fun k ->
        Instruction (I_CMPI (r,e.v))::
        Instruction (I_BNE lab)::
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
        let init,okcs,st = emit_store st p init (as_data Code.ok) 0 in
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
