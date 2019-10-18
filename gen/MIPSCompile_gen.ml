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

module Make(Cfg:CompileCommon.Config) : XXXCompile_gen.S =
  struct

    module MIPS = MIPSArch_gen.Make(Cfg)
    include CompileCommon.Make(Cfg)(MIPS)


(******)
    let ppo _f k = k

    open MIPS
    open C


(* Utilities *)
    let _next_reg x = MIPS.alloc_reg x

    let li r v =
      if (v < 0 || v > 0xffff) then
        Warn.fatal "MIPS generator cannot handle constant %i\n" v ;
      OPI (OR,r,MIPS.r0,v)

    let mv r1 r2 = OP (OR,r1,MIPS.r0,r2)


    module Extra = struct
      let use_symbolic = true
      type reg = MIPS.reg
      type instruction = MIPS.pseudo
      let mov r v = Instruction (li r v)
      let mov_mixed _sz _r _v = assert false
      let mov_reg r1 r2 = Instruction (mv r1 r2)
      let mov_reg_mixed _sz _r _v = assert false
    end

    module U = GenUtils.Make(Cfg)(MIPS)(Extra)


(* RMW utilities *)
    let atom r1 r2 addr k =
      LL (r1,0,addr)::
      move tmp2 r2::
      SC (tmp2,0,addr)::k

    let emit_loop_pair _p st r1 r2 addr =
      let lab = Label.next_label "Loop" in
      Label (lab,Nop)::
      lift_code (atom r1 r2 addr [BC (EQ,tmp2,r0,lab)]),
      st

    let emit_unroll_pair u p st r1 r2 addr =
      if u <= 0 then
        lift_code (atom r1 r2 addr []),st
      else if u = 1 then
        lift_code (atom r1 r2 addr [BC (EQ,tmp2,r0,Label.fail p (current_label st));]),
      next_label_st st
      else
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
            atom r1 r2 addr [BC (EQ,tmp2,r0,Label.fail p (current_label st));]
          | u ->
              atom r1 r2 addr
                (BC (NE,tmp2,r0,out)::do_rec (u-1)) in
        lift_code (do_rec u)@[Label (out,Nop)],
        next_label_st st

    let emit_pair = match Cfg.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u

(*********)
(* loads *)
(*********)
    let do_branch cond r i lab k = match i with
    | 0 ->
        BC (cond,r,MIPS.r0,lab)::k
    | _ ->
        li MIPS.tmp3 i::
        BC (cond,r,tmp3,lab)::k


    let branch_neq r i lab k = do_branch MIPS.NE r i lab k

    let _branch_eq r i lab k = do_branch MIPS.EQ r i lab k

    let emit_load st p init x =
      let rA,st = _next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,lift_code [LW (rA,0,rB)],st

    let emit_obs = emit_load

    let emit_obs_not_zero st p init x =
      let rA,st = _next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      lift_code
        (LW (rA,0,rB)::branch_neq rA 0 lab []),
      st

    let emit_load_one st p init x =
      let rA,st = _next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      lift_code (LW (rA,0,rB)::branch_neq rA 1 lab []),
      st

    let emit_load_not st p init x bne =
      let rA,st = _next_reg st in
      let rC,st = _next_reg st in
      let rB,init,st = U.next_init st p init x in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      Instruction (li rC 200)::
      (* 200 X about 5 ins looks for a typical memory delay *)
      Label (lab,Nop)::
      lift_code
        (LW (rA,0,rB)::
         bne rA out
           (OPI (ADDU,rC,rC,-1)::branch_neq rC 0 lab []))@
      [Label (out,Nop)],
      st

    let emit_obs_not_eq st p init x rP =
      emit_load_not st p init x (fun r lab k -> BC (NE,r,rP,lab)::k)

    let emit_obs_not_value st p init x v =
      emit_load_not st p init x
        (fun r lab k -> branch_neq r v lab k)

    let emit_load_idx st p init x idx =
      let rA,st = _next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,lift_code [OP (ADDU,tmp1,idx,rB);LW (rA,0,tmp1)],st

(**********)
(* Stores *)
(**********)

    let emit_store_reg st p init x rA =
      let rB,init,st = U.next_init st p init x in
      init,[Instruction (SW (rA,0,rB))],st

    let emit_store_idx_reg st p init x idx rA =
      let rB,init,st = U.next_init st p init x in
      let cs = [OP (ADDU,tmp1,idx,rB); SW (rA,0,tmp1);] in
      init,lift_code cs,st

    let emit_store st p init x v =
      let rA,init,csv,st = U.emit_mov st p init v in
      let init,cs,st = emit_store_reg st p init x rA in
      init,csv@cs,st

    let emit_store_idx st p init x idx v =
      let rA,init,csv,st = U.emit_mov st p init v in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,csv@cs,st

(* Load exclusive *)

(* FNO *)

    let emit_ll_reg st _p init rB =
      let rA,st = _next_reg st in
      rA,init,lift_code [LL (rA,0,rB)],st

    let emit_ll st p init x =
      let rB,init,st = U.next_init st p init x in
      emit_ll_reg st p init rB

    let emit_ll_idx st p init x idx =
      let rA,st = _next_reg st in
      let rB,init,st = U.next_init st p init x in
      rA,init,
      lift_code [OP (ADDU,tmp1,idx,rB); LL (rA,0,tmp1)],st

    let do_emit_fno st p init rA =
      let rR,st = _next_reg st in
      let cs,st = emit_pair p st rR rR rA in
      rR,init,cs,st

    let emit_fno st p init x =
      let rB,init,st = U.next_init st p init x in
      do_emit_fno st p init rB

    let emit_fno_idx st p init x idx =
      let rB,init,st = U.next_init st p init x in
      let r,init,cs,st = do_emit_fno st p init tmp1 in
      r,init,
      Instruction (OP (ADDU,tmp1,idx,rB))::cs,
      st

(* STA *)

    let do_emit_sta st p init rA rW =
      let rR,st = _next_reg st in
      let cs,st = emit_pair p st rR rW rA in
      Some rR,init,cs,st

    let emit_sta_reg st p init x rW =
      let rA,init,st = U.next_init st p init x in
      do_emit_sta st p init rA rW

    let emit_sta st p init x v =
      let rW,init,csv,st = U.emit_mov st p init v in
      let ro,init,cs,st = emit_sta_reg st p init x rW in
      ro,init,csv@cs,st

    let emit_sta_idx st p init x idx v =
      let rA,init,st = U.next_init st p init x in
      let rW,init,csv,st = U.emit_mov st p init v in
      let ro,init,cs,st =  do_emit_sta st p init tmp1 rW in
      ro,
      init,
      csv@Instruction (OP (ADDU,tmp1,idx,rA))::cs,
      st

(*************)
(* Acccesses *)
(*************)

let emit_joker st init = None,init,[],st

    let emit_access  st p init e = match e.dir with
    | None -> Warn.fatal "MIPSCompile.emit_access"
    | Some d ->
        match d,e.atom,e.loc with
        | R,None,Data loc ->
            let r,init,cs,st = emit_load st p init loc in
            Some r,init,cs,st
        | R,Some Reserve,Data loc ->
            let r,init,cs,st = emit_ll st p init loc  in
            Some r,init,cs,st
        | R,Some Atomic,Data loc ->
            let r,init,cs,st = emit_fno st p init loc  in
            Some r,init,cs,st
        | W,None,Data loc ->
            let init,cs,st = emit_store st p init loc e.v in
            None,init,cs,st
        | W,Some Reserve,Data _ -> Warn.fatal "No store with reservation"
        | W,Some Atomic,Data loc ->
            let ro,init,cs,st = emit_sta st p init loc e.v in
            ro,init,cs,st
        | _,Some (Mixed _),Data _ -> assert false
        | J, _,Data _ -> emit_joker st init
        | _,_,Code _ -> Warn.fatal "No code location for arch MIPS"

    let emit_exch st p init er ew =
      let rA,init,st = U.next_init st p init (Code.as_data er.loc) in
      let rR,st = _next_reg st in
      let rW,init,csv,st = U.emit_mov st p init ew.v in
      let cs,st = emit_pair p st rR rW rA in
      rR,init,csv@cs,st

    let emit_rmw () st p init er ew  =
      let rR,init,cs,st = emit_exch st p init er ew in
      Some rR,init,cs,st

    let emit_access_dep_addr st p init e  r1 =
      let r2,st = _next_reg st in
      let c =  OP (XOR,r2,r1,r1) in
      match e.dir,e.loc  with
      | None,_ -> Warn.fatal "TODO"
      | Some d,Data loc ->
          begin match d,e.atom with
          | R,None ->
              let r,init,cs,st = emit_load_idx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some Reserve ->
              let r,init,cs,st = emit_ll_idx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | R,Some Atomic ->
              let r,init,cs,st = emit_fno_idx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | W,None ->
              let init,cs,st = emit_store_idx st p init loc r2 e.v in
              None,init,Instruction c::cs,st
          | W,Some Reserve -> Warn.fatal "No store with reservation"
          | W,Some Atomic ->
              let ro,init,cs,st = emit_sta_idx st p init loc r2 e.v in
              ro,init,Instruction c::cs,st
          | _,Some (Mixed _) -> assert false
          | J,_ -> emit_joker st init
          end
      | _,Code _ -> Warn.fatal "No code location for MIPS"

    let emit_exch_dep_addr st p init er ew rd =
      let rA,init,st = U.next_init st p init (as_data er.loc) in
      let rR,st = _next_reg st in
      let rW,init,csv,st = U.emit_mov st p init ew.v in
      let cs,st = emit_pair p st rR rW tmp1 in
      rR,init,
      csv@
      Instruction (OP (XOR,tmp1,rd,rd))::
      Instruction (OP (ADDU,tmp1,rA,tmp1))::
      cs,
      st

    let emit_access_dep_data st p init e  r1 =
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some R,_ -> Warn.fatal "data dependency to load"
      | Some W,Data loc ->
          let r2,st = _next_reg st in
          let cs2 =
            [Instruction (OP (XOR,r2,r1,r1)) ;
             Instruction (OPI (ADDU,r2,r2,e.v)) ; ] in
          begin match e.atom with
          | None ->
              let init,cs,st = emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some Atomic ->
              let ro,init,cs,st = emit_sta_reg st p init loc r2 in
              ro,init,cs2@cs,st
          | Some Reserve ->
              Warn.fatal "No store with reservation"
          | Some (Mixed _) -> assert false
          end
      | Some J,_ -> emit_joker st init
      | _,Code _ -> Warn.fatal "No code location for MIPS"

    let emit_access_ctrl st p init e r1 =
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (BC (NE,r1,r1,lab));
         Label (lab,Nop);] in
      let ropt,init,cs,st = emit_access st p init e in
      ropt,init, c@cs,st

    let emit_exch_ctrl st p init er ew rd =
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (BC (NE,rd,rd,lab));
         Label (lab,Nop);] in
      let ropt,init,cs,st = emit_exch st p init er ew in
      ropt,init, c@cs,st

    let emit_access_dep st p init e dp r1 _v1 = match dp with
    | ADDR -> emit_access_dep_addr st p init e r1
    | DATA -> emit_access_dep_data st p init e r1
    | CTRL -> emit_access_ctrl st p init e r1

    let emit_exch_dep st p init er ew dp r1 = match dp with
    | ADDR -> emit_exch_dep_addr st p init er ew  r1
    | DATA -> Warn.fatal "no data depency to RMW"
    | CTRL -> emit_exch_ctrl st p init er ew r1

    let emit_rmw_dep () st p init er ew dp rd =
      let r,init,cs,st = emit_exch_dep  st p init er ew dp rd in
      Some r,init,cs,st

(* Fences *)

    let emit_fence _ _ _ f = [Instruction (match f with Sync -> SYNC)]
    let full_emit_fence = GenUtils.to_full emit_fence
    let stronger_fence = Sync

(* Check load *)
    let do_check_load p st r e =
      let lab = Label.exit p (current_label st) in
      (fun k -> lift_code (branch_neq r e.v lab [])@k),
      next_label_st st

    let check_load  p r e init st =
      let cs,st = do_check_load p st r e in
      init,cs,st

(* Postlude *)

    let list_of_fail_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = Instruction (B (Label.exit p n))::
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
