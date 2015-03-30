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

module Make(Cfg:CompileCommon.Config) : XXXCompile.S =
  struct

    module MIPS = MIPSArch

    include CompileCommon.Make(Cfg)(MIPS)


(******)
    let ppo _f k = k

    open MIPS
    open C


(* Utilities *)
    let next_reg x = MIPS.alloc_reg x

    let next_init st p init loc =
      let rec find_rec = function
        | (Reg (p0,r0),loc0)::_ when loc0 = loc && p = p0 ->
            r0,init,st
        | _::rem -> find_rec rem
        | [] ->
            let r = Symbolic_reg (sprintf "%s%i" loc p) in
            r,(Reg (p,r),loc)::init,st in
      find_rec init

    let pseudo = List.map (fun i -> Instruction i)


(* RMW utilities *)
    let atom r1 r2 addr k =
      LL (r1,0,addr)::
      move tmp2 r2::
      SC (tmp2,0,addr)::k

    let emit_loop_pair _p r1 r2 addr =
      let lab = Label.next_label "Loop" in
      Label (lab,Nop)::
      pseudo (atom r1 r2 addr [BC (EQ,tmp2,r0,lab)])


    let emit_unroll_pair u p r1 r2 addr =
      if u <= 0 then
        pseudo (atom r1 r2 addr [])
      else if u = 1 then
        pseudo (atom r1 r2 addr [BC (EQ,tmp2,r0,Label.fail p);])
      else
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 -> atom r1 r2 addr [BC (EQ,tmp2,r0,Label.fail p);]
          | u ->
              atom r1 r2 addr
                (BC (NE,tmp2,r0,out)::do_rec (u-1)) in
        pseudo (do_rec u)@[Label (out,Nop)]

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
        LI (MIPS.tmp3,i)::
        BC (cond,r,tmp3,lab)::k


    let branch_neq r i lab k = do_branch MIPS.NE r i lab k

    let _branch_eq r i lab k = do_branch MIPS.EQ r i lab k

    let emit_load st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [LW (rA,0,rB)],st

    let emit_load_not_zero st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      pseudo
        (LW (rA,0,rB)::branch_neq rA 0 lab []),
      st

    let emit_load_one st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      pseudo (LW (rA,0,rB)::branch_neq rA 1 lab []),
      st

    let emit_load_not st p init x bne =
      let rA,st = next_reg st in
      let rC,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      Instruction (LI (rC,200))::
      (* 200 X about 5 ins looks for a typical memory delay *)
      Label (lab,Nop)::
      pseudo
        (LW (rA,0,rB)::
         bne rA out
           (OPI (ADDU,rC,rC,-1)::branch_neq rC 0 lab []))@
      [Label (out,Nop)],
      st
        
    let emit_load_not_eq st p init x rP =
      emit_load_not st p init x (fun r lab k -> BC (NE,r,rP,lab)::k)

    let emit_load_not_value st p init x v =
      emit_load_not st p init x
        (fun r lab k -> branch_neq r v lab k)
        
    let emit_load_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [OP (ADDU,tmp1,idx,rB);LW (rA,0,tmp1)],st

(**********)
(* Stores *)
(**********)

    let emit_store_reg st p init x rA =
      let rB,init,st = next_init st p init x in
      init,[Instruction (SW (rA,0,rB))],st

    let emit_store_idx_reg st p init x idx rA =
      let rB,init,st = next_init st p init x in
      let cs = [OP (ADDU,tmp1,idx,rB); SW (rA,0,tmp1);] in
      init,pseudo cs,st

    let emit_store st p init x v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_reg st p init x rA in
      init,Instruction (LI (rA,v))::cs,st

    let emit_store_idx st p init x idx v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,Instruction (LI (rA,v))::cs,st

(* Load exclusive *)

(* FNO *)

    let emit_ll_reg st _p init rB =
      let rA,st = next_reg st in
      rA,init,pseudo [LL (rA,0,rB)],st

    let emit_ll st p init x =
      let rB,init,st = next_init st p init x in
      emit_ll_reg st p init rB

    let emit_ll_idx st p init x idx =      
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,
      pseudo [OP (ADDU,tmp1,idx,rB); LL (rA,0,tmp1)],st

    let do_emit_fno st p init rA =
      let rR,st = next_reg st in
      let cs = emit_pair p rR rR rA in
      rR,init,cs,st

    let emit_fno st p init x =
      let rB,init,st = next_init st p init x in
      do_emit_fno st p init rB

    let emit_fno_idx st p init x idx =
      let rB,init,st = next_init st p init x in
      let r,init,cs,st = do_emit_fno st p init tmp1 in
      r,init,
      Instruction (OP (ADDU,tmp1,idx,rB))::cs,
      st
        
(* STA *)

    let do_emit_sta st p init rA rW =
      let rR,st = next_reg st in
      let cs = emit_pair p rR rW rA in
      Some rR,init,cs,st

    let emit_sta_reg st p init x rW =
      let rA,init,st = next_init st p init x in
      do_emit_sta st p init rA rW

    let emit_sta st p init x v =
      let rW,st = next_reg st in
      let ro,init,cs,st = emit_sta_reg st p init x rW in
      ro,init,
      Instruction (LI (rW,v))::cs,
      st

    let emit_sta_idx st p init x idx v =
      let rA,init,st = next_init st p init x in
      let rW,st = next_reg st in
      let ro,init,cs,st =  do_emit_sta st p init tmp1 rW in
      ro,
      init,
      Instruction (LI (rW,v))::
      Instruction (OP (ADDU,tmp1,idx,rA))::cs,
      st

(*************)
(* Acccesses *)
(*************)

    let emit_access  st p init e = match e.dir,e.atom with
    | R,None ->
        let r,init,cs,st = emit_load st p init e.loc in
        Some r,init,cs,st
    | R,Some Reserve ->
        let r,init,cs,st = emit_ll st p init e.loc  in
        Some r,init,cs,st
    | R,Some Atomic ->
        let r,init,cs,st = emit_fno st p init e.loc  in
        Some r,init,cs,st
    | W,None ->
        let init,cs,st = emit_store st p init e.loc e.v in
        None,init,cs,st
    | W,Some Reserve -> Warn.fatal "No store with reservation"
    | W,Some Atomic ->
        let ro,init,cs,st = emit_sta st p init e.loc e.v in
        ro,init,cs,st

    let emit_exch st p init er ew =
      let rA,init,st = next_init st p init er.loc in
      let rR,st = next_reg st in
      let rW,st = next_reg st in
      let cs = emit_pair p rR rW rA in
      rR,init,
      Instruction (LI (rW,ew.v))::cs,
      st

    let emit_access_dep_addr st p init e  r1 =
      let r2,st = next_reg st in
      let c =  OP (XOR,r2,r1,r1) in
      match e.dir,e.atom with
      | R,None ->
          let r,init,cs,st = emit_load_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Reserve ->
          let r,init,cs,st = emit_ll_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Atomic ->
          let r,init,cs,st = emit_fno_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | W,None ->
          let init,cs,st = emit_store_idx st p init e.loc r2 e.v in
          None,init,Instruction c::cs,st
      | W,Some Reserve -> Warn.fatal "No store with reservation"
      | W,Some Atomic ->
          let ro,init,cs,st = emit_sta_idx st p init e.loc r2 e.v in
          ro,init,Instruction c::cs,st


    let emit_exch_dep_addr st p init er ew rd =
      let rA,init,st = next_init st p init er.loc in
      let rR,st = next_reg st in
      let rW,st = next_reg st in
      let cs = emit_pair p rR rW tmp1 in
      rR,init,
      Instruction (LI (rW,ew.v))::
      Instruction (OP (XOR,tmp1,rd,rd))::
      Instruction (OP (ADDU,tmp1,rA,tmp1))::
      cs,
      st

    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R -> Warn.fatal "data dependency to load"
      | W ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (OP (XOR,r2,r1,r1)) ;
             Instruction (OPI (ADDU,r2,r2,e.v)) ; ] in
          begin match e.atom with
          | None ->
              let init,cs,st = emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some Atomic ->
              let ro,init,cs,st = emit_sta_reg st p init e.loc r2 in
              ro,init,cs2@cs,st        
          | Some Reserve ->
              Warn.fatal "No store with reservation"
          end

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

    let emit_access_dep st p init e dp r1 = match dp with
    | ADDR -> emit_access_dep_addr st p init e r1
    | DATA -> emit_access_dep_data st p init e r1
    | CTRL -> emit_access_ctrl st p init e r1

    let emit_exch_dep st p init er ew dp r1 = match dp with
    | ADDR -> emit_exch_dep_addr st p init er ew  r1
    | DATA -> Warn.fatal "no data depency to RMW"
    | CTRL -> emit_exch_ctrl st p init er ew r1


(* Fences *)

    let emit_fence f =
      Instruction
        (match f with
        | Sync -> SYNC)

    let stronger_fence = Sync

(* Check load *)
    let check_load p r e =
      let lab = Label.exit p in
      fun k -> pseudo (branch_neq r e.v lab [])@k

(* Postlude *)

    let does_jump lab cs =
      List.exists
        (fun i -> match i with
        | Instruction (B lab0|BC (_,_,_,lab0)|BCZ (_,_,lab0)) ->
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
        Instruction (B (Label.exit p))::
        Label (Label.fail p,Nop)::
        okcs@
        [Label (Label.exit p,Nop)],
        st
      else if does_exit p cs then
        init,cs@[Label (Label.exit p,Nop)],st
      else init,cs,st

  end
