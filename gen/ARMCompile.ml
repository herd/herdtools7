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

    module ARM = ARMArch

    include CompileCommon.Make(Cfg)(ARM)


(******)
    let ppo _f k = k

    open ARM
    open C


(* Utilities *)
    let next_reg x = ARM.alloc_reg x

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
    let tempo1 = ARM.Symbolic_reg "T1" (* May be used for address *)
    let tempo2 = ARM.Symbolic_reg "T2" (* utility *)


    let emit_loop_pair _p r1 r2 addr =
      let lab = Label.next_label "Loop" in
      Label (lab,Nop)::
      pseudo
        [I_LDREX (r1,addr) ;
         I_STREX (tempo2,r2,addr,AL);
         I_CMPI (tempo2,0);
         I_BNE (lab);
       ]

    let emit_unroll_pair u p r1 r2 addr =
      if u <= 0 then
        pseudo
          [I_LDREX (r1,addr);
           I_STREX (tempo2,r2,addr,AL);]
      else if u = 1 then
        pseudo
          [I_LDREX (r1,addr);
           I_STREX (tempo2,r2,addr,AL);
           I_CMPI (tempo2,0);
           I_BNE (Label.fail p);]
      else
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              [I_LDREX (r1,addr);
               I_STREX (tempo2,r2,addr,AL);
               I_CMPI (tempo2,0);
               I_BNE (Label.fail p);]
          | u ->
              I_LDREX (r1,addr)::
              I_STREX (tempo2,r2,addr,AL)::
              I_CMPI (tempo2,0)::
              I_BEQ (out)::
              do_rec (u-1) in
        pseudo (do_rec u)@[Label (out,Nop)]
        
    let emit_pair = match Cfg.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u


(*********)
(* loads *)
(*********)

    let emit_load st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [I_LDR (rA,rB,AL)],st

    let emit_load_not_zero st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::pseudo [I_LDR (rA,rB,AL); I_CMPI (rA,0); I_BEQ (lab)],
      st

    let emit_load_one st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::pseudo [I_LDR (rA,rB,AL); I_CMPI (rA,1); I_BNE (lab)],
      st

    let emit_load_not st p init x cmp =
      let rA,st = next_reg st in
      let rC,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      Instruction (I_MOVI (rC,200,AL))::
      (* 200 X about 5 ins looks for a typical memory delay *)
      Label (lab,Nop)::
      pseudo
        [
         I_LDR (rA,rB,AL); cmp rA ;
         I_BNE out; I_ADD (DontSetFlags,rC,rC,-1) ;
         I_CMPI (rC,0) ; I_BNE lab ;
       ]@
      [Label (out,Nop)],
      st
        
    let emit_load_not_eq st p init x rP =
      emit_load_not st p init x (fun r -> I_CMP (r,rP))

    let emit_load_not_value st p init x v =
      emit_load_not st p init x (fun r -> I_CMPI (r,v))
        
    let emit_load_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [I_LDR3 (rA,idx,rB,AL)],st

(**********)
(* Stores *)
(**********)

    let emit_store_reg st p init x rA =
      let rB,init,st = next_init st p init x in
      init,[Instruction (I_STR (rA,rB,AL))],st

    let emit_store_idx_reg st p init x idx rA =
      let rB,init,st = next_init st p init x in
      init,[Instruction (I_STR3 (rA,idx,rB,AL))],st

    let emit_store st p init x v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_reg st p init x rA in
      init,Instruction (I_MOVI (rA,v,AL))::cs,st

    let emit_store_idx st p init x idx v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,Instruction (I_MOVI (rA,v,AL))::cs,st

    let emit_one_strex_reg st p init rA v =
      let rV,st = next_reg st in
      init,
      pseudo
        [I_MOVI (rV,v,AL);
         I_STREX (tempo2,rV,rA,AL);
         I_CMPI (tempo2,0);
         I_BNE (Label.fail p);],
      st


    let emit_ldrex_reg st _p init rB =
      let rA,st = next_reg st in
      rA,init,pseudo [I_LDREX (rA,rB)],st

    let emit_ldrex st p init x =
      let rB,init,st = next_init st p init x in
      emit_ldrex_reg st p init rB

    let emit_ldrex_idx st p init x idx =      
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,
      pseudo [I_ADD3 (DontSetFlags,tempo1,idx,rB); I_LDREX (rA,tempo1)],st


(* LDA *)
    let emit_lda st p init x =
      let rA,init,st = next_init st p init x in
      let rR,st = next_reg st in
      let cs = emit_pair p rR rR rA in
      rR,init,cs,st

    let emit_lda_idx st p init x idx =
      let rA,init,st = next_init st p init x in
      let rR,st = next_reg st in
      let cs = emit_pair p rR rR tempo1 in
      rR,init,
      Instruction (I_ADD3 (DontSetFlags,tempo1,idx,rA))::cs,
      st

(* STA *)

    let emit_sta_reg st p init x rW =
      let rA,init,st = next_init st p init x in
      let rR,st = next_reg st in
      let cs = emit_pair p rR rW rA in
      Some rR,init,cs,st

    let emit_sta st p init x v =
      let rW,st = next_reg st in
      let ro,init,cs,st = emit_sta_reg st p init x rW in
      ro,init,
      Instruction (I_MOVI (rW,v,AL))::cs,
      st

    let emit_sta_idx st p init x idx v =
      let rA,init,st = next_init st p init x in
      let rW,st = next_reg st in
      let rR,st = next_reg st in
      let cs = emit_pair p rR rW tempo1 in
      Some rR,
      init,
      Instruction (I_MOVI (rW,v,AL))::
      Instruction (I_ADD3 (DontSetFlags,tempo1,idx,rA))::cs,
      st


(*************)
(* Acccesses *)
(*************)

    let emit_access  st p init e = match e.dir,e.atom with
    | R,None ->
        let r,init,cs,st = emit_load st p init e.loc in
        Some r,init,cs,st
    | R,Some Reserve ->
        let r,init,cs,st = emit_ldrex st p init e.loc  in
        Some r,init,cs,st
    | R,Some Atomic ->
        let r,init,cs,st = emit_lda st p init e.loc  in
        Some r,init,cs,st
    | W,None ->
        let init,cs,st = emit_store st p init e.loc e.v in
        None,init,cs,st
    | W,Some Reserve -> Warn.fatal "No store with reservation"
    | W,Some Atomic ->
        let ro,init,cs,st = emit_sta st p init e.loc e.v in
        ro,init,cs,st
    | _,Some (Mixed _) -> assert false

    let emit_exch st p init er ew =
      let rA,init,st = next_init st p init er.loc in
      let rW,st = next_reg st in
      let rR,st = next_reg st in
      let cs = emit_pair p rR rW rA in
      rR,init,
      Instruction (I_MOVI (rW,ew.v,AL))::cs,
      st

    let emit_access_dep_addr st p init e  r1 =
      let r2,st = next_reg st in
      let c =  I_XOR (DontSetFlags,r2,r1,r1) in
      match e.dir,e.atom with
      | R,None ->
          let r,init,cs,st = emit_load_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Reserve ->
          let r,init,cs,st = emit_ldrex_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Atomic ->
          let r,init,cs,st = emit_lda_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | W,None ->
          let init,cs,st = emit_store_idx st p init e.loc r2 e.v in
          None,init,Instruction c::cs,st
      | W,Some Reserve -> Warn.fatal "No store with reservation"
      | W,Some Atomic ->
          let ro,init,cs,st = emit_sta_idx st p init e.loc r2 e.v in
          ro,init,Instruction c::cs,st
      | _,Some (Mixed _) -> assert false

    let emit_exch_dep_addr st p init er ew rd =
      let rA,init,st = next_init st p init er.loc in
      let c =
        [Instruction (I_XOR (DontSetFlags,tempo1,rd,rd));
         Instruction (I_ADD3 (DontSetFlags,tempo1,rA,tempo1));] in
      let r,init,csr,st = emit_ldrex_reg st p init tempo1 in
      let init,csw,st = emit_one_strex_reg st p init tempo1 ew.v in
      r,init,c@csr@csw,st


    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R -> Warn.fatal "data dependency to load"
      | W ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (I_XOR (DontSetFlags,r2,r1,r1)) ;
             Instruction (I_ADD (DontSetFlags,r2,r2,e.v)) ; ] in
          begin match e.atom with
          | None ->
              let init,cs,st = emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some Atomic ->
              let ro,init,cs,st = emit_sta_reg st p init e.loc r2 in
              ro,init,cs2@cs,st        
          | Some Reserve ->
               Warn.fatal "No store with reservation"
          | Some (Mixed _) -> assert false
          end

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
      ropt,init,insert_isb isb c cs,st


    let emit_access_dep st p init e dp r1 = match dp with
    | ADDR -> emit_access_dep_addr st p init e r1
    | DATA -> emit_access_dep_data st p init e r1
    | CTRL -> emit_access_ctrl false st p init e r1
    | CTRLISYNC -> emit_access_ctrl true st p init e r1

    let emit_exch_dep  st p init er ew dp rd = match dp with
    | ADDR -> emit_exch_dep_addr   st p init er ew rd
    | DATA -> Warn.fatal "not data dependency to RMW"
    | CTRL -> emit_exch_ctrl false st p init er ew rd
    | CTRLISYNC -> emit_exch_ctrl true st p init er ew rd


(* Fences *)

    let emit_fence f =
      Instruction
        (match f with
        | DMB o -> I_DMB o
        | DSB o -> I_DSB o
        | ISB -> I_ISB)

    let stronger_fence = DMB SY

    let check_load p r e =
      let lab = Label.exit p in
      (fun k ->
        Instruction (I_CMPI (r,e.v))::
        Instruction (I_BNE lab)::
        k)
(* Postlude *)

    let does_jump lab cs =
      List.exists
        (fun i -> match i with
        | Instruction (I_B lab0|I_BNE lab0|I_BEQ lab0) ->
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
        Instruction (I_B (Label.exit p))::
        Label (Label.fail p,Nop)::
        okcs@
        [Label (Label.exit p,Nop)],
        st
      else if does_exit p cs then
         init,cs@[Label (Label.exit p,Nop)],st
      else init,cs,st

  end
