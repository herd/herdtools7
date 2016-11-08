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

    let next_init st p init loc =
      let rec find_rec = function
        | (Reg (p0,r0),loc0)::_ when loc0 = loc && p = p0 ->
            r0,init,st
        | _::rem -> find_rec rem
        | [] ->
(* ARGL no proper symbolic regs in litmus...
   let r = Symbolic_reg (sprintf "%s%i" loc p) in
 *)
            let r,st = next_reg st in
            r,(Reg (p,r),loc)::init,st in
      find_rec init

    let next_const st p init v =
      let r,st = next_reg st in
      r,(Reg (p,r),Printf.sprintf "0x%x" v)::init,st

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

    let cbz r1 lbl = I_CBZ (vloc,r1,lbl)
    let cbnz r1 lbl = I_CBNZ (vloc,r1,lbl)
    let mov r i = I_MOV (vloc,r,i)
    let mov_mixed sz r i =
      let v =
        let open MachSize in
        match sz with
        | Byte|Short|Word -> V32
        | Quad -> V64 in
      I_MOV (v,r,i)
    let cmpi r i = I_OP3 (vloc,SUBS,ZR,r,K i)
    let cmp r1 r2 = I_OP3 (vloc,SUBS,ZR,r1,RV (vloc,r2))
    let bne lbl = I_BC (NE,lbl)
    let eor r1 r2 r3 = I_OP3 (vloc,EOR,r1,r2,RV (vloc,r3))
    let andi r1 r2 k = I_OP3 (vloc,AND,r1,r2,K k)
    let addi r1 r2 k = I_OP3 (vloc,ADD,r1,r2,K k)
(*    let add r1 r2 r3 = I_OP3 (vloc,ADD,r1,r2,r3) *)
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
    let ldxr r1 r2 = I_LDAR (vloc,XX,r1,r2)
    let ldaxr r1 r2 = I_LDAR (vloc,AX,r1,r2)
    let sxtw r1 r2 = I_SXTW (r1,r2)
    let ldr_idx r1 r2 idx = I_LDR (vloc,r1,r2,RV (vloc,idx))

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

(* Compute address in tempo1 *)
    let _sxtw r k = match vloc with
    | V64 -> k
    | V32 -> sxtw r r::k

    let sum_addr st rA idx =
      let r,st = tempo1 st in
      r,[add64 r rA idx],st

(************)
(* loads    *)
(************)

    module type L = sig
      val load : reg -> reg -> instruction
      val load_idx : A.st -> reg -> reg -> reg -> instruction list * A.st
    end

    let emit_load_mixed sz o st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [ldr_mixed rA rB sz o],st

    module LOAD(L:L) =
      struct

        let emit_load st p init x =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          rA,init,pseudo [L.load rA rB],st


        let emit_load_not_zero st p init x =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          pseudo
            [L.load rA rB; cbz rA lab],
          st

        let emit_load_one st p init x =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          pseudo [L.load rA rB; cmpi rA 1; bne lab],
          st

        let emit_load_not st p init x cmp =
          let rA,st = next_reg st in
          let rC,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let lab = Label.next_label "L" in
          let out = Label.next_label "L" in
          rA,init,
          Instruction (mov rC 200)::
          (* 200 X about 5 ins looks for a typical memory delay *)
          Label (lab,Nop)::
          pseudo
            [
             L.load rA rB; cmp rA ;
             bne out; I_OP3 (vloc,SUBS,rC,rC,K 1) ;
             cbnz rC lab ;
           ]@
          [Label (out,Nop)],
          st

        let emit_load_not_eq st p init x rP =
          emit_load_not st p init x (fun r -> cmp r rP)

        let emit_load_not_value st p init x v =
          emit_load_not st p init x (fun r -> cmpi r v)

        let emit_load_idx st p init x idx =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let ins,st = L.load_idx st rA rB idx in
          rA,init,pseudo ins ,st

      end

    module LDR =
      LOAD
        (struct
          let load = ldr
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
          let load = ldar
          let load_idx st rA rB idx =
            let r,ins,st = sum_addr st rB idx in
            ins@[ldar rA r],st
        end)

(**********)
(* Stores *)
(**********)

    module type S = sig
      val store : reg -> reg -> instruction
      val store_idx : A.st -> reg -> reg -> reg -> instruction list * A.st
    end

    let emit_const st p init v =
      if 0 <= v && v < 0xffff then
        None,init,st
      else
        let rA,init,st = next_const st p init v in
        Some rA,init,st

    let emit_mov sz st p init v = match emit_const st p init v with
    | None,init,st ->
        let rA,st = next_reg st in
        rA,init,[Instruction (mov_mixed sz rA v)],st
    | Some rA,init,st ->
        rA,init,[],st

    let emit_store_reg_mixed sz o st p init x rA =
      let rB,init,st = next_init st p init x in
      init,[Instruction (str_mixed sz o rA rB)],st

    let emit_store_mixed sz o st p init x v =
      let rA,init,csi,st = emit_mov sz st p init v in
      let init,cs,st = emit_store_reg_mixed sz o st p init x rA in
      init,csi@cs,st

    module STORE(S:S) =
      struct

        let emit_store_reg st p init x rA =
          let rB,init,st = next_init st p init x in
          init,[Instruction (S.store rA rB)],st

        let emit_store st p init x v =
          let rA,st = next_reg st in
          let init,cs,st = emit_store_reg st p init x rA in
          init,Instruction (mov rA v)::cs,st

        let emit_store_idx_reg st p init x idx rA =
          let rB,init,st = next_init st p init x in
          let ins,st = S.store_idx st rA rB idx in
          init,pseudo ins,st

        let emit_store_idx st p init x idx v =
          let rA,st = next_reg st in
          let init,cs,st = emit_store_idx_reg st p init x idx rA in
          init,Instruction (mov rA v)::cs,st

      end

    module STR =
      STORE
        (struct
          let store = str
          let store_idx st rA rB idx = [str_idx rA rB idx],st
        end)

    module STLR =
      STORE
        (struct
          let store = stlr
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
      let r,st = tempo3 st in      
      let cs =
        if u <= 0 then
          pseudo
            [get_xload rw rR rA;
             get_xstore rw r rW rA;]
        else if u = 1 then
          emit_one_pair rw p r rR rW rA []
        else
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
          do_rec u in
      cs,st

    let emit_pair = match Cfg.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u

    let emit_lda_reg rw st p rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair rw p st rR rR rA in
      rR,cs,st

    let emit_lda rw st p init loc =
      let rA,init,st = next_init st p init loc in
      let r,cs,st =  emit_lda_reg rw st p rA in
      r,init,cs,st

    let emit_lda_idx rw st p init loc idx =
      let rA,init,st = next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let r,cs2,st =  emit_lda_reg rw st p rA in
      r,init,pseudo cs1@cs2,st


    let do_emit_sta rw st p rW rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair rw p st rR rW rA in
      rR,cs,st

    let emit_sta rw st p init loc v =
      let rA,init,st = next_init st p init loc in
      let rW,st = next_reg st in
      let rR,cs,st = do_emit_sta rw st p rW rA in
      rR,init,Instruction (mov rW v)::cs,st

    let emit_sta_reg rw st p init loc rW =
      let rA,init,st = next_init st p init loc in
      let rR,cs,st = do_emit_sta rw st p rW rA in
      rR,init,cs,st

    let emit_sta_idx rw st p init loc idx v =
      let rA,init,st = next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let rW,st = next_reg st in
      let rR,cs2,st = do_emit_sta rw st p rW rA in
      rR,init,Instruction (mov rW v)::pseudo cs1@cs2,st


(**********)
(* Access *)
(**********)

    let emit_access  st p init e = match e.dir,e.atom with
    | R,None ->
        let r,init,cs,st = LDR.emit_load st p init e.loc in
        Some r,init,cs,st
    | R,Some Acq ->
        let r,init,cs,st = LDAR.emit_load st p init e.loc  in
        Some r,init,cs,st
    | R,Some Rel ->
        Warn.fatal "No load release"
    | R,Some Atomic rw ->
        let r,init,cs,st = emit_lda rw st p init e.loc  in
        Some r,init,cs,st
    | R,Some (Mixed (sz,o)) ->        
        let r,init,cs,st = emit_load_mixed sz o st p init e.loc in
        Some r,init,cs,st
    | W,None ->
        let init,cs,st = STR.emit_store st p init e.loc e.v in
        None,init,cs,st
    | W,Some Rel ->
        let init,cs,st = STLR.emit_store st p init e.loc e.v in
        None,init,cs,st
    | W,Some Acq -> Warn.fatal "No store acquire"
    | W,Some Atomic rw ->
        let r,init,cs,st = emit_sta rw st p init e.loc e.v in
        Some r,init,cs,st
    | W,Some (Mixed (sz,o)) ->
        let init,cs,st = emit_store_mixed sz o st p init e.loc e.v in
        None,init,cs,st

    let tr_a ar aw = match ar,aw with
    | None,None -> PP
    | Some Acq,None -> AP
    | None,Some Rel -> PL
    | Some Acq,Some Rel -> AL
    | _,_ ->
        Warn.fatal
          "bad atomicity in rmw, %s%s"
          (E.pp_atom_option ar)
          (E.pp_atom_option aw)

    let emit_exch st p init er ew =
      let rA,init,st = next_init st p init er.loc in
      let rR,st = next_reg st in
      let rW,st = next_reg st in
      let arw = tr_a er.C.atom ew.C.atom in
      let cs,st = emit_pair arw p st rR rW rA in
      rR,init,Instruction (mov rW ew.v)::cs,st

(* Fences *)

    let emit_fence f =  Instruction (I_FENCE f)

    let stronger_fence = strong


(* Dependencies *)
    let calc0 =
      if Cfg.realdep then
        fun dst src ->  andi dst src 128 
      else
        fun dst src -> eor dst src src 

    let emit_access_dep_addr st p init e  rd =
      let r2,st = next_reg st in
      let c =  calc0 r2 rd in
      match e.dir,e.atom with
      | R,None ->
          let r,init,cs,st = LDR.emit_load_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Acq ->
          let r,init,cs,st = LDAR.emit_load_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some Rel ->
          Warn.fatal "No load release"
      | R,Some Atomic rw ->
          let r,init,cs,st = emit_lda_idx rw st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | W,None ->
          let init,cs,st = STR.emit_store_idx st p init e.loc r2 e.v in
          None,init,Instruction c::cs,st
      | W,Some Rel ->
          let init,cs,st = STLR.emit_store_idx st p init e.loc r2 e.v in
          None,init,Instruction c::cs,st
      | W,Some Acq -> Warn.fatal "No store acquire"
      | W,Some Atomic rw ->
          let r,init,cs,st = emit_sta_idx rw st p init e.loc r2 e.v in
          Some r,init,Instruction c::cs,st
      | _,Some (Mixed _) ->
          Warn.fatal "addr dep with mixed"


    let emit_exch_dep_addr st p init er ew rd =
      let r2,st = next_reg st in
      let c = calc0 r2 rd in
      let rA,init,st = next_init st p init er.loc in
      let rA,csum,st = sum_addr st rA r2 in
      let rR,st = next_reg st in
      let rW,st = next_reg st in
      let arw = tr_a er.C.atom ew.C.atom in
      let cs,st = emit_pair arw p st rR rW rA in
      rR,init,
      Instruction (mov rW ew.v)::pseudo (c::csum)@cs,
      st

    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R -> Warn.fatal "data dependency to load"
      | W ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (calc0 r2 r1) ;
             Instruction (addi r2 r2 e.v) ; ] in
          begin match e.atom with
          | None ->
              let init,cs,st = STR.emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some Rel ->
              let init,cs,st = STLR.emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some Atomic rw ->
              let r,init,cs,st = emit_sta_reg rw st p init e.loc r2 in
              Some r,init,cs2@cs,st        
          | Some Acq ->
              Warn.fatal "No store acquire"
          | Some (Mixed _) ->
              Warn.fatal "data dep with mixed"
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


    let check_load p r e =
      let lab = Label.exit p in
      (fun k ->
        Instruction (cmpi r e.v)::
        Instruction (bne lab)::
        k)

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
  end
