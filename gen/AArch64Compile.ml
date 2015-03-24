(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Code

module Make(Cfg:CompileCommon.Config) : XXXCompile.S =
  struct

(* Common *)
    module A64 = AArch64Arch
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

    let pseudo = List.map (fun i -> Instruction i)

let tempo1 st = A.alloc_trashed_reg "T1" st (* May be used for address *)
let tempo2 st = A.alloc_trashed_reg "T2" st (* May be used for data *)
let tempo3 st = A.alloc_trashed_reg "T3" st (* May be used for STRX flag *)

(******************)
(* Idiosyncrasies *)
(******************)

    let vloc = V32

    let cbz r1 lbl = I_CBZ (vloc,r1,lbl)
    let cbnz r1 lbl = I_CBNZ (vloc,r1,lbl)
    let mov r i = I_MOV (vloc,r,i)
    let cmpi r i = I_OP3 (vloc,SUBS,ZR,r,K i)
    let cmp r1 r2 = I_OP3 (vloc,SUBS,ZR,r1,RV (vloc,r2))
    let eor r1 r2 r3 = I_OP3 (vloc,EOR,r1,r2,RV (vloc,r3))
    let addi r1 r2 k = I_OP3 (vloc,ADD,r1,r2,K k)
(*    let add r1 r2 r3 = I_OP3 (vloc,ADD,r1,r2,r3) *)
    let add64 r1 r2 r3 = I_OP3 (V64,ADD,r1,r2,RV (vloc,r3))

    let ldr r1 r2 = I_LDR (vloc,r1,r2,K 0)
    let ldar r1 r2 = I_LDAR (vloc,AA,r1,r2)
    let ldxr r1 r2 = I_LDAR (vloc,XX,r1,r2)
    let ldaxr r1 r2 = I_LDAR (vloc,AX,r1,r2)
    let sxtw r1 r2 = I_SXTW (r1,r2)
    let ldr_idx r1 r2 idx = I_LDR (vloc,r1,r2,RV (vloc,idx))

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
      val load_x : reg -> reg -> instruction
    end

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
          pseudo [L.load rA rB; cmpi rA 1; I_BC (NE,lab)],
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
             I_BC (NE,out); I_OP3 (vloc,SUBS,rC,rC,K 1) ;
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

(* Exclusive *)
        let emit_ldrex_reg st _p init rB =
          let rA,st = next_reg st in
          rA,init,pseudo [L.load_x rA rB],st

        let emit_ldrex st p init x =
          let rB,init,st = next_init st p init x in
          emit_ldrex_reg st p init rB

      end

    module LDR =
      LOAD
        (struct
          let load = ldr
          let load_idx st rA rB idx = [ldr_idx rA rB idx],st
          let load_x = ldxr
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
          let load_x = ldaxr
        end)

(**********)
(* Stores *)
(**********)

    module type S = sig
      val store : reg -> reg -> instruction
      val store_idx : A.st -> reg -> reg -> reg -> instruction list * A.st
      val strex : reg -> reg -> reg -> instruction
    end

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
(* Exclusive *)
        let emit_one_strex_reg  st p init rA v =
          let rV,st = next_reg st in
          let t2,st = tempo2 st in
          init,
          pseudo
            [mov rV v ;
             S.strex t2 rV rA;
             cbnz t2 (Label.fail p);],
          st

        let emit_one_strex st p init x v =
          let rA,init,st = next_init st p init x in
          emit_one_strex_reg  st p init rA v
      end

    module STR =
      STORE
        (struct
          let store = str
          let store_idx st rA rB idx = [str_idx rA rB idx],st
          let strex = stxr
        end)

    module STLR =
      STORE
        (struct
          let store = stlr
          let store_idx  st rA rB idx =
            let r,ins,st = sum_addr st rB idx in
            ins@[stlr rA r],st
          let strex = stlxr
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


(* No FNO yet *)
    let emit_fno _st _p _init _x = assert false
    let emit_fno2 _st _p _init _x = assert false
    and emit_open_fno _st _p _init _x = assert false
    and emit_close_fno _st _p _init _lab _r _x = assert false


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

    let emit_exch st p init er ew =
      let emit_load = match er.C.atom with
      | Some Acq -> LDAR.emit_ldrex
      | None -> LDR.emit_ldrex
      | a -> Warn.fatal "bad R atomicity in rmw, %s" (E.pp_atom_option a)
      and emit_store = match ew.C.atom with
      | Some Rel -> STLR.emit_one_strex
      | None -> STR.emit_one_strex
      | a -> Warn.fatal "bad W atomicity in rmw, %s" (E.pp_atom_option a) in
      let r,init,csr,st = emit_load st p init er.loc  in
      let init,csw,st = emit_store st p init ew.loc ew.v in
      r,init,csr@csw,st

(* Fences *)

    let emit_fence f =  Instruction (I_FENCE f)

    let stronger_fence = strong


(* Dependencies *)

    let emit_access_dep_addr st p init e  rd =
      let r2,st = next_reg st in
      let c =  eor r2 rd rd in
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

    let emit_exch_dep_addr st p init er ew rd =
      let r2,st = next_reg st in
      let c = eor r2 rd rd in
      let rA,init,st = next_init st p init er.loc in
      let rA,csum,st = sum_addr st rA r2 in
      let emit_load = match er.C.atom with
      | Some Acq -> LDAR. emit_ldrex_reg
      | None -> LDR.emit_ldrex_reg
      | a -> Warn.fatal "bad R atomicity in rmw, %s" (E.pp_atom_option a)
      and emit_store = match ew.C.atom with
      | Some Rel -> STLR.emit_one_strex_reg
      | None -> STR.emit_one_strex_reg
      | a -> Warn.fatal "bad W atomicity in rmw, %s" (E.pp_atom_option a) in
      let r,init,csr,st = emit_load st p init rA  in
      let init,csw,st = emit_store st p init rA ew.v in
      r,init,pseudo (c::csum)@csr@csw,st



    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R -> Warn.fatal "data dependency to load"
      | W ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (eor r2 r1 r1) ;
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


    let emit_access_dep st p init e dp r1 = match dp with
    | ADDR -> emit_access_dep_addr st p init e r1
    | DATA -> emit_access_dep_data st p init e r1
    | CTRL -> emit_access_ctrl false st p init e r1
    | CTRLISYNC -> emit_access_ctrl true st p init e r1

    let emit_exch_dep st p init er ew dp rd = match dp with
    | ADDR -> emit_exch_dep_addr   st p init er ew rd
    | DATA -> Warn.fatal "not data dependency to RMW"
    | CTRL -> emit_exch_ctrl false st p init er ew rd
    | CTRLISYNC -> emit_exch_ctrl true st p init er ew rd

(* Postlude *)
    let does_fail p cs =
      let lab = Label.fail p in
      List.exists
        (fun i -> match i with
        | Instruction (I_B lab0|I_BC (_,lab0)
        | I_CBZ (_,_,lab0)|I_CBNZ (_,_,lab0)) ->
            (lab0:string) = lab
        | _ -> false)
        cs

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
      else init,cs,st

  end
