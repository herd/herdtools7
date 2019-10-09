(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Sign

module type Config = sig
  include CompileCommon.Config
  val realdep : bool
end

module Make(Cfg:Config) : XXXCompile_gen.S  =
  struct
    let naturalsize = TypBase.get_size Cfg.typ

    module RISCV =
      RISCVArch_gen.Make
        (struct
          let naturalsize = naturalsize
          let  moreedges = Cfg.moreedges
        end)
    include CompileCommon.Make(Cfg)(RISCV)

    let ppo _f k = k

    module AV=RISCV
    open RISCV
    open C
    open Code

(* Utilities *)
    let zero = AV.Ireg AV.X0
    let next_reg x = RISCV.alloc_reg x

    let next_init st p init loc =
      let rec find_rec = function
        | (Reg (p0,r0),Some loc0)::_ when loc0 = loc && p = p0 ->
            r0,init,st
        | _::rem -> find_rec rem
        | [] ->
            let r,st = next_reg st in
            r,(Reg (p,r),Some loc)::init,st in
      find_rec init


    let next_const st p init v = match v with
    | 0 -> zero,init,st
    | _ ->
        let r,st = next_reg st in
        r,(Reg (p,r),Some (Printf.sprintf "0x%x" v))::init,st


(**********************)
(* Basic instructions *)
(**********************)

    let  wloc =
      let open TypBase in
      match Cfg.typ with
      | Std (_,MachSize.Quad) -> AV.Double
      | Int |Std (_,MachSize.Word) -> AV.Word
      | t -> Warn.user_error "RISCV, illegal base type: %s" (pp t)


    let bne r1 r2 lab =  AV.Bcc (AV.NE,r1,r2,lab)
    let cbz r lab = AV.Bcc (AV.EQ,r,zero,lab)
    and cbnz r lab = AV.Bcc (AV.NE,r,zero,lab)

    let ori r1 r2 k = AV.OpI (AV.ORI,r1,r2,k)
    let li r k =  ori r zero k
    let andi r1 r2 k = AV.OpI (AV.ANDI,r1,r2,k)
    let mv r1 r2 = AV.Op (AV.OR,r1,r2,zero)

    let addiw r1 r2 k = AV.OpIW (AV.ADDIW,r1,r2,k)
    let subiw r1 r2 k = addiw r1 r2 (-k)
    let addi r1 r2 k = AV.OpI (AV.ADDI,r1,r2,k)
    let _subi r1 r2 k = addi r1 r2 (-k)

    let amoswap mo r1 r2 r3 = AV.Amo (AV.AMOSWAP,wloc,mo,r1,r2,r3)
    let amoor_as_load mo r1 r2 = AV.Amo (AV.AMOOR,wloc,mo,r1,zero,r2)
    and swap_as_store mo r1 r2 = AV.Amo (AV.AMOSWAP,wloc,mo,zero,r1,r2)

    let as_amo = Cfg.variant Variant_gen.AsAmo

    let ldr mo r1 r2 =  match mo with
    |AV.Rlx -> AV.Load (wloc,Signed,mo,r1,0,r2)
    |AV.Acq|AV.Rel|AV.AcqRel ->
        if as_amo then amoor_as_load mo r1 r2
        else AV.Load (wloc,Signed,mo,r1,0,r2)
    |AV.Sc -> assert false
    and str mo r1 r2 =  match mo with
    |AV.Rlx -> AV.Store (wloc,mo,r1,0,r2)
    |AV.Acq|AV.Rel|AV.AcqRel ->
        if as_amo then swap_as_store mo r1 r2
        else  AV.Store (wloc,mo,r1,0,r2)
    |AV.Sc -> assert false

    let add r1 r2 r3 = AV.Op (AV.ADD,r1,r2,r3)
    let xor r1 r2 r3 = AV.Op (AV.XOR,r1,r2,r3)

    let tr_sz = function
      | MachSize.Byte -> AV.Byte
      | MachSize.Short -> AV.Half
      | MachSize.Word -> AV.Word
      | MachSize.Quad -> AV.Double

    let ldr_mixed r1 r2 sz o = AV.Load (tr_sz sz,Signed,AV.Rlx,r1,o,r2)
    and str_mixed r1 r2 sz o = AV.Store (tr_sz sz,AV.Rlx,r1,o,r2)

    let lr mo r1 r2 = AV.LoadReserve (wloc,mo,r1,r2)
    and sc mo r1 r2 r3 = AV.StoreConditional (wloc,mo,r1,r2,r3)

    module Extra = struct
      let use_symbolic = false
      type reg = AV.reg
      type instruction = AV.pseudo

      let mov r v = Instruction (li r v)
      let mov_mixed _sz _r _v = assert false
      let mov_reg r1 r2 = Instruction (mv r1 r2)
      let mov_reg_mixed _sz _r1 _r2 = assert false
    end

     module U = GenUtils.Make(Cfg)(AV)(Extra)

(*********)
(* Loads *)
(*********)

    let emit_load_mixed sz o st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,lift_code [ldr_mixed rA rB sz o],st

    module LOAD =
      struct

        let load_idx mo st rA rB idx =
          let rD,st = next_reg st in
          [add rD rB idx;ldr mo rA rD],st

        let emit_load mo st p init x =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          rA,init,lift_code [ldr mo rA rB],st


        let emit_load_not_zero mo st p init x =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          lift_code
            [ldr mo rA rB; cbz rA lab],
          st

        let emit_load_one mo st p init x =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          lift_code [ldr mo rA rB; subiw rA rA 1; cbnz rA lab; li rA 1;],
          st

        let emit_load_not mo st p init x bne =
          let rA,st = next_reg st in
          let rC,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let lab = Label.next_label "L" in
          let out = Label.next_label "L" in
          let r200,init,st = next_const st p init 200 in
          rA,init,
          Instruction (mv rC r200)::
          (* 200 X about 5 ins looks for a typical memory delay *)
          Label (lab,Nop)::
          lift_code
            [
             ldr mo rA rB; bne rA out ;
             subiw rC rC 1 ;
             cbnz rC lab ;
           ]@
          [Label (out,Nop)],
          st

        let emit_load_not_eq mo st p init x rP =
          emit_load_not mo st p init x (fun r out -> bne rP r out)

        let emit_load_not_value mo st p init x v =
          let rC,init,st = next_const st p init v in
          emit_load_not_eq mo st p init x rC

        let emit_load_idx mo st p init x idx =
          let rA,st = next_reg st in
          let rB,init,st = next_init st p init x in
          let ins,st = load_idx mo st rA rB idx in
          rA,init,lift_code ins ,st
      end

(* For export *)
    let emit_load_one = LOAD.emit_load_one AV.Rlx
    let emit_load = LOAD.emit_load  AV.Rlx
    let emit_load_not_value = LOAD.emit_load_not_value AV.Rlx
    let emit_load_not_eq = LOAD.emit_load_not_eq AV.Rlx
    let emit_load_not_zero = LOAD.emit_load_not_zero AV.Rlx


(**********)
(* Stores *)
(**********)

    let emit_store_reg_mixed sz o st p init x rA =
      let rB,init,st = next_init st p init x in
      init,[Instruction (str_mixed rA rB sz o)],st

    let emit_store_mixed sz o st p init x v =
      let rA,init,csi,st = U.emit_mov st p init v in
      let init,cs,st = emit_store_reg_mixed sz o st p init x rA in
      init,csi@cs,st

    module STORE = struct

      let store_idx mo st rA rB idx =
        let rD,st = next_reg st in
        [add rD rB idx;str mo rA rD],st

      let emit_store_reg mo st p init x rA =
        let rB,init,st = next_init st p init x in
        init,[Instruction (str mo rA rB)],st

      let emit_store mo st p init x v =
        let rA,init,csi,st = U.emit_mov st p init v in
        let init,cs,st = emit_store_reg mo st p init x rA in
        init,csi@cs,st

      let emit_store_idx_reg mo st p init x idx rA =
        let rB,init,st = next_init st p init x in
        let ins,st = store_idx mo st rA rB idx in
        init,lift_code ins,st

      let emit_store_idx mo st p init x idx v =
        let rA,init,csi,st = U.emit_mov st p init v in
        let init,cs,st = emit_store_idx_reg mo st p init x idx rA in
        init,csi@cs,st
    end

(***************************)
(* Atomic loads and stores *)
(***************************)

    let tempo1 st = A.alloc_trashed_reg "T1" st

    let emit_loop_pair mo1 mo2 _p st rR rW rA =
      let lbl = Label.next_label "Loop" in
      let r,st = tempo1 st in
      let cs =
        [
         Label (lbl,Instruction (lr mo1 rR rA));
         Instruction (sc mo2 r rW rA);
         Instruction (cbnz r lbl);
       ] in
      cs,st

    let emit_one_pair mo1 mo2 p st r rR rW rA k =
      Instruction (lr mo1 rR rA)::
      Instruction (sc mo2 r rW rA)::
      Instruction (cbnz r (Label.fail p (current_label st)))::k,
      next_label_st st

    let emit_unroll_pair u mo1 mo2 p st rR rW rA =
      if u <= 0 then
        let r,st = next_reg st in
        lift_code [lr mo1 rR rA; sc mo2 r rW rA;],st
      else if u = 1 then
        let r,st = tempo1 st in
        emit_one_pair mo1 mo2 p st r rR rW rA []
      else
        let r,st = tempo1 st in
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              let cs,_ = emit_one_pair
                mo1 mo2 p st r rR rW rA [Label (out,Nop)] in
              cs
          | u ->
              (Instruction (lr mo1 rR rA)::
              Instruction (sc mo2 r rW rA)::
              Instruction (cbz r out)::
              do_rec (u-1)) in
        do_rec u,st

    let emit_pair = match Cfg.unrollatomic with
    | None -> emit_loop_pair
    | Some u -> emit_unroll_pair u

    let emit_lda_reg mo1 mo2 st p rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair mo1 mo2 p st rR rR rA in
      rR,cs,st

    let emit_lda mo1 mo2 st p init loc =
      let rA,init,st = next_init st p init loc in
      let r,cs,st =  emit_lda_reg mo1 mo2 st p rA in
      r,init,cs,st

    let emit_lda_idx mo1 mo2 st p init loc idx =
      let rA,init,st = next_init st p init loc in
      let rB,st = next_reg st in
      let r,cs2,st =  emit_lda_reg mo1 mo2 st p rB in
      r,init,lift_code [add rB rA idx]@cs2,st


    let do_emit_sta mo1 mo2 st p rW rA =
      let rR,st = next_reg st in
      let cs,st = emit_pair mo1 mo2 p st rR rW rA in
      rR,cs,st

    let emit_sta mo1 mo2 st p init loc v =
      let rA,init,st = next_init st p init loc in
      let rW,init,csv,st = U.emit_mov st p init v in
      let rR,cs,st = do_emit_sta mo1 mo2 st p rW rA in
      rR,init,csv@cs,st

    let emit_sta_reg mo1 mo2 st p init loc rW =
      let rA,init,st = next_init st p init loc in
      let rR,cs,st = do_emit_sta mo1 mo2 st p rW rA in
      rR,init,cs,st

    let emit_sta_idx mo1 mo2 st p init loc idx v =
      let rA,init,st = next_init st p init loc in
      let rX,st = next_reg st in
      let rW,init,csv,st = U.emit_mov st p init v in
      let rR,cs2,st = do_emit_sta mo1 mo2 st p rW rX in
      rR,init,csv@Instruction (add rX rA idx)::cs2,st

(**********)
(* Access *)
(**********)
    let emit_joker st init = None,init,[],st

    let emit_access  st p init e = match e.dir,e.loc with
    | None,_ -> Warn.fatal "TODO"
    | Some d,Data loc ->
        begin match d,e.atom with
        | Code.R,None ->
            let r,init,cs,st = LOAD.emit_load AV.Rlx st p init loc in
            Some r,init,cs,st
        | Code.R,Some (MO mo) ->
            let r,init,cs,st = LOAD.emit_load mo st p init loc  in
            Some r,init,cs,st
        | Code.R,Some (Atomic (mo1,mo2)) ->
            let r,init,cs,st = emit_lda mo1 mo2 st p init loc  in
            Some r,init,cs,st
        | Code.R,Some (Mixed (sz,o)) ->
            let r,init,cs,st = emit_load_mixed sz o st p init loc in
            Some r,init,cs,st
        | Code.W,None ->
            let init,cs,st = STORE.emit_store AV.Rlx st p init loc e.v in
            None,init,cs,st
        | Code.W,(Some (MO mo)) ->
            let init,cs,st = STORE.emit_store mo st p init loc e.v in
            None,init,cs,st
        | Code.W,Some (Atomic (mo1,mo2)) ->
            let r,init,cs,st = emit_sta mo1 mo2 st p init loc e.v in
            Some r,init,cs,st
        | Code.W,Some (Mixed (sz,o)) ->
            let init,cs,st = emit_store_mixed sz o st p init loc e.v in
            None,init,cs,st
        | Code.J, _ -> emit_joker st init
        end
    | _,Code _ -> Warn.fatal "No code location for RISCV"

    let tr_a = function
      | None -> AV.Rlx
      | Some (MO mo) -> mo
      | Some (Atomic _|Mixed _) as at ->
          Warn.fatal
            "bad atomicity in rmw, %s"
            (E.pp_atom_option at)

(*
  let emit_exch st p init er ew =
  let rA,init,st = next_init st p init er.loc in
  let rR,st = next_reg st in
  let rW,init,csv,st = U.emit_mov st p init ew.v in
  let mo1 = tr_a er.C.atom and mo2 = tr_a ew.C.atom in
  let cs,st = emit_pair mo1 mo2  p st rR rW rA in
  rR,init,csv@cs,st
 *)
    let tr_swap a1 a2 = match tr_a a1,tr_a a2 with
    | (AV.Rlx,a)|(a,AV.Rlx) -> a
    | (AV.Acq,AV.Rel)|(AV.AcqRel,_)|(_,AV.AcqRel) -> AV.AcqRel
    | (AV.Sc,_)|(_,AV.Sc) -> assert false
    | (AV.Rel,_)|(_,AV.Acq) ->
        Warn.fatal
          "bad atomicity in rmw, acquire on write or release on read"

    let emit_exch st p init er ew =
      let rA,init,st = next_init st p init (Code.as_data er.loc) in
      let rR,st = next_reg st in
      let rW,init,csv,st = U.emit_mov st p init ew.v in
      let mo = tr_swap er.C.atom ew.C.atom in
      rR,init,csv@[Instruction (amoswap mo rR rW rA)],st

    let emit_rmw () st p init er ew  =
      let rR,init,cs,st = emit_exch st p init er ew in
      Some rR,init,cs,st

(**********)
(* Fences *)
(**********)

    let emit_fence _ _ _ f = [Instruction (AV.FenceIns f)]
    let full_emit_fence = GenUtils.to_full emit_fence
    let stronger_fence = strong

        (* Dependencies *)
    let calc0 =
      if Cfg.realdep then
        fun dst src ->  andi dst src 128
      else
        fun dst src -> xor dst src src

    let emit_access_dep_addr st p init e rd =
      let r2,st = next_reg st in
      let c = calc0 r2 rd in
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some d,Data loc ->
          begin match d,e.atom with
          | Code.R,None ->
              let r,init,cs,st = LOAD.emit_load_idx AV.Rlx st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | Code.R,Some (MO mo) ->
              let r,init,cs,st = LOAD.emit_load_idx mo st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | Code.R,Some (Atomic (mo1,mo2)) ->
              let r,init,cs,st = emit_lda_idx mo1 mo2  st p init loc r2 in
              Some r,init, Instruction c::cs,st
          | Code.W,None ->
              let init,cs,st =
                STORE.emit_store_idx AV.Rlx st p init loc r2 e.v in
              None,init,Instruction c::cs,st
          | Code.W,Some (MO mo) ->
              let init,cs,st = STORE.emit_store_idx mo st p init loc r2 e.v in
              None,init,Instruction c::cs,st
          | Code.W,Some (Atomic (mo1,mo2)) ->
              let r,init,cs,st = emit_sta_idx mo1 mo2 st p init loc r2 e.v in
              Some r,init,Instruction c::cs,st
          | _,Some (Mixed _) ->
              Warn.fatal "addr dep with mixed"
          | Code.J, _ -> emit_joker st init
          end
      | _,Code _ -> Warn.fatal "No code location for RISCV"

    let emit_exch_dep_addr st p init er ew rd =
      let r2,st = next_reg st in let c = calc0 r2 rd in
      let loc = Code.as_data er.loc in
      let rA,init,st = next_init st p init loc in
      let rW,init,csv,st = U.emit_mov st p init ew.v in
      let rR,st = next_reg st in
      let mo = tr_swap er.C.atom ew.C.atom in
      let swap = Instruction (amoswap mo rR rW rA) in
      rR,init,
      Instruction c::Instruction (add r2 rA r2)::csv@[swap],st

    let emit_access_dep_data st p init e  r1 =
      match e.dir,e.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some Code.R,_ -> Warn.fatal "data dependency to load"
      | Some Code.W,Data loc ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (calc0 r2 r1) ;
             Instruction (ori r2 r2 e.v) ; ] in
          begin match e.atom with
          | None ->
              let init,cs,st = STORE.emit_store_reg AV.Rlx st p init loc r2 in
              None,init,cs2@cs,st
          | Some (MO mo) ->
              let init,cs,st = STORE.emit_store_reg mo st p init loc r2 in
              None,init,cs2@cs,st
          | Some (Atomic (mo1,mo2)) ->
              let r,init,cs,st = emit_sta_reg mo1 mo2 st p init loc r2 in
              Some r,init,cs2@cs,st
          | Some (Mixed _) ->
              Warn.fatal "data dep with mixed"
          end
      | Some Code.J,_ -> emit_joker st init
      | _,Code _ -> Warn.fatal "No code location for RISCV"

    let insert_isb isb cs1 cs2 =
      if isb then cs1@emit_fence 0 [] C.nil FenceI@cs2
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

    let emit_rmw_dep () st p init er ew dp rd =
      let r,init,cs,st = emit_exch_dep  st p init er ew dp rd in
      Some r,init,cs,st

    let check_load p r e init st =
      let lab = Label.exit p (current_label st) in
      let st = next_label_st st in
      let rI,init,ci,st = U.emit_mov st p init e.v in
      init,(fun k -> ci@(Instruction (bne r rI lab))::k),st

(* Postlude *)

    let list_of_fail_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = Instruction (RISCV.J (Label.exit p n))::
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
        let init,okcs,st = STORE.emit_store AV.Rlx st p init Code.ok_str 0 in
        init,
        cs@
        (list_of_fail_labels p st)@
        okcs@
        (list_of_exit_labels p st),
        st
      else if does_exit p st then
        init,cs@(list_of_exit_labels p st),st
      else
        init,cs,st


    let get_strx_result k = function
      | StoreConditional (_,_,r,_,_)  -> r::k
      | _ -> k

    let get_strx_result_pseudo k = pseudo_fold  get_strx_result k

    let get_xstore_results = match Cfg.unrollatomic with
    | Some x when x <= 0 ->
        fun cs ->
          let rs = List.fold_left get_strx_result_pseudo [] cs in
          List.rev_map (fun r -> r,0) rs
    | Some _|None -> fun _ -> []

  end
