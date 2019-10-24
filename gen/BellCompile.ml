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
  val realdep : bool
end

module Make(Cfg:Config)(BO:BellArch_gen.Config) : XXXCompile_gen.S =
  struct

(* Common *)
    module Bell = BellArch_gen.Make(BO)
    include CompileCommon.Make(Cfg)(Bell)

    let ppo _f k = k

    open Bell
    open C

(* Utilities *)

    let tempo1 = Symbolic_reg "T1" (* May be used for branch cond *)
    let next_reg x = Bell.alloc_reg x

    let pseudo = List.map (fun i -> Instruction i)

(* Bell instructions *)
    let symb_of_string x = Abs (ParsedConstant.nameToV x)
(*    let movi r i = Pmov (r,IAR_imm i) *)
    let ld_tagged r x a = Pld (r, Addr_op_atom (symb_of_string x),a)
    let ld r x = ld_tagged r x []
    let ld_idx_tagged r x idx a =
      Pld (r, Addr_op_add (symb_of_string x,Regi idx),a)

    let st_tagged x v a =
      Pst (Addr_op_atom (symb_of_string  x),Imm v,a)

    let st_reg_tagged x r a =
      Pst (Addr_op_atom (Abs (ParsedConstant.nameToV x)),Regi r,a)

    let st_idx_tagged x v idx a =
      Pst (Addr_op_add (symb_of_string x,Regi idx),Imm v,a)

    let mov rA op = Pmov (rA,op)

    let movne rA rB k = mov  rA (OP (Neq,IAR_roa (Rega rB),IAR_imm k))
    let moveq rA rB k = mov  rA (OP (Eq,IAR_roa (Rega rB),IAR_imm k))
    let xor r1 r2 r3 = mov r1 (OP (Xor,IAR_roa (Rega r2),IAR_roa (Rega r3)))
    let addk r1 r2 k = mov r1 (OP (Add,IAR_roa (Rega r2),IAR_imm k))
    let andk r1 r2 k = mov r1 (OP (And,IAR_roa (Rega r2),IAR_imm k))
    let branchcc reg lab = Pbranch (Some reg,lab,[])


(*    let addi r1 r2 i = Add(r1,IAR_roa (Rega r2),IAR_imm i)
    let subi r1 r2 i = Add(r1,IAR_roa (Rega r2),IAR_imm (-i))
    let xor r1 r2 r3 = Xor(r1,IAR_roa (Rega r2),IAR_roa (Rega r3))

    let exch_tagged r x v a =
      Prmw2_op (r,Abs (Constant.Symbolic x),Imm v,RMWExch,a) *)

(**********)
(* Export *)
(**********)

let emit_joker st init = None,init,[],st

(* Loads (some specific added) *)
    let emit_load_tagged st _p init x a =
      let rA,st = next_reg st in
      rA,init,pseudo [ld_tagged rA x a],st

    let emit_load st p init x = emit_load_tagged st p init x  []

    let emit_obs = emit_load

    let emit_load_idx_tagged st _p init x idx a =
      let rA,st = next_reg st in
      rA,init,pseudo [ld_idx_tagged rA x idx a],st

    let emit_load_idx st p init x idx =
      emit_load_idx_tagged st p init x idx []

    let emit_obs_not_zero st _p init x =
      let rA,st = next_reg st in
      let rB,st = next_reg st in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      pseudo
        [ld rA x; movne rB rA 0 ; branchcc rB lab;],
      st

    let emit_load_one st _p init x =
      let rA,st = next_reg st in
      let rB,st = next_reg st in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      pseudo [ld rA x; movne rB rA 1; branchcc rB lab],
      st

(*    let emit_load_not st _p init x bcc =
      let rA,st = next_reg st in
      let rC,st = next_reg st in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      Instruction (movi rC 200)::
      (* 200 X about 5 ins looks for a typical memory delay *)
      Label (lab,Nop)::
      pseudo
        [
         ld rA x; bcc rA out;
         subi rC rC 1 ;
         bcci Ne rC 0 lab ;
       ]@
      [Label (out,Nop)],
      st

*)

(*
    let emit_load_not_eq st p init x rP =
      emit_load_not st p init x
zz        (fun r lab -> bcc Ne r rP lab)

    let emit_load_not_value st p init x v =
      emit_load_not st p init x
        (fun r lab -> bcci Ne r v lab)
*)

    let emit_obs_not_eq _ = assert false
    let emit_obs_not_value _ = assert false

(* Stores *)

    let emit_store_tagged st _p init x v a =
      init,[Instruction (st_tagged x v a)],st

    let emit_store st p init x v = emit_store_tagged st p init x v []

    let emit_store_idx_tagged st _p init x v idx a =
      init,[Instruction (st_idx_tagged x v idx a)],st

    let emit_store_idx st p init x v idx =
      emit_store_idx_tagged st p init x v idx []

    let emit_store_reg_tagged st _p init x r a =
      init,[Instruction (st_reg_tagged x r a)],st

    let emit_store_reg st p init x r =
      emit_store_reg_tagged st p init x r []

(**********)
(* Access *)
(**********)

    let emit_access  st p init e = match e.dir with
    | None -> Warn.fatal "BellCompile.emit_access"
    | Some d ->
        match d,e.atom,e.loc with
        | R,None,Data loc ->
            let r,init,cs,st = emit_load st p init loc in
            Some r,init,cs,st
        | R,Some a,Data loc ->
            let r,init,cs,st = emit_load_tagged st p init loc a in
            Some r,init,cs,st
        | W,None,Data loc ->
            let init,cs,st = emit_store st p init loc e.v in
            None,init,cs,st
        | W,Some a,Data loc ->
            let init,cs,st = emit_store_tagged st p init loc e.v a in
            None,init,cs,st
        | J,_,_ -> emit_joker st init
        | _,_,Code _ -> Warn.fatal "No code location in Bell"

(* Dubious... *)
    let _tr_a ar aw = match ar,aw with
    | None,None -> []
    | (Some a,None)
    | (None,Some a) -> a
    | Some a,Some _ -> a

(*    let emit_exch st _p init er ew =
      let rR,st = next_reg st in
      let arw = tr_a er.C.atom ew.C.atom in
      rR,init,[Instruction (exch_tagged rR er.loc ew.v arw)],st *)

let emit_rmw _ = assert false

(**********)
(* Fences *)
(**********)

    let emit_fence _ _ _ f =  [Instruction (Pfence f)]
    let full_emit_fence = GenUtils.to_full emit_fence
    let _emit_fence_tagged o a = Instruction (Pfence(Fence(a,o)))

    let stronger_fence = strong

(****************)
(* Dependencies *)
(****************)

(*jade: l'idee c'est de tout faire par les labelled fences en LISA*)

    let calc_zero =
      if Cfg.realdep then fun dst src ->  Instruction  (andk dst src kbig)
      else fun dst src ->  Instruction  (xor dst src src)

    let emit_access_dep_addr st p init e r1 =
      let idx,st = next_reg st in
      let cA = calc_zero idx r1 in
      begin match Misc.as_some e.dir,e.atom,e.loc with
      | R,None,Data loc ->
          let rC,init,cs,st = emit_load_idx st p init loc idx in
          Some rC,init,cA::cs,st
      | R,Some a,Data loc ->
          let rC,init,cs,st = emit_load_idx_tagged st p init loc idx a in
          Some rC,init,cA::cs,st
      | W,None,Data loc ->
          let init,cs,st = emit_store_idx st p init loc e.v idx in
          None,init,cA::cs,st
      | W,Some a,Data loc ->
          let init,cs,st = emit_store_idx_tagged st p init loc e.v idx a in
          None,init,cA::cs,st
      | J,_,Data _ -> emit_joker st init
      | _,_,Code _ -> Warn.fatal "No code location for Bell"
      end

    let emit_access_dep_data st p init e r1 = match e.dir,e.loc with
    | None,_ ->   Warn.fatal "BellCompile.emit_access_dep_data"
    | Some R,_ ->  Warn.fatal "data dependency to load"
    | Some W,Data loc ->
        let r2,st = next_reg st in
        let cs2 =  [calc_zero r2 r1;Instruction (addk r2 r2 e.v);] in
        begin match e.atom with
        | None ->
            let init,cs,st = emit_store_reg st p init loc r2 in
            None,init,cs2@cs,st
        | Some a ->
            let init,cs,st = emit_store_reg_tagged st p init loc r2 a in
            None,init,cs2@cs,st
        end
    | Some J,Data _ -> emit_joker st init
    | _,Code _ -> Warn.fatal "No code location for Bell"

let emit_access_ctrl st p init e r1 v1 =
  if Cfg.realdep then
    let lab =  Label.exit p (current_label st) in
    let st = next_label_st st in
    let rd,st = next_reg st in
    let c =
       [Instruction (movne rd r1 v1) ;
       Instruction (branchcc rd lab) ;] in
    let ropt,init,cs,st = emit_access st p init e in
    ropt,init,c@cs,st
  else
    let lab = Label.next_label "LC" in
    let rd,st = next_reg st in
    let c =
      [Instruction (moveq rd r1 0) ;
       Instruction (branchcc rd lab) ;
       Label (lab,Nop);] in
    let ropt,init,cs,st = emit_access st p init e in
    ropt,init,c@cs,st

let emit_access_dep  st p init e dp r1 v1 = match dp with
| ADDR -> emit_access_dep_addr st p init e r1
| DATA -> emit_access_dep_data st p init e r1
| CTRL -> emit_access_ctrl st p init e r1 v1

let emit_rmw_dep _ = assert false

(*jade: ca me parait un peu fort d'avoir ca required non?*)

(*    let emit_access_dep_addr st p init e  rd =
      let r2,st = next_reg st in
      let c =  xor r2 rd rd in
      match e.dir,e.atom with
      | R,None ->
          let r,init,cs,st = emit_load_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Some a ->
          let r,init,cs,st = emit_load_idx_tagged st p init e.loc r2 a in
          Some r,init, Instruction c::cs,st
      | W,None ->
          let init,cs,st = emit_store_idx st p init e.loc e.v r2 in
          None,init, Instruction c::cs,st
      | W,Some a ->
          let init,cs,st = emit_store_idx_tagged st p init e.loc e.v r2 a in
          None,init, Instruction c::cs,st

    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R -> Warn.fatal "data dependency to load"
      | W ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (xor r2 r1 r1) ;
             Instruction (addi r2 r2 e.v) ; ] in
          begin match e.atom with
          | None ->
              let init,cs,st = emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Some a ->
              let init,cs,st = emit_store_reg_tagged st p init e.loc r2 a in
              None,init,cs2@cs,st
          end

    let emit_access_ctrl st p init e r1 =
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (bcci Eq r1 0 lab); Label (lab,Nop);] in
      let ropt,init,cs,st = emit_access st p init e in
      ropt,init,c@cs,st

    let emit_access_dep st p init e dp r1 = match dp with
    | ADDR -> emit_access_dep_addr st p init e r1
    | DATA -> emit_access_dep_data st p init e r1
    | CTRL -> emit_access_ctrl st p init e r1


    let emit_exch_dep _ = assert false *)

(* Check load *)
    let do_check_load p st r e =
      let lab = Label.exit p (current_label st) in
      (fun k ->
        Instruction (movne tempo1 r e.v)::
        Instruction (branchcc tempo1 lab)::k),
        next_label_st st

    let check_load  p r e init st =
      let cs,st = do_check_load p st r e in
      init,cs,st

(* Postlude for adding exit label *)

    let does_jump lab cs =
      List.exists
        (fun i -> match i with
        | Instruction (Pbranch (_,lab0,_)) ->
            (lab0:string) = lab
        | _ -> false)
        cs

    let does_exit p cs st = does_jump (Label.exit p (current_label st)) cs

    let list_of_exit_labels p st =
      let rec do_rec i k =
        match i with
        | 0 -> k
        | n -> let k' = Label (Label.exit p n,Nop)::k
               in do_rec (i-1) k'
      in
    do_rec (current_label st) []

    let postlude st p init cs =
      if does_exit p cs st then
       init,cs@(list_of_exit_labels p st),st
      else init,cs,st

    let get_xstore_results _ = []
  end
