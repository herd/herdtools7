(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*       Luc Maranget INRIA Paris-Rocquencourt, France.              *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Code

module Make(Cfg:CompileCommon.Config)(BO:BellArch.Config) : XXXCompile.S =
  struct

(* Common *)
    module Bell = BellArch.Make(BO)
    include CompileCommon.Make(Cfg)(Bell)

    let ppo _f k = k

    open Bell
    open C

(* Utilities *)


    let next_reg x = Bell.alloc_reg x

    let pseudo = List.map (fun i -> Instruction i)

(* Bell instructions *)
    let movi r i = Pmov (r,IAR_imm i)
    let ld_tagged r x a = Pld (r, Addr_op_atom (Abs (Constant.Symbolic x)),a)
    let ld r x = ld_tagged r x []
    let ld_idx_tagged r x idx a =
      Pld (r, Addr_op_add (Abs (Constant.Symbolic x),Regi idx),a)

    let st_tagged x v a =
      Pst (Addr_op_atom (Abs (Constant.Symbolic x)),Imm v,a)

    let st_reg_tagged x r a =
      Pst (Addr_op_atom (Abs (Constant.Symbolic x)),Regi r,a)

    let st_idx_tagged x v idx a =
      Pst (Addr_op_add (Abs (Constant.Symbolic x),Regi idx),Imm v,a)

    let bcci cond r i lab = Pbcc (cond,r,Imm i,lab)
    let bcc cond r1 r2 lab = Pbcc (cond,r1,Regi r2,lab)
    let addi r1 r2 i = Pop (Add,r1,IAR_roa (Rega r2),IAR_imm i)
    let subi r1 r2 i = Pop (Add,r1,IAR_roa (Rega r2),IAR_imm (-i))
    let xor r1 r2 r3 =
      Pop (Xor,r1,IAR_roa (Rega r2),IAR_roa (Rega r3))

(**********)
(* Export *)
(**********)

(* Loads (some specific added) *)
    let emit_load_tagged st _p init x a =
      let rA,st = next_reg st in
      rA,init,pseudo [ld_tagged rA x a],st

    let emit_load st p init x = emit_load_tagged st p init x []

    let emit_load_idx_tagged st _p init x idx a =
      let rA,st = next_reg st in
      rA,init,pseudo [ld_idx_tagged rA x idx a],st

    let emit_load_idx st p init x idx =
      emit_load_idx_tagged st p init x idx []

    let emit_load_not_zero st _p init x =
      let rA,st = next_reg st in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      pseudo [ld rA x; bcci Ne rA 0 lab],
      st

    let emit_load_one st _p init x =
      let rA,st = next_reg st in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::
      pseudo [ld rA x; bcci Ne rA 1 lab],
      st

    let emit_load_not st _p init x bcc =
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

    let emit_load_not_eq st p init x rP =
      emit_load_not st p init x
        (fun r lab -> bcc Ne r rP lab)

    let emit_load_not_value st p init x v =
      emit_load_not st p init x
        (fun r lab -> bcci Ne r v lab)


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

    let emit_access  st p init e = match e.dir,e.atom with
    | R,None ->
        let r,init,cs,st = emit_load st p init e.loc in
        Some r,init,cs,st
    | R,Some a ->
        let r,init,cs,st = emit_load_tagged st p init e.loc a in
        Some r,init,cs,st
    | W,None ->
        let init,cs,st = emit_store st p init e.loc e.v in
        None,init,cs,st
    | W,Some a ->
        let init,cs,st = emit_store_tagged st p init e.loc e.v a in
        None,init,cs,st

    let emit_exch _ = assert false

(**********)
(* Fences *)
(**********)

    let emit_fence f =  Instruction (Pfence f)

    let stronger_fence = strong

(****************)
(* Dependencies *)
(****************)

    let emit_access_dep_addr st p init e  rd =
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


    let emit_exch_dep _ = assert false

(* No FNO's *)
    let emit_fno _ = assert false
    let emit_fno2 _ = assert false
    let emit_open_fno _ = assert false
    let emit_close_fno _ = assert false

(* Check load *)
    let check_load p r e =
      let lab = Label.exit p in
      fun k -> Instruction (bcci Ne r e.v lab)::k

(* Postlude for adding exit label *)

    let does_jump lab cs =
      List.exists
        (fun i -> match i with
        | Instruction (Pbal lab0|Pbcc (_,_,_,lab0)) ->
            (lab0:string) = lab
        | _ -> false)
        cs

    let does_exit p cs =  does_jump (Label.exit p) cs

    let postlude st p init cs =
      if does_exit p cs then
        init,cs@[Label (Label.exit p,Nop)],st
      else init,cs,st

  end
