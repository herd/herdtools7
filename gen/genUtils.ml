(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2018-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let to_full emit st p init n x = init,emit st p init n x,st

module type Config = sig
  val hexa : bool
  val variant : Variant_gen.t -> bool
end


module type Extra = sig
  type reg
  type instruction
  val mov : reg -> int -> instruction
  val mov_mixed : MachSize.sz -> reg -> int -> instruction
  val mov_reg : reg -> reg -> instruction
  val mov_reg_mixed : MachSize.sz -> reg -> reg -> instruction
end

module Make(Cfg:Config)(A:Arch_gen.S)
    (Extra : Extra with
     type reg = A.reg and type instruction = A.pseudo) = struct
       open A

       let next_init st p init loc =
         let exact_match = List.find_opt ( function
           | (Reg (p0,_),Some (A.S loc0)) ->
             Misc.string_eq loc0 loc && Misc.int_eq p p0
           | _ -> false ) init in
         (* Despite `st` is local for procedure, `p`, we assume `st.regs`,
            i.e. the allocation pool, contains no `r0`.
            This means, `r0` is reserved for location `loc0` for all procedures,
            hence `A.alloc_reg st` will never return `r0`. Given this assumption,
            it is safe to bind `r0` to `loc` (`loc` = `loc0`) here. *)
         let same_loc_match = List.find_opt ( function
           | (Reg (_,_),Some (A.S loc0)) ->
             Misc.string_eq loc0 loc
           | _ -> false ) init in
         match exact_match,same_loc_match with
         | Some (Reg (_,r),Some _), _ -> r,init,st
         | None, Some (Reg (_,r),Some _) ->
           r,(Reg (p,r),Some (A.S loc))::init,st
         | None, None ->
           (* no previous register assignment, so add new *)
           let r,st = A.alloc_reg st in
           r,(Reg (p,r),Some (A.S loc))::init,st
         | _,_ -> Warn.user_error "Unexpected error in `next_init`."

       let find_init p init loc =
         let rec find_rec = function
           | (Reg (p0,r0),Some (A.S loc0))::_
             when Misc.string_eq loc0 loc && Misc.int_eq p p0
             -> r0
           | _::rem -> find_rec rem
           | [] -> raise Not_found in
         find_rec init

       let next_const st p init k =
         let exact_match = List.find_opt ( function
           | (Reg (p0,_),Some k0) ->
               A.initval_eq k k0 && p = p0
           | _ -> false ) init in
         let same_value_match = List.find_opt ( function
           | (Reg (_,_),Some k0) -> A.initval_eq k k0
           | _ -> false ) init in
         match exact_match,same_value_match with
         | Some (Reg (_,r),Some _), _ -> r,init,st
         | None, Some (Reg (_,r),Some _) ->
           r,(Reg (p,r),Some k)::init,st
         | None, None ->
           (* no previous register assignment, so add new *)
           let r,st = A.alloc_reg st in
           r,(Reg (p,r),Some k)::init,st
         | _,_ -> Warn.user_error "Unexpected error in `next_const."

       let allow_consts_in_code =
         not (Cfg.variant Variant_gen.ConstsInInit)

(* RISCV limit, fits all ? *)
       let max_k = 1 lsl 12 and min_k = - (1 lsl 12)

       let emit_const st p init v =
         if min_k <= v && v < max_k && allow_consts_in_code then
           None,init,st
         else
           let k =
             S (Printf.sprintf (if Cfg.hexa then "0x%x" else "%i") v) in
           let rA,init,st = next_const st p init k in
           Some rA,init,st

       let emit_pteval  st p init v = next_const st p init (A.P v)

       let emit_nop st p init nop =
         let rA,init,st = next_const st p init (S ("instr:\""^nop^"\"")) in
         rA,init,st

       let emit_mov st p init v = match emit_const st p init v with
       | None,init,st ->
           let rA,st = A.alloc_reg st in
           rA,init,[Extra.mov rA v],st
       | Some rA,init,st ->
           rA,init,[],st

       let emit_mov_sz sz st p init v = match emit_const st p init v with
       | None,init,st ->
           let rA,st = A.alloc_reg st in
           rA,init,[Extra.mov_mixed sz rA v],st
       | Some rA,init,st ->
           rA,init,[],st

       let emit_mov_fresh st p init v = match emit_const st p init v with
       | None,init,st ->
           let rA,st = A.alloc_reg st in
           rA,init,[Extra.mov rA v],st
       | Some rA,init,st ->
           let rB,st = A.alloc_reg st in
           rB,init,[Extra.mov_reg rB rA],st

       let emit_mov_sz_fresh sz st p init v = match emit_const st p init v with
       | None,init,st ->
           let rA,st = A.alloc_reg st in
           rA,init,[Extra.mov_mixed sz rA v],st
       | Some rA,init,st ->
           let rB,st = A.alloc_reg st in
           rB,init,[Extra.mov_reg_mixed sz rB rA],st

     end
