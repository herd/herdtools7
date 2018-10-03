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

module type Config = sig
  val hexa : bool
  val variant : Variant_gen.t -> bool
end


module type Extra = sig
  val use_symbolic : bool
  type reg
  type instruction
  val mov : reg -> int -> instruction   
  val mov_mixed : MachSize.sz -> reg -> int -> instruction
end

module Make(Cfg:Config)(A:Arch_gen.S)
    (Extra : Extra with
     type reg = A.reg and type instruction = A.pseudo) = struct
       open A

       let next_init st p init loc =
         let rec find_rec = function
           | (Reg (p0,r0),loc0)::_ when loc0 = loc && p = p0 ->
	       r0,init,st
           | _::rem -> find_rec rem
           | [] ->
	       let r,st =
                 if Extra.use_symbolic then
                   A.symb_reg (Printf.sprintf "%s%i" loc p),st
                 else A.alloc_reg st in
	       r,(Reg (p,r),loc)::init,st in
         find_rec init

       let next_const st p init v =
         let k = Printf.sprintf (if Cfg.hexa then "0x%x" else "%i") v in
         let rec find_rec = function
           | (Reg (p0,r0),k0)::_ when k0 = k && p = p0 ->
	       r0,init,st
           | _::rem -> find_rec rem 
           | [] ->
               let r,st = A.alloc_reg st in
               r,(Reg (p,r),k)::init,st in
         find_rec init

       let allow_consts_in_code =
         not (Cfg.variant Variant_gen.ConstsInInit)

(* RISCV limit, fits all ? *)
       let max_k = 1 lsl 12 and min_k = - (1 lsl 12)

       let emit_const st p init v =
         if min_k <= v && v < max_k && allow_consts_in_code then
	   None,init,st
         else
	   let rA,init,st = next_const st p init v in
	   Some rA,init,st

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

     end
