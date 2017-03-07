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
module Make(V:Constant.S) =
  struct
    module A = LISAArch_litmus.Make(V)
    open A
    open A.Out
    open CType
    open Printf
      
(***************************************************)
(* Extract explicit [symbolic] addresses from code *)
(***************************************************)
    let abs_to_string abs =  A.vToName abs

    let extract_ra = function
      | Rega _ -> StringSet.empty
      | Abs a -> StringSet.singleton (abs_to_string a)

    let extract_iar = function
      | IAR_roa ra -> extract_ra ra
      | IAR_imm _ -> StringSet.empty

    let extract_ao = function
      | Addr_op_atom ra
      | Addr_op_add (ra,_)
        -> extract_ra ra

    let extract_op = function
      | RAI iar -> extract_iar iar
      | Add (iar1,iar2)
      | Xor (iar1,iar2)
      | And (iar1,iar2)
      | Eq (iar1,iar2)
      | Neq (iar1,iar2)
          ->  StringSet.union (extract_iar iar1) (extract_iar iar2)

    let extract_addrs = function
      | Pld (_,ao,_)
      | Pst (ao,_,_)
        -> extract_ao ao
      | Prmw (_,op,ao,_) ->
          StringSet.union (extract_op op) (extract_ao ao)
      | Pmov  (_,op) -> extract_op op            
      | Pfence _|Pcall _|Pbranch _ -> StringSet.empty

(*****************************)
(* Compilation (to kernel C) *)
(*****************************)

            
    let compile_ins tr_lab ins k = match ins with
    | Pld (r,Addr_op_atom (Abs (Constant.Symbolic s)) ,["once"]) ->
        { empty_ins with
          memo = sprintf "^o0 = READ_ONCE(*%s);" s;
          outputs = [r;] }::k
    | _ -> k


(********)
(* Vrac *)
(********)

    let stable_regs _ins = A.RegSet.empty

    let emit_loop k =  Warn.fatal "no time loop for LISA"

    let signaling_write _i _k = Warn.fatal "no signaling write for LISA"

    let emit_tb_wait _ = Warn.fatal "no time base for LISA"
  end
