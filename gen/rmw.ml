(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type RmwType = sig
  type arch_atom
  type value
end

(** No rmw instruction *)
module No(A:RmwType) = struct
  type rmw = unit
  type atom = A.arch_atom
  type value = A.value

  let pp_rmw _ _ = assert false
  let is_one_instruction _ = assert false
  let fold_rmw _ r = r
  let fold_rmw_compat _ r = r
  let applies_atom_rmw _ _ _ = assert false
  let show_rmw_reg _ = assert false
  let compute_rmw _ _ _ = assert false
end

(** The only RMW is exchange *)
module
  Make
    (I:
      sig
        include RmwType
        val pp : string
        val is_one_instruction : bool
      end) =
  struct
    type rmw = unit
    type atom = I.arch_atom
    type value = I.value

    let pp_rmw compat () = if compat then "Rmw" else I.pp

    let is_one_instruction _ = I.is_one_instruction

    let fold_rmw f r = f () r
    let fold_rmw_compat f r = f () r

    let applies_atom_rmw () ar aw = match ar,aw with
      | None,None -> true
      | _,_ -> false

    let show_rmw_reg () = false

    let compute_rmw () _old co_cell  = co_cell
  end

module LxSx(A:RmwType) = struct
  include Make
    (struct
      type arch_atom = A.arch_atom
      type value = A.value
      let pp = "LxSx"
      let is_one_instruction = false
    end)
end
module Exch(A:RmwType) = struct
  include Make
    (struct
      type arch_atom = A.arch_atom
      type value = A.value
      let pp = "Exch"
      let is_one_instruction = true
    end)
end
