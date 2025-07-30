(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** No rmw instruction *)
module No(A:sig
  type atom
  end) = struct
  type rmw = unit
  type atom = A.atom

  let pp_rmw _ _ = assert false
  let is_one_instruction _ = assert false
  let fold_rmw _ _ r = r
  let fold_rmw_compat _ r = r
  let applies_atom_rmw _ _ _ = assert false
  let show_rmw_reg _ = assert false
  let compute_rmw _ ~old:_ ~operand:_ = assert false
  let expand_rmw r = [r]
  let valid_rmw _ = assert false
end

(** The only RMW is exchange *)
module
  Make
    (I:
      sig
        type atom
        val pp : string
        val is_one_instruction : bool
      end) =
  struct
    type rmw = unit
    type atom = I.atom

    let pp_rmw compat () = if compat then "Rmw" else I.pp

    let is_one_instruction _ = I.is_one_instruction

    let fold_rmw _b f r = f () r
    let fold_rmw_compat f r = f () r

    let applies_atom_rmw () ar aw = match ar,aw with
      | None,None -> true
      | _,_ -> false

    let show_rmw_reg () = false

    let compute_rmw _ ~old:_ ~operand  = operand

    let expand_rmw rmw = [rmw]

    let valid_rmw _ = true
  end

module LxSx(A:sig type atom end) = struct
  include Make
    (struct
      type atom = A.atom
      let pp = "LxSx"
      let is_one_instruction = false
    end)
end
module Exch(A:sig type atom end) = struct
  include Make
    (struct
      type atom = A.atom
      let pp = "Exch"
      let is_one_instruction = true
    end)
end
