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

(** No rmw instruction *)

module Make(A:sig type arch_atom end) = struct
  type rmw
  type rmw_atom = A.arch_atom

  let pp_rmw _ _ = assert false
  let is_one_instruction _ = assert false
  let fold_rmw _ r = r
  let fold_rmw_compat _ r = r
  let applies_atom_rmw _ _ _ = assert false
  let show_rmw_reg _ = assert false
  let compute_rmw _ _ _ = assert false
end
