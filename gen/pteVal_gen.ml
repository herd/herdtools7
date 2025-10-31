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

module type S = sig
  type pte_atom
  type t
  val pp : t -> string
  val default : string -> t
  (* Initial a valid pte_value that can be used to process atom list `pte_atom` *)
  val init : string -> pte_atom list -> t
  val compare : t -> t -> int
  val as_virtual: t -> string option
  val set_pteval : pte_atom -> t -> (unit -> string) -> t
  (* Implicitly set pte value. Return indicates fault check and new pte vaule *)
  val implicit_set_pteval : Code.dir -> StringSet.t -> t -> (Code.extr * t) option
  val can_fault : Code.dir -> t -> bool
  (* check if the `pte_atom` trigger fault check for further access,
     Dir W and Dir R for write and read, respectively.
     and Irr for both, NoDir for none *)
  val need_check_fault : pte_atom option -> Code.extr
end

module No(A:sig type arch_atom end) = struct
  type pte_atom = A.arch_atom
  type t = string
  let pp a = a
  let default s = s
  let init s _atom_list = default s
  let compare _ _ = 0
  let as_virtual _ = None
  let set_pteval _ p _ = p
  let implicit_set_pteval _ _ _ = None
  let can_fault _ _t = false
  let need_check_fault _ = Code.NoDir
end


