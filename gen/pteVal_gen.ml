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
  val compare : t -> t -> int
  val set_pteval : pte_atom -> t -> (unit -> string) -> t
end

module No(A:sig type arch_atom end) = struct
  type pte_atom = A.arch_atom
  type t = string
  let pp a = a
  let default s = s
  let compare _ _ = 0
  let set_pteval _ p _ = p
end
    
  
