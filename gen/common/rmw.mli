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
module No(A:sig type atom end) : Atom.RMW with type rmw = unit and type atom = A.atom

(** The only RMW is exchange *)
(* Implemented as load reserve store conditional *)
module LxSx(A:sig type atom end) : Atom.RMW with type rmw = unit and type atom = A.atom

(* Implemented as exchange instruction *)
module Exch(A:sig type atom end) : Atom.RMW with type rmw = unit and type atom = A.atom
