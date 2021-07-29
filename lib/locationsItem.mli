(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type ('loc,'v) fault_atom = ('v,('loc,'v) ConstrGen.prop) Fault.atom

type ('loc, 'v) t =
| Loc of 'loc ConstrGen.rloc * TestType.t
| Fault of ('loc,'v) fault_atom

val fold_loc : ('loc -> 'r -> 'r) -> ('loc,'v) t -> 'r -> 'r
val fold_locs : ('loc -> 'r -> 'r) -> ('loc,'v) t list -> 'r -> 'r

val iter_loc : ('loc -> unit) -> ('loc,'v) t -> unit
val iter_locs : ('loc -> unit) -> ('loc,'v) t list -> unit

val map_loc : ('loc -> 'a) -> ('loc,'v) t -> ('a,'v) t
val map_locs : ('loc -> 'a) -> ('loc,'v) t list -> ('a,'v) t list

val locs_and_faults :
  ('loc,'v) t list -> ('loc ConstrGen.rloc list * ('loc,'v)fault_atom list)
