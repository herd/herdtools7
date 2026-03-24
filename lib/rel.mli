(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Implementation of relations *)

module type S = sig

  type e1
  type e2

  module Es1 : MySet.S with type elt = e1
  module Es2 : MySet.S with type elt = e2

  module M : MyMap.S with type key = e1

  include MyRel.S
    with type elt1 = e1 and type elt2 = e2
    and module Elts1 = Es1 and module Elts2 = Es2
    and type t = Es2.t M.t
end

module Make :
functor (O1:MySet.OrderedType) ->
  functor(O2:MySet.OrderedType) ->
    S with
type e1 = O1.t and type e2 = O2.t
and module Es1 = MySet.Make(O1)
and module Es2 = MySet.Make(O2)
