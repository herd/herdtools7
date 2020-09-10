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

module Make(Scalar:Scalar.S) = struct
  open Printf

  module Scalar = Scalar

  type v = Scalar.t Constant.t
  open Constant

  let intToV i = Concrete (Scalar.of_int i)
  and nameToV s = Constant.mk_sym s

  let zero = Concrete Scalar.zero
  and one = Concrete Scalar.one


  let compare c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2
  | Symbolic sym1,Symbolic sym2 -> symbol_compare sym1 sym2
  | Label (p1,s1),Label (p2,s2) ->
      begin match String.compare s1 s2 with
      | 0 -> Proc.compare p1 p2
      | r -> r
      end
  | Tag t1,Tag t2 -> String.compare t1 t2
  | PteVal p1,PteVal p2 -> PTEVal.compare p1 p2
  | (Concrete _,(Symbolic _|Label _|Tag _|PteVal _))
  | (Symbolic _,(Label _|Tag _|PteVal _))
  | (Label _,(Tag _|PteVal _))
  | (Tag _,PteVal _)
      -> -1
  | (Symbolic _|Label _|Tag _|PteVal _),Concrete _
  | ((Label _|Tag _|PteVal _),Symbolic _)
  | ((Tag _|PteVal _),Label _)
  | (PteVal _,Tag _)
      -> 1

  let pp hexa = function
    | Concrete i -> Scalar.pp hexa i
    | Symbolic s -> pp_symbol s
    | Label (p,lbl)  -> sprintf "%i:%s" p lbl
    | Tag s -> sprintf ":%s" s
    | PteVal p -> PTEVal.pp p

  let pp_v = pp false

  let tag_eq = Misc.opt_eq Misc.string_eq

  let location_eq (s1,t1) (s2,t2) = Misc.string_eq s1 s2 && tag_eq t1 t2

  let eq c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2 = 0
  | Symbolic (Virtual (s1,o1)),Symbolic (Virtual (s2,o2)) ->
      location_eq  s1 s2 && Misc.int_eq o1 o2
  | Symbolic (Physical (s1,o1)),Symbolic (Physical (s2,o2)) ->
      Misc.string_eq s1 s2 && Misc.int_eq o1 o2
  | Symbolic (System (t1,s1)),Symbolic (System (t2,s2)) ->
      t1=t2 && Misc.string_eq s1 s2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.string_eq  s1 s2 && Misc.int_eq p1 p2
  | Tag t1,Tag t2 -> Misc.string_eq t1 t2
  | PteVal p1,PteVal p2 -> PTEVal.compare p1 p2 = 0
  | (PteVal _,(Symbolic _|Concrete _|Label _|Tag _))
  | (Concrete _,(Symbolic _|Label _|Tag _|PteVal _))
  | (Symbolic _,(Concrete _|Label _|Tag _|PteVal _))
  | (Label _,(Concrete _|Symbolic _|Tag _|PteVal _))
  | (Tag _,(Concrete _|Symbolic _|Label _|PteVal _))
  | (Symbolic (Virtual _),Symbolic (Physical _|System _))
  | (Symbolic (Physical _),Symbolic (Virtual _|System _))
  | (Symbolic (System _),Symbolic (Virtual _|Physical _))
    -> false

(* Arch dependant result *)
  exception Result of Archs.t * v * string
end
