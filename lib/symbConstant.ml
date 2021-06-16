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
  and stringToV s = Concrete (Scalar.of_string s)

  let bit_at k v = Scalar.bit_at k v

  let zero = Concrete Scalar.zero
  and one = Concrete Scalar.one

  let rec do_pp pp_symbol pp_scalar hexa = function
    | Concrete i -> pp_scalar hexa i
    | ConcreteVector (_,vs) ->
        let s =
          String.concat ","
            (List.map (do_pp pp_symbol pp_scalar hexa) vs)
        in sprintf "{%s}" s
    | Symbolic sym -> pp_symbol sym
    | Label (p,lbl)  -> sprintf "%i:%s" p lbl
    | Tag s -> sprintf ":%s" s
    | PteVal p -> PTEVal.pp p

  let pp = do_pp Constant.pp_symbol Scalar.pp
  and pp_unsigned = do_pp Constant.pp_symbol Scalar.pp_unsigned

  let pp_v = pp false
  let pp_v_old = do_pp Constant.pp_symbol_old Scalar.pp false

  let rec compare c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2
  | ConcreteVector (_sz1, v1), ConcreteVector (_sz2, v2) ->
      Misc.list_compare compare v1 v2
  | Symbolic sym1,Symbolic sym2 -> compare_symbol sym1 sym2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.pair_compare Proc.compare String.compare (p1,s1) (p2,s2)
  | Tag t1,Tag t2 -> String.compare t1 t2
  | PteVal p1,PteVal p2 -> PTEVal.compare p1 p2
  | (Concrete _,(ConcreteVector _|Symbolic _|Label _|Tag _|PteVal _))
  | (ConcreteVector _,(Symbolic _|Label _|Tag _|PteVal _))
  | (Symbolic _,(Label _|Tag _|PteVal _))
  | (Label _,(Tag _|PteVal _))
  | (Tag _,PteVal _)
    -> -1
  | (PteVal _,(Tag _|Label _|Symbolic _|ConcreteVector _|Concrete _))
  | (Tag _,(Label _|Symbolic _|ConcreteVector _|Concrete _))
  | (Label _,(Symbolic _|ConcreteVector _|Concrete _))
  | (Symbolic _,(ConcreteVector _|Concrete _))
  | (ConcreteVector _,Concrete _)
    -> 1

  let rec eq c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2 = 0
  | ConcreteVector (_,v1), ConcreteVector (_,v2) ->
      Misc.list_eq eq v1 v2
  | Symbolic s1, Symbolic s2 -> Constant.symbol_eq s1 s2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.string_eq  s1 s2 && Misc.int_eq p1 p2
  | Tag t1,Tag t2 -> Misc.string_eq t1 t2
  | PteVal p1,PteVal p2 -> PTEVal.compare p1 p2 = 0
  | (PteVal _,(Symbolic _|Concrete _|ConcreteVector _|Label _|Tag _))
  | (ConcreteVector _,(Symbolic _|Label _|Tag _|Concrete _|PteVal _))
  | (Concrete _,(Symbolic _|Label _|Tag _|ConcreteVector _|PteVal _))
  | (Symbolic _,(Concrete _|Label _|Tag _|ConcreteVector _|PteVal _))
  | (Label _,(Concrete _|Symbolic _|Tag _|ConcreteVector _|PteVal _))
  | (Tag _,(Concrete _|Symbolic _|Label _|ConcreteVector _|PteVal _))
    -> false

 (* For building code symbols. *)
  let vToName = function
    | Symbolic s-> Constant.as_address s
    | Concrete _|ConcreteVector _ | Label _|Tag _|PteVal _
        -> assert false

(* Arch dependant result *)
  exception Result of Archs.t * v * string
end
