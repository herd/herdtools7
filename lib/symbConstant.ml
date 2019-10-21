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
  and nameToV s = Symbolic ((s,None),0)

  let zero = Concrete Scalar.zero
  and one = Concrete Scalar.one

  let tag_compare = Misc.opt_compare String.compare

  let compare c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2
  | Symbolic ((s1,t1),o1),Symbolic ((s2,t2),o2) ->
      begin match String.compare s1 s2 with
      | 0 ->
          begin match tag_compare t1 t2 with
          | 0 -> Misc.int_compare o1 o2
          | r -> r
          end
      | r -> r
      end
  | Label (p1,s1),Label (p2,s2) ->
      begin match String.compare s1 s2 with
      | 0 -> Proc.compare p1 p2
      | r -> r
      end
  | Tag t1,Tag t2 -> tag_compare t1 t2
  | (Concrete _,(Symbolic _|Label _|Tag _))
  | (Symbolic _,(Label _|Tag _))
  | (Label _,Tag _)
      -> -1
  | (Symbolic _|Label _|Tag _),Concrete _
  | ((Label _|Tag _),Symbolic _)
  | (Tag _,Label _)
      -> 1

  let pp_location (s,t) = match t with
  | None -> s
  | Some t -> sprintf "%s:%s" s t

  let pp hexa = function
    | Concrete i -> Scalar.pp hexa i
    | Symbolic (s,0) -> pp_location s
    | Symbolic (s,o) -> sprintf "%s+%i" (pp_location s) o
    | Label (p,lbl)  -> sprintf "%i:%s" p lbl
    | Tag None -> "NoTag"
    | Tag (Some s) -> sprintf "'%s" s

  let pp_v = pp false

  let tag_eq = Misc.opt_eq Misc.string_eq

  let location_eq (s1,t1) (s2,t2) = Misc.string_eq s1 s2 && tag_eq t1 t2

  let eq c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2 = 0
  | Symbolic (s1,o1),Symbolic (s2,o2) ->
      location_eq  s1 s2 && Misc.int_eq o1 o2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.string_eq  s1 s2 && Misc.int_eq p1 p2
  | Tag t1,Tag t2 -> tag_eq t1 t2
  | (Concrete _,(Symbolic _|Label _|Tag _))
  | (Symbolic _,(Concrete _|Label _|Tag _))
  | (Label _,(Concrete _|Symbolic _|Tag _))
  | (Tag _,(Concrete _|Symbolic _|Label _))
    -> false

 (* For building code symbols, significant for symbols only ? *)
  let vToName = function
    | Symbolic ((s,None),0) -> s
    | Symbolic _|Concrete _|Label _|Tag _ -> assert false
end
