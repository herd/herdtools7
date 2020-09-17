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

type t = Addr of string | Pte of string | Phy of string

let pp = function
  | Addr s -> s
  | Pte s -> Misc.add_pte s
  | Phy s -> Misc.add_physical s

let compare g1 g2 = match g1,g2 with
| (Addr s1,Addr s2)
| (Pte s1,Pte s2)
| (Phy s1,Phy s2)
    -> String.compare s1 s2
| (Addr _,(Pte _|Phy _))
| (Pte _,Phy _)
  -> -1
| ((Pte _|Phy _),Addr _)
| (Phy _,Pte _)
  -> 1

let as_addr = function
  | Addr s -> s
  | Pte s -> Printf.sprintf "pte_%s" s
  | Phy _ -> assert false

let tr_symbol =
  let open Constant in
  function
    | Virtual ((s,None),0) -> Addr s
    | Physical (s,0) -> Phy s
    | System (PTE,s) -> Pte s
    | c ->  Warn.fatal "litmus cannot handle symbol '%s'" (pp_symbol c)

type u = t

module Ordered = struct
  type t = u
  let compare = compare
end

module Set = MySet.Make(Ordered)
module Map = MyMap.Make(Ordered)
