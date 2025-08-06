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

type t = Addr of string | Pte of string | Phy of string | AddrT of string * int | Tag of string * int

let pp_old = function
  | Addr s -> s
  | Pte s -> Misc.add_pte s
  | Phy s -> Misc.add_physical s
  | _ -> assert false

let pp = function
  | Addr s -> s
  | Pte s -> Misc.pp_pte s
  | Phy s -> Misc.pp_physical s
  | AddrT (s,t) -> Misc.pp_tagged s t
  | Tag (s,_) -> Printf.sprintf "tag(%s)" s

let compare g1 g2 = match g1,g2 with
| (Addr s1,Addr s2)
| (Pte s1,Pte s2)
| (Phy s1,Phy s2)
| (Addr s1, AddrT (s2,_))
| (AddrT (s1,_), Addr s2)
| (AddrT (s1,_),AddrT (s2,_))
    -> String.compare s1 s2
| (Tag (s1,t1),Tag (s2,t2))
    -> String.compare (Misc.pp_tagged s1 t1) (Misc.pp_tagged s2 t2)
| ((Addr _|AddrT _|Tag _) ,(Pte _|Phy _))
| (Pte _,Phy _)
| ((Addr _|AddrT _), Tag _)
  -> -1
| ((Pte _|Phy _),(Addr _|AddrT _|Tag _))
| (Tag _, (Addr _|AddrT _))
| (Phy _,Pte _)
  -> 1

let as_addr = function
  | Addr s -> s
  | Pte s -> Printf.sprintf "pte_%s" s
  | Tag (s,_) -> Printf.sprintf "tag_%s" s
  | Phy _ | AddrT _ -> assert false


let tr_symbol = function
  | Constant.Physical (s,0) -> Phy s
  | Constant.System (Constant.PTE,s) -> Pte s
  | Constant.TagAddr (Constant.VIR,s,t) -> Tag (s,t)
  | Constant.Virtual {Constant.name=s;
                      Constant.tag=None;
                      Constant.cap=0L;
                      Constant.offset=0;} -> Addr s
  | Constant.Virtual {Constant.name=s;
                      Constant.tag=Some(t);
                      Constant.cap=0L;
                      Constant.offset=0;} -> AddrT (s,Misc.int_of_tag t)
  | c ->  Warn.fatal "litmus cannot handle symbol '%s'" (Constant.pp_symbol c)

type u = t

module Ordered = struct
  type t = u
  let compare = compare
end

module Set = MySet.Make(Ordered)
module Map = MyMap.Make(Ordered)

(* Dislayed globals, including arrays cells *)

type displayed = string ConstrGen.rloc

let displayed_compare = ConstrGen.compare_rloc String.compare

let dump_displayed = ConstrGen.dump_rloc Misc.identity

module DisplayedSet =
  MySet.Make
    (struct type t = displayed let compare = displayed_compare end)
