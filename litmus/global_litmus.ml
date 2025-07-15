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

type stage =
  | Stage1
  | Stage2

type level =
  | Lv2
  | Lv3

type t =
  | Addr of string | Pte of string | Phy of string
  | Ttd of { stage: stage; level: level ; s: string}

let string_of_stage = function
  | Stage1 -> "s1"
  | Stage2 -> "s2"

let string_of_level = function
  | Lv2 -> "lv2"
  | Lv3 -> "lv3"

let pp_old = function
  | Addr s -> s
  | Pte s -> Misc.add_pte s
  | Ttd { stage = Stage1; level = Lv3; s } -> Misc.add_pte s
  | Ttd { stage = Stage1; level = Lv2; s } -> Misc.add_pmd s
  | Ttd _ -> assert false
  | Phy s -> Misc.add_physical s

let pp = function
  | Addr s -> s
  | Pte s -> Misc.pp_pte s
  | Ttd { stage; level; s } ->
      begin match level with
      | Lv3 -> Misc.pp_pte s
      | Lv2 -> Misc.pp_ttd (string_of_stage stage) (string_of_level level) s
      end
  | Phy s -> Misc.pp_physical s

let compare g1 g2 = match g1,g2 with
| (Addr s1,Addr s2)
| (Pte s1,Pte s2)
| (Phy s1,Phy s2)
    -> String.compare s1 s2
| (Ttd r1,Ttd r2) -> begin
  match String.compare (string_of_stage r1.stage) (string_of_stage r2.stage) with
  | 0 -> (
      match String.compare (string_of_level r1.level) (string_of_level r2.level) with
      | 0 -> String.compare r1.s r2.s
      | n -> n
    )
  | n -> n
end
| (Addr _,(Pte _|Ttd _ |Phy _))
| (Pte _,(Ttd _ |Phy _))
| Ttd _ , Phy _
  -> -1
| ((Pte _|Phy _|Ttd _),Addr _)
| ((Phy _|Ttd _),Pte _)
| (Phy _,Ttd _)
  -> 1

let as_addr = function
  | Addr s -> s
  | Pte s | Ttd { stage = Stage1; level = Lv3; s }
    -> Printf.sprintf "pte_%s" s
  | Ttd { stage = Stage1; level = Lv2; s } -> Printf.sprintf "pmd_%s" s
  | Ttd _ -> assert false
  | Phy _ -> assert false

let herd2litmus_stage = function
  | Constant.S1 -> Stage1
  | s -> Warn.fatal "litmus does not support stage %s" (Constant.string_of_stage s)

let herd2litmus_level = function
  | Constant.LV2 -> Lv2
  | Constant.LV3 -> Lv3

let tr_symbol =
  let open Constant in
  function
    | Virtual {name=s; tag=None; cap=0L; offset=0; _} -> Addr s
    | Physical (s,0) -> Phy s
    | System (PTE,s) -> Pte s
    | System (TTD {stage; level},s) ->
      Ttd {
        stage = herd2litmus_stage stage;
        level = herd2litmus_level level;
        s; }
    | c ->  Warn.fatal "litmus cannot handle symbol '%s'" (pp_symbol c)

let get_base_symbol =
 let open Constant in
  function
    | Virtual {name=s; tag=None; cap=0L; _ } -> Addr s
    | Physical (s,_) -> Phy s
    | System (PTE,s) -> Pte s
    | System (TTD {stage; level},s) ->
      Ttd {
        stage = herd2litmus_stage stage;
        level = herd2litmus_level level;
        s; }
    | c ->  Warn.fatal "litmus cannot get base of symbol '%s'" (pp_symbol c)

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
