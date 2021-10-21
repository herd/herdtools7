(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
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

type t =
    | LIT_B of Proc.t * string
    | LIT_NOP

let pp = function
    | LIT_B(proc,label) -> Printf.sprintf "B %d:%s" proc label
    | LIT_NOP -> "NOP"

let compare i1 i2 =
    match (i1, i2) with
    | (LIT_B(p1,l1), LIT_B(p2,l2)) ->
        Misc.pair_compare Int.compare String.compare (p1,l1) (p2, l2)
    | (LIT_NOP, LIT_NOP) -> 0
    | (LIT_B(_,_), LIT_NOP) -> -1
    | (LIT_NOP, LIT_B(_,_)) -> 1

(* let from_string = function
    | "NOP" -> LIT_NOP
    | _ -> Warn.fatal "FIXME: unreachable code reached in instrLit.ml" *)