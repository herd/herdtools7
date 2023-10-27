(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t =
    | LIT_INSTR of string
    | LIT_NOP

let pp = function
    | LIT_INSTR s -> Printf.sprintf "instr:\"%s\"" s
    | LIT_NOP -> "NOP"

let compare i1 i2 =
    match (i1, i2) with
    | (LIT_INSTR(l1), LIT_INSTR(l2)) ->
        String.compare l1 l2
    | (LIT_NOP, LIT_NOP) -> 0
    | (LIT_INSTR(_), LIT_NOP) -> -1
    | (LIT_NOP, LIT_INSTR(_)) -> 1

let eq i1 i2 = compare i1 i2 = 0
