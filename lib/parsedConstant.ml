(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type v = (string,ParsedPteVal.t) Constant.t

let zero = Constant.Concrete "0"
and one = Constant.Concrete "1"
and intToV i = Constant.Concrete (string_of_int i)

let no_comp _ _ = assert false

(* Comparison is used by locations, which should contain symbols only,
   It fails on scalars *)
let compare c1 c2 = Constant.compare no_comp no_comp c1 c2
and eq c1 c2 = Constant.eq no_comp no_comp c1 c2

let nameToV = Constant.mk_sym

let pp_v v = Constant.pp Misc.identity ParsedPteVal.pp v
let pp_v_old v = Constant.pp_old Misc.identity ParsedPteVal.pp v

(* Hexa parameter ignored... *)
let pp _hexa = pp_v
let pp_norm _hexa pp_pteval =  Constant.pp Misc.identity pp_pteval
