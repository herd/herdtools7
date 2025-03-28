(****************************************************************************)
(*                           The diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t = string option * bool option * string

let compare (lhs_label, lhs_bool, lhs_var) (rhs_label, rhs_bool, rhs_var) = 
    let label = 
        Option.compare String.compare lhs_label rhs_label in
    let boolean =
        Option.compare Bool.compare lhs_bool rhs_bool in
    match label, boolean with
    | 0, 0 -> String.compare lhs_var rhs_var
    | 0, boolean -> boolean
    | label, _ -> label
