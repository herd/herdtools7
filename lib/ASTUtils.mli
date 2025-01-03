(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

val is_var : AST.exp -> bool
val as_var : AST.exp -> (TxtLoc.t * AST.var) option
val as_vars : AST.exp list -> (TxtLoc.t * AST.var) list option

(* Get location of an expression *)
val exp2loc : AST.exp -> TxtLoc.t
val ins2loc : AST.ins -> TxtLoc.t

(* Pre-condition expression for miaou:
 *   + Flatten associative operations
 *   + Change [_] into "id"
 *)
val flatten : AST.exp -> AST.exp

(* Get free variables *)
val free_body : AST.var option list -> AST.exp -> AST.varset
