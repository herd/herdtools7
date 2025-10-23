(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Describes behaviour of tools as regards test hashes *)

(*
 * Test hashes come from two sources:
 *  + Computation from test
 *  + Meta-data Hash=...
 *
 * Possible behaviours are as follows:
 *  + Standard, always include hash in test structure,
 *    meta data have precedence over computation.
 *    Default mode of all tools except `mprog`.
 *    Commanded by option `-set-hash true` of `mprog`.
 *  + NoOp, include hash in test structure when
 *    from meta data, otherwise do not include hash.
 *    That is, do not change anything.
 *    Default mode for `mprog`.
 *  + Rehash, always recompute and include hash.
 *    Option of `-rehash true` of `mshowhashes`.
 *)

type t =
  | Std
  | NoOp
  | Rehash
