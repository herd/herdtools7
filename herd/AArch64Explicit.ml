(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type nexp =  AF|DB|AFDB|IFetch|Other
type explicit = Exp | NExp of nexp

let pp = function
  | Exp -> "Exp"
  | NExp Other-> "NExp"
  | NExp IFetch-> "IFetch"
  | NExp AF-> "NExpAF"
  | NExp DB-> "NExpDB"
  | NExp AFDB-> "NExpAFDB"

let is_explicit_annot = function
  | Exp -> true
  | NExp _ -> false

and is_not_explicit_annot = function
  | NExp _ -> true
  | Exp -> false
