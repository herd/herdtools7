(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Debug tags *)

type t = {
  solver : bool ;
  lexer : bool ;
  top : bool ;
  mem : bool ;
  monad : bool ;
  barrier : bool ;
  res : bool ;
  rfm : bool  ;
  pretty : bool ;
  mixed : bool ;
  files : bool ;
  timeout : bool ;
  profile_cat: bool ;
  exc : bool ;  }

val none : t
val tags : string list
val parse : t -> string -> t option
