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

module type Config = sig
  include Top_gen.Config
  val family : string option
  val canonical_only : bool
  val fmt : int
  val no : string list
  val tarfile : string option
  val sufname : string option
  val addnum : bool
  val numeric : bool
  val lowercase : bool
  val overload : int option
  val cpp : bool
  val scope : Scope.t
  val info : MiscParser.info
  val stdout: bool
end

module Make(Config:Config) (T:Builder.S) : sig

  type edge = T.edge
  type check = edge list list -> bool
  type info = (string * string) list

(* Compute information *)
  type mk_info = edge list -> info * T.R.Set.t
  val no_info : mk_info

(* Compute name *)
  type mk_name =  edge list -> string option
  val no_name : mk_name

(* Compute scope *)
  type mk_scope = edge list -> BellInfo.scopes option
  val no_scope : mk_scope

(* Remains abstract: for dumper internal usage *)
  type t

(* Type of cycle generator *)
  type generator =
      (edge list -> mk_info -> mk_name ->  mk_scope -> t -> t) -> t -> t


(* Combine generator and dumper: so as to dump all test
   from generated cycles, check is the "last minute check"
   that operates on cycles splitted by proc *)
  val all : ?check:T.check -> generator -> unit


end
