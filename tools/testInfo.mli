(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Extract information from test, at the moment name + fname + hash *)

(* Type of information *)
module T : sig
  type t = 
      { tname : string ; fname : string ; hash : string ; } 
  val compare : t -> t -> int
end

(* Extract information out of parsed test *)
module Make(A:ArchBase.S) : sig
  val zyva : Name.t -> A.pseudo MiscParser.t -> T.t
end

(* Parser an extract *)
module Z : sig
  val from_file : string -> T.t
end
