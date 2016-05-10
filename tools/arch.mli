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

(* We need this to apply symbolic register renaming (used in mprog) *)

module type S = sig
  include ArchBase.S

  module RegSet : MySet.S with type elt = reg
  module ProcMap : MyMap.S with type key = int

  type v = Constant.v
  val one : v
  val maybevToV  : v -> v
  val pp_v : v -> string

  type global = Constant.v
  val maybevToGlobal  : global -> v

  include Location.S with type loc_reg = reg and type loc_global = global
(*
  type location = 
    | Location_global of global
    | Location_reg of int * reg

  val pp_location : location -> string
  val pp_rval : location -> string
*)
  type test = (location,v,pseudo) MiscParser.r3
  type prop = (location,v) ConstrGen.prop
  type constr = prop ConstrGen.constr

end
