(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* We need this to apply symbolic register renaming (used in mprog) *)

module type S = sig
  include ArchBase.S

  type v = Constant.v
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
  type constr = (location,v) ConstrGen.prop ConstrGen.constr

end
