(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Operations on symbolic registers *)

module type Arch = sig
  include ArchBase.S

(* Values and global locations and their creators *)
  type v
  val maybevToV : MiscParser.maybev -> v
  type global
  val maybevToGlobal : MiscParser.maybev -> global

(* Manifest location type *)
  type location = 
    | Location_global of global
    | Location_reg of int * reg

end

module Make(A:Arch) : sig

  type ('loc,'v) t = ('loc,'v, A.pseudo) MiscParser.r3
      
  val allocate_regs :
    (MiscParser.location, MiscParser.maybev) t -> (A.location,A.v) t

end
