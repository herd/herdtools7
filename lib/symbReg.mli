(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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
    | Location_deref of global * int
    | Location_reg of int * reg
    | Location_pte of global
end

module Make(A:Arch) : sig

  type ('loc,'v) t = ('loc,'v, A.pseudo) MiscParser.r3
      
  val allocate_regs :
    (MiscParser.location, MiscParser.maybev) t -> (A.location,A.v) t

end
