(*********************************************************************)
(*                          DIY                                      *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module Make :
    functor (O : sig val hexa : bool end) ->
      functor (A:ArchBase.S) -> Arch.S
with type instruction = A.instruction
and type reg = A.reg
and type pseudo = A.pseudo
