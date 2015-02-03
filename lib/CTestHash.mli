(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(**********************************)
(* Digest for litmus-style C code *)
(**********************************)

module type Input = sig
  type code

  val dump_prog : code -> string list        
end

module Make(P:Input) : sig
  val digest : MiscParser.state -> P.code list -> MiscParser.LocSet.t -> string
end
