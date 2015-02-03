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

(** Common signature of PPCMem, X86Mem, ARMMem, for abstract usage *)

module type S = sig
  val model : Model.t
  module S : Sem.Semantics

  val check_event_structure :
      S.test -> S.concrete ->
	(S.concrete ->  S.state -> S.rel_pp Lazy.t -> int option (* number of failed requires clauses, None in machine models *) -> 'a -> 'a) ->
              'a -> 'a

end
