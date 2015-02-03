(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Dump a test *)

module Make(S: Sem.Semantics) : sig
  type test =
     (S.state, (int * S.A.pseudo list) list, S.constr, S.location)
        MiscParser.result
  val dump : out_channel -> Name.t -> test -> unit
  val lines : Name.t -> test -> string list
end = struct
  type test =
      (S.state, (int * S.A.pseudo list) list, S.constr, S.location)
        MiscParser.result
  include SimpleDumper.Make
      (struct
        module A = S.A

        type state = S.state
        let dump_state = S.A.dump_state
            
        type constr = S.constr
        let dump_constr = S.C.constraints_to_string

        type location = S.location
        let dump_location loc = S.A.dump_location loc
      end)
end
