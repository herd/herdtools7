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

(* Internal keys for mcompare *)

(* A key is a reference for a row, it features the name of
   the test, plus information left abstract *)

type 'a t = { name : string ; info : 'a }


module type Config = sig
  val verbose : int
  val kinds : LogState.kind TblRename.t
  val conds : LogConstr.constr TblRename.t
 end

(*************************************)
(* All sorts of information for keys *)
(*************************************)

module Make(Opt:Config) : sig

(* None *)
  module None : sig
    type info = unit

    val add_names : string array -> info t array
    val add_log : LogState.t -> info t array
  end

(* kind + loop *)
  module Kind : sig
    type info = { kind : LogState.kind ; loop : bool }

    val add : string array -> LogState.t list -> info t array

    val pps : info t array -> string list array
  end

(* Complete: test of first colum *)
  module Full : sig
    type info = LogState.test

    val add : LogState.t -> info t array

  end

(* Final condition, extracted from kind map (cf. Opt), cond map,
   and first column *)
  module Cond : sig

    type info = 
        { cond : LogConstr.constr option ; unsure : bool ;
          kind : LogState.kind; }

    val add : LogState.t -> info t array
    val adds : LogState.t list -> info t array
  end
end
