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

(* Internal keys for mcompare *)

(* A key is a reference for a row, it features the name of
   the test, plus information left abstract *)

type 'a t = { name : string ; info : 'a }


module type Config = sig
  val verbose : int
  val kinds : LogState.kind TblRename.t
  val conds : LogConstr.cond TblRename.t
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
        { cond : LogConstr.cond option ; unsure : bool ;
          kind : LogState.kind; }

    val add : LogState.t -> info t array
    val adds : LogState.t list -> info t array
  end
end
