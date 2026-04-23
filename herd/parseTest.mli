(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Run a test from source file, dispatch on tests architecture *)

module Top :
  functor (C : sig
    include RunTest.Config
    val collect_graph_data : bool
    (** Whether callers intend to consume execution graph data.
        Used as optimization. *)
  end) ->
  sig
    val from_string : filename:string option -> contents:string -> TestHash.env -> TestHash.env * (module RunTest.Outcome) option
    val from_file : string -> TestHash.env -> TestHash.env * (module RunTest.Outcome) option
  end
