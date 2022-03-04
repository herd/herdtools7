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

(* Signature of test builder *)
module type S = sig

  include ArchRun.S


  val ppo : (R.relax -> 'a -> 'a) -> 'a -> 'a


  type test
(* Various access to test *)
  val get_nprocs : test -> int
  val get_name : test -> string
  val set_name : test -> string -> test
  val set_scope : test -> BellInfo.scopes -> test
  val add_info : test -> string -> string -> test

  type node = C.node
  type edge = E.edge
  type check = edge list list -> bool

(* Returns resolved edges of test *)
  val extract_edges : test -> edge list

(* Build up test, test structure includes
   name & comment given as first two arguments,
   third argument is the last minute check *)

  val make_test :
      string -> ?com:string -> ?info:Code.info -> ?check:check ->
      ?scope:BellInfo.scopes ->
      edge list -> test

(* Build test from cycle *)
  val test_of_cycle :
      string ->
        ?com:string -> ?info:Code.info -> ?check:check ->
          ?scope:BellInfo.scopes -> ?init:Code.env ->
            edge list -> node -> test

(* Dump the given test *)
  val dump_test_channel : out_channel -> test -> unit

end
