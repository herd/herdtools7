(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Signature of test builder *)
module type S = sig

  include ArchRun.S

        
  val ppo : (R.relax -> 'a -> 'a) -> 'a -> 'a


  type test
  type node = C.node
  type edge = E.edge
  type check = edge list list -> bool

(* Returns resolved edges of test *)
  val extract_edges : test -> edge list

(* Build up test, test structure includes
   name & comment given as first two arguments,
   third argument is the last minute check *)

  val make_test :
      string -> ?com:string -> ?info:Code.info -> ?check:check -> edge list -> test
(* Build test from cycle *)
  val test_of_cycle :
      string -> ?com:string -> ?info:Code.info -> ?check:check -> edge list ->
       node -> test  
(* Dump the given test *)
(*  val dump_test : test -> unit *)
  val dump_test_channel : out_channel -> test -> unit

end
