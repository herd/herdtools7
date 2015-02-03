(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Answer of a test compilation or run *)

type info = { filename : string ;  hash : string ; }

type answer =
  | Completed of
      Archs.t * Name.t (* Test arch and name *)
        * string (* C source file of test *)
        * StringSet.t (* cycles *)
        * info StringMap.t (* name -> hash *)
  | Interrupted of Archs.t * exn
  | Absent of Archs.t
