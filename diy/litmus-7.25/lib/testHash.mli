(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Generate hash from a litmus test *)

(**************)
(* Digest Env *)
(**************)

type hinfo = { hash : string ; filename : string; }

type env = hinfo StringMap.t

exception Seen

val check_env : env -> string -> string -> string -> env

(*******************)
(* Compute digests *)
(*******************)

(* Digest of init (shared with C digests) *)

val digest_init :
  (string -> string -> unit) (* debug *) -> MiscParser.state -> string

module Make :
  functor (A:ArchBase.S) -> sig
    type init = MiscParser.state
    type prog = (int * A.pseudo list) list
    type locations = MiscParser.LocSet.t

    val refresh_labels : string -> prog -> prog
    val digest : init -> prog -> locations -> string
  end    
