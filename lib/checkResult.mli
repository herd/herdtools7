(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Result of running a check. Invalid results also record witnesses. For
    example, a cycle as a list of nodes for a failed "acyclic" check. *)

type 'event nodes =
  | Events of 'event list
  | Classes of 'event list list

type 'event node =
  | Event of 'event
  | Class of 'event list

type 'event nonempty =
  | Member of 'event node
  | Edge of 'event node * 'event node

type 'event witness =
  | Cycle of 'event nodes
  | Reflexive of 'event node
  | Nonempty of 'event nonempty

type 'event failed_assertion =
  | FailedYes of AST.do_test * 'event witness
  | FailedNo of AST.do_test

type 'event failure = {
  name : string option;
  source : TxtLoc.t;
  check_statement : AST.pos;
  assertion : 'event failed_assertion;
}

type 'event t = Valid | Invalid of 'event failure list

val is_valid : 'event t -> bool

val rev_append_invalid : 'event failure list -> 'event t -> 'event t
(** Add failures. If not empty, Valid becomes Invalid and Invalid accumulates
    new failures in reverse order.

    Using List.rev to recover the report order so that the first failure is
    consistent between -through none (with at most one failure) and -through
    {invalid,all} (with potentially more than one failures) *)

