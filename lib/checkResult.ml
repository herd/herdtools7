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

let is_valid = function
  | Valid -> true
  | Invalid _ -> false

let rev_append_invalid failures result =
  let previous = match result with
    | Valid -> []
    | Invalid previous -> previous in
  match failures with
  | [] -> result
  | _ -> Invalid (List.rev_append failures previous)
