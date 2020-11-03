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

(** Unit-testing utilities. *)

exception AssertionFailure of string

let run_test (name, test) =
  try
    test () ;
    true
  with
  | AssertionFailure msg ->
      Printf.printf "Failed: %s: %s\n" name msg ;
      false
  | e ->
      Printf.printf "Failed %s: raised exception\n" name ;
      raise e

let run tests =
  let results = List.map run_test tests in
  let failed r = not r in
  if List.exists failed results then
    exit 1

let fail msg =
    raise (AssertionFailure msg)
