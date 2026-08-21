(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A tool that runs herd and promotes its output as reference *)

let Args.{args; com; wrapped} = Args.split_wrapper_args Sys.argv

let get_litmus = function
  | Some litmus -> litmus
  | None ->
      Printf.eprintf "%s: Could not find litmus among arguments: [%s]\n%!"
        Sys.argv.(0) (String.concat "; " args) ;
      exit 1

let litmus =
  let rec gather_args litmus = function
    | [] -> get_litmus litmus
    | arg :: args when String.ends_with ~suffix:".litmus" arg ->
        gather_args (Some arg) args
    | _ :: args ->
        gather_args litmus args
  in
  gather_args None args

let () =
  if false then
    Printf.eprintf "%s called with com: %s and args: [%s]\n%!"
      Sys.argv.(0) com (String.concat "; " wrapped)

let () =
  let st = TestHerd.run_herd_args com wrapped litmus in
  let ok = TestHerd.promote litmus st in
  exit (if ok then 0 else 1)
