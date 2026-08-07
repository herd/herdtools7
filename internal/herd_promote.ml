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

let litmus = Sys.argv.(Array.length Sys.argv -1)

let _, com, args = Args.split_wrapper_args (Array.to_list Sys.argv)

let () =
  if false then
    Printf.eprintf "%s called with com: %s and args: [%s]\n%!"
      Sys.argv.(0) com (String.concat "; " args)

let () =
  let st = TestHerd.run_herd_args com args litmus in
  let ok = TestHerd.promote litmus st in
  exit (if ok then 0 else 1)
