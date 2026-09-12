(****************************************************************************)
(*                           The Diy Toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let parse_verbose verbose =
  [
    "-q", Arg.Unit (fun _ -> verbose := -1),"<non-default> be silent";
    "-v", Arg.Unit (fun _ -> incr verbose),
    "<non-default> show various diagnostics, repeat to increase verbosity";
  ]

let parse_dest dest =
  "-o", Arg.String (fun s -> dest := Some s),
  "<name> output to directory or tar file <name>"

