(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module type Config = sig
(* Names *)
  val check_name : string -> bool
  val check_rename : string -> string option
(* Parameters *)
  val verbose : int
  val hexa : bool
  val is_out : bool
  val size : int
  val runs : int
  val stride : Stride.t
  val tarname : string
end

module Make(O:Config)(Tar:Tar.S) = struct

  let from_chan fname chan =
    let base =
      Filename.chop_suffix (Filename.basename fname) ".litmus" in
    sprintf "%s.o" base

  let from_file fname (srcs,hash_env as k) =
      try
        let src = Misc.input_protect (from_chan fname) fname in
        src::srcs,hash_env
      with
      | Misc.Exit -> k
      | Misc.Fatal msg
      | Misc.UserError msg ->
          eprintf "%a %s\n%!" Pos.pp_pos0 fname msg ;
          k
      | e -> 
          let msg = sprintf "exception %s"  (Printexc.to_string e) in
          eprintf "%a %s\n%!" Pos.pp_pos0 fname msg ;
          assert false
            
  let from_files args =
    let sources,_ = Misc.fold_argv from_file args ([],StringMap.empty)in
    List.iter (eprintf "%s\n") sources
end
