(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Show test hashes *)

open Printf

module Top
    (Opt:
       sig
         val verbose : int
         val ok : string -> bool
         val recompute_hash : bool
       end) =
  struct

    module Config = struct
      include ToolParse.DefaultConfig
      let verbose = Opt.verbose
      let hash =
        let open HashInfo in
        if Opt.recompute_hash then Rehash else Std
    end
    module TI = TestInfo.Top(Config)

    let do_test name =
      try
        let t = TI.Z.from_file name in
        let tname =  t.TestInfo.T.tname in
        if Opt.ok tname then
          printf "%s %s\n" tname t.TestInfo.T.hash
      with
      | Misc.Fatal msg|Misc.UserError msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          ()
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    let zyva tests = Misc.iter_argv_or_stdin do_test tests

  end


let verbose = ref 0
let recompute_hash = ref false
let arg = ref []
let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mshowhash"

open OptNames

let () =
  Arg.parse
    (LibOpts.parse_verbose verbose
     @ parse_noselect
     @ ["-rehash", Arg.Bool (fun b -> recompute_hash := b),
     sprintf
       " recompute test hashes, regardless of explicit hashes presence, default %b" !recompute_hash;])
    (fun s -> arg := s :: !arg)
    (sprintf "Usage: %s [options]* [test]*" prog)


module Check =
  CheckName.Make
    (struct
      let verbose = !verbose
      let rename = []
      let select = []
      let names = !names
      let oknames = !oknames
      let excl = !excl
      let nonames = !nonames
    end)


let tests = List.rev !arg

module X =
  Top
    (struct
      let verbose = !verbose
      let ok = Check.ok
      let recompute_hash = !recompute_hash
    end)

let () = X.zyva tests
