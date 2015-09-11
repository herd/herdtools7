(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Select tests according to various criteria
   -maxins <n> maximal number of instruction on a thread
*)

open Printf

module Top
    (Opt:
       sig
         val verbose : int
         val maxins : int
       end) =
  struct

    module T = struct
      type t = bool
    end


    module Make(A:ArchBase.S) = struct

      let zyva _name parsed =
        List.for_all
          (fun (_,code) -> List.length code <= Opt.maxins)
          parsed.MiscParser.prog

    end

    module Z = ToolParse.Top(T)(Make)

    let do_test name  =
      try
        let ok = Z.from_file name in
        if ok then Printf.printf "%s\n" name
      with
      | Misc.Exit -> ()
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    let zyva tests = Misc.iter_argv do_test tests

  end


(* Go *)
let maxins = ref 3
let verbose = ref 0
let tests = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mselect"

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> incr verbose), " be verbose";
     "-ins", Arg.Int (fun x -> maxins := x),
     Printf.sprintf "maximal instruction count, default %i" !maxins;]
    (fun s -> tests := s :: !tests)
    (sprintf "Usage: %s [options]* [test]*" prog)

module X = Top
    (struct
      let verbose = !verbose
      let maxins = !maxins
    end)

let () = X.zyva !tests
