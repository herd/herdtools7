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
   -threads <n>    maximal number of threads
   -ins <n> maximal number of instruction on a thread
*)

open Printf

module Top
    (Opt:
       sig
         val verbose : int
         val ins : Interval.t
         val threads : Interval.t
       end) =
  struct

    module T = struct
      type t = bool
    end


    module Make(A:ArchBase.S) = struct

      let zyva _name parsed =
        let prog = parsed.MiscParser.prog in
        Interval.inside Opt.threads (List.length prog) &&
        List.for_all
          (fun (_,code) -> Interval.inside Opt.ins (List.length code))
          prog

    end

    module Z = ToolParse.Top(T)(Make)

    let do_test name  =
      try
        let ok = Z.from_file name in
        if ok then begin
          if Opt.verbose >= 0 then Printf.printf "%s\n" name
        end ;
        if Opt.verbose < 0 then exit (if ok then 0 else 1)
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
let parse_inter s =
  try
    LexInterval.parse s
  with LexInterval.Error ->
    raise (Arg.Bad (sprintf "'%s' is not an interval specification" s))
let set_inter x s = x := parse_inter s

let ins = ref Interval.all
let threads = ref Interval.all
let verbose = ref 0
let tests = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mselect"

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> incr verbose), " be verbose";
     "-q",Arg.Unit (fun () -> verbose := -1), " quiet mode, status only";
     "-ins", Arg.String (set_inter ins),
     sprintf "<inter> instruction count, default %s" (Interval.pp !ins);
     "-threads", Arg.String (set_inter threads),
     sprintf "<inter> thread count, default %s" (Interval.pp !threads);]
    (fun s -> tests := s :: !tests)
    (sprintf "Usage: %s [options]* [test]*" prog)

module X = Top
    (struct
      let verbose = !verbose
      let ins = !ins
      let threads = !threads
    end)

let () = X.zyva !tests
