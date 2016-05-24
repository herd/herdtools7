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

open Printf

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mcmp"

(* Simple outcome comparator, memory efficient *)

module type Opt = sig
  val verbose : int
  val quiet : bool
  val same : bool
  val ok : string -> bool
  val pos : string option
  val neg : string option
end

module Make(O:Opt) = struct

  module LL =
    LexLog.Make
      (struct
        let verbose = O.verbose
        let rename n = n
        let ok = O.ok
        let hexa = false
      end)

  module LS = LogState.Make(O)

  let read_logs fnames = LL.read_names_simple fnames


  let cmp_logs fname t1 t2 = match fname with
  | Some fname ->
      Misc.output_protect
        (fun chan ->
          LS.simple_diff_not_empty
            (fun n _ -> fprintf chan "%s\n" n ; true)
            t1 t2 false) fname
  | None -> false

  let simple_diff pp t1 t2 =
    LS.simple_diff
      (fun n _ -> pp n ; true)
      t1 t2 false

  let simple_same pp1 pp2 t1 t2 =
    LS.simple_same
      (fun n _ -> pp1 n ; true)
      (fun n _ -> pp2 n ; true)
      t1 t2 false

 let run f1 f2 =
    match read_logs [f1;f2;] with
    | [t1;t2] ->
        if O.same then
          simple_same
            (if O.quiet then (fun _ -> ()) else printf "%s: %s\n%!" f1)
            (if O.quiet then (fun _ -> ()) else printf "%s: %s\n%!" f2)
            t1 t2
        else
          let b0 =
            simple_diff
              (if O.quiet then (fun _ -> ()) else printf "%s\n") t1 t2 in
          let b1 = cmp_logs O.pos t1 t2 in
          let b2 = cmp_logs O.neg t2 t1 in
          b0 || b1 || b2
    | _ ->
        Warn.user_error "%s operates on two log files" prog
end

let verbose = ref 0
let select = ref []
let names = ref []
let excl = ref []
let pos = ref None
let neg = ref None
let quiet = ref false
let same = ref false
let options =
  [
   ("-v", Arg.Unit (fun _ -> incr verbose),
    "<non-default> show various diagnostics, repeat to increase verbosity") ;
   ("-q", Arg.Unit (fun _ -> quiet := true; verbose := 0;),
    "<non-default> be quite, no output at all") ;
   ("-same", Arg.Unit (fun _ -> same := true),
    "<non-default> check that logs contain the same tests") ;
   ("-pos",
     Arg.String (fun s -> pos := Some s),
    " <file> dump positive differences, default "^ (match !pos with None -> "don't dump" | Some s -> s));
   ("-neg",
     Arg.String (fun s -> neg := Some s),
    "<file> dump negative differences, default "^ (match !neg with None -> "don't dump" | Some s -> s));
   CheckName.parse_select select;
   CheckName.parse_names names;
   CheckName.parse_excl excl;
 ]
let logs = ref []



let () =
  Arg.parse options
    (fun s -> logs := s :: !logs)
    (sprintf "Usage %s [options]* log1 log2
  - logs are log file names from memevents or litmus
  - options are:" prog)

module Check =
  CheckName.Make
    (struct
      let verbose = !verbose
      let rename = []
      let select = !select
      let names = !names
      let excl = !excl
    end)

module M =
  Make
    (struct
      let verbose = !verbose
      let quiet = !quiet
      let same = !same
      let ok = Check.ok
      let pos = !pos
      let neg = !neg
    end)

let f1,f2 = match !logs with
| [f1;f2;] -> f1,f2
| _ ->
    eprintf "%s takes two arguemts\n" prog ; exit 2

let () =
  let some_diff = M.run f1 f2 in
  if some_diff then
    exit 1
  else
    exit 0
