(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

(* Simple outcome comparator, memory efficient *)

module type Opt = sig
  val verbose : int
  val quiet : bool
  val name_ok : string -> bool
  val pos : string option
  val neg : string option
end

module Make(O:Opt) = struct

  module LL =
    LexLog.Make
      (struct
        let verbose = O.verbose
        let rename n = n
        let ok = O.name_ok
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

  let run f1 f2 =
    match read_logs [f1;f2;] with
    | [t1;t2] ->
        let b0 =
          simple_diff
            (if O.quiet then (fun _ -> ()) else printf "%s\n") t1 t2 in
        let b1 = cmp_logs O.pos t1 t2 in
        let b2 = cmp_logs O.neg t2 t1 in
        b0 || b1 || b2
    | _ -> assert false
end

let verbose = ref 0
let select = ref []
let names = ref []
let pos = ref None
let neg = ref None
let quiet = ref false

let options =
  [
   ("-v", Arg.Unit (fun _ -> incr verbose),
    "<non-default> show various diagnostics, repeat to increase verbosity") ;
   ("-q", Arg.Unit (fun _ -> quiet := true; verbose := 0;),
    "<non-default> be quite, no output at all") ;
   ("-pos",
     Arg.String (fun s -> pos := Some s),
    " <file> dump positive differences, default "^ (match !pos with None -> "don't dump" | Some s -> s));
   ("-neg",
     Arg.String (fun s -> neg := Some s),
    "<file> dump negative differences, default "^ (match !neg with None -> "don't dump" | Some s -> s));
   ("-names",
    Arg.String
      (fun s -> names := s :: !names),
    "<name> specify  selected name file, can be repeated") ;
   ("-select",
    Arg.String
      (fun s -> select := s :: !select),
    "<name> specify selected test file (or index file), can be repeated") ;       ]

let logs = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mcmp"


let () =
  Arg.parse options
    (fun s -> logs := s :: !logs)
    (sprintf "Usage %s [options]* log1 log2
  - logs are log file names from memevents or litmus
  - options are:" prog)


  let names1 = match !names with
  | [] -> None
  | args ->
      Some
        (List.fold_left
           (fun r name -> ReadNames.from_file name StringSet.add r)
           StringSet.empty args)

  let names2 = match !select with
  | [] -> None
  | args ->
      Some
        (StringSet.of_list
           (Names.from_fnames (Misc.expand_argv args)))

  let names = match names1,names2 with
  | (None,ns)|(ns,None) -> ns
  | Some ns1,Some ns2 -> Some (StringSet.union ns1 ns2)

module M =
  Make
    (struct
      let verbose = !verbose
      let quiet = !quiet
      let name_ok = match names with
      | None -> fun _ -> true
      | Some ns -> (fun n -> StringSet.mem n ns)
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
