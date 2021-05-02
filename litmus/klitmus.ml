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

(* a simple litmus for kernel *)

open Printf

let pgm = if Array.length Sys.argv > 0 then Sys.argv.(0) else "klitmus"

(* Local options are in Option module *)
let sources = ref []

module KOption : sig
(* Generic setings *)
  type arg_triple =  string * Arg.spec * string

  val argbool :  string -> bool ref -> string -> arg_triple
  val argint :  string -> int ref -> string -> arg_triple
  val arginto : int option ref -> Arg.spec
  val argkm : string -> int ref -> string -> arg_triple

(* Complex settings *)
  val set_tar : string -> unit
  val get_tar : unit -> string
  val is_out : unit -> bool

(* Direct options *)
  val verbose : int ref
  val hexa : bool ref
  val avail : int option ref
  val size : int ref
  val runs : int ref
  val stride : KStride.t ref
  val names : string list ref
  val excl : string list ref
  val rename : string list ref
  val rcu : Rcu.t ref
  val expedited : bool ref
  val pad : int ref
  val barrier : KBarrier.t ref
  val affinity : KAffinity.t ref
  val ccopts : string list ref
  val sharelocks : int option ref
  val delay : int ref

  val carch : Archs.System.t option ref
  val set_carch : Archs.System.t -> unit
end = struct
  include Option
  let stride = ref (KStride.St 1)
  let rcu = ref Rcu.No
  let expedited = ref true
  let pad = ref 3
  let barrier = ref KBarrier.User
  let affinity = ref KAffinity.No
  let ccopts = ref []
  let sharelocks = ref None
  let delay = ref 256
  let carch = ref (Some `X86_64)
end

open KOption

module PStride = ParseTag.Make(KStride)

let opts =
  [
(* General behavior *)
   "-v", Arg.Unit (fun () -> incr verbose), " be verbose";
   "-version", Arg.Unit (fun () -> print_endline Version.version; exit 0),
   " show version number and exit";
   "-libdir", Arg.Unit (fun () -> print_endline !Option.libdir; exit 0),
   " show installation directory and exit";
   "-set-libdir", Arg.String (fun s -> Option.libdir := s),
   "<path> set installation directory to <path>";
   "-o", Arg.String set_tar,
     "<name> cross compilation to directory or tar file <name>" ;
   "-hexa", Arg.Set KOption.hexa,
   " hexadecimal output";
   argint "-pad" KOption.pad "size of padding for C litmus source names";
(* Test parameters *)
   "-a", arginto KOption.avail,
     "<n> Run maximal number of tests concurrently for n available cores (default, run one test)";
   "-avail", arginto KOption.avail, "<n> alias for -a <n>";
   argkm "-s" KOption.size "size of test" ;
   argkm "-size_of_test" KOption.size  "alias for -s";
   argkm "-r" KOption.runs "number of runs" ;
   argkm "-number_of_run" KOption.runs "alias for -r" ;
   PStride.parse "-st" KOption.stride "stride for scanning memory" ;
   PStride.parse "-stride" KOption.stride "stride for scanning memory" ;
   begin let module P = ParseTag.Make(KBarrier)  in
   P.parse "-barrier" KOption.barrier "synchronisation barrier style" end;
   "-delay", Arg.Int (fun i -> KOption.delay := i),
   sprintf
     "set timebase delay (default %i)" !KOption.delay;
(* number if shared spinlocks and srcu_struct *)
   "-share_locks", arginto KOption.sharelocks,
     "<n> number of spinlock_t's and srcu)_structs to share between test instances (default, do not share)";
(* Affinity *)
   begin let module P = ParseTag.Make(KAffinity) in
   P.parse "-affinity" KOption.affinity
     "attach threads to logical processors" end ;
   "-i",
   Arg.Int
     (fun i ->
       let i = if i >=0 then i else 0 in
       KOption.affinity := KAffinity.Incr i),
   "<n> alias for -affinity incr<n>" ;
   begin let module P = ParseTag.Make(Archs.System) in
   P.parse_withfun "-carch"
     KOption.set_carch "Target architechture (C arch only)" None end ;
(********)
(* Misc *)
(********)
(* Compilation options *)
   "-ccopts", Arg.String (fun s -> KOption.ccopts := !KOption.ccopts @ [s]),
   "<string> Additional option for C compiler";
(* Change input *)
   CheckName.parse_names names ;
   CheckName.parse_excl excl ;
   CheckName.parse_rename rename ;
   begin let module P = ParseTag.Make(Rcu) in
   P.parse "-rcu" KOption.rcu "accept RCU tests or not" end ;
   argbool "-expedited" KOption.expedited "translate syncronize_rcu to synchronize_expedited";
 ]


let usage = sprintf   "Usage: %s [opts]* filename" pgm

let () = Arg.parse opts (fun s -> sources := s :: !sources) usage

let sources = !sources
let rename = !rename
let names = !names
let excl = !excl
let verbose = !KOption.verbose
let () =
  try
(* Time to read kind files.. *)
    let module Check =
      CheckName.Make
        (struct
          let verbose = verbose
          let rename = rename
          let select = []
          let names = names
          let excl = excl
        end) in
    let outname =
      if KOption.is_out () then KOption.get_tar ()
      else begin
        Warn.user_error "%s, option -o <name> is mandatory" pgm
      end in
    let module Tar =
      Tar.Make
        (struct
          let verbose = verbose
          let outname = Some outname
        end) in
    let module Config = struct
(* Parser *)
      let check_name = Check.ok
      let check_rename = Check.rename_opt
      let check_kind _ = None
      let check_cond _ = None
(* Static options *)
      let verbose = verbose
      let hexa = !hexa
      let is_out = is_out ()
      let size = !size
      let runs = !runs
      let avail = !avail
      let stride =
        let open KStride in
        let st = !stride in
        match st with
        | Adapt -> st
        | St i ->  if i > 0 then st else St 1
      let barrier = !barrier
      let affinity = !affinity
      let rcu = !rcu
      let expedited = !expedited
      let pad = !pad
      let ccopts = !ccopts
      let sharelocks = !sharelocks
      let delay = !delay
      let sysarch = Misc.as_some !carch
(* tar stuff *)
      let tarname = KOption.get_tar ()
    end in
    let module T = Top_klitmus.Top(Config) (Tar) in
    T.from_files sources ;
    if not (KOption.is_out ()) then MySys.rmdir outname ;
    exit 0
  with
    | LexRename.Error|Misc.Exit -> exit 2
    | Misc.UserError msg ->
        eprintf "User error: %s\n%!" msg ;
        exit 2
    | Misc.Fatal msg ->
        eprintf "Fatal error: %s\n%!" msg ;
        exit 2
