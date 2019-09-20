(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
open MySys

module type CommonConfig = sig
  val verbose : int
  val is_out : bool
  val targetos : TargetOS.t
  val affinity : Affinity.t
  val force_affinity : bool
  val logicalprocs : int list option
  val linkopt : string
  val barrier : Barrier.t
  val launch : Launch.t
  val alloc : Alloc.t
  val memory : Memory.t
  val preload : Preload.t
  val safer : Safer.t
  val speedcheck : Speedcheck.t
  val isync : bool
  val c11 : bool
  include DumpParams.Config
end

module type Config = sig
  include CommonConfig
(* Arch dependant *)
  val gcc : string
  val gccopts : string
  val word : Word.t
  val delay : int
end

module Make(O:Config) =
struct

(* Check dontrun file *)
let file_base doc =
  let base = Filename.basename doc.Name.file in
  Filename.chop_extension base

let open_norun doc chan =
  if O.is_out then fprintf chan "if [ ! -f %s.no ]; then\n" (file_base doc)

and close_norun chan =
  if O.is_out then output_line chan "fi"

(* Text quotation for cross-compilation *)
let open_quote chan =
  if O.is_out then output_line chan "cat <<'EOF'"
and close_quote chan =
  if O.is_out then output_line chan "EOF"

let is_darwin () : bool =
  Sys.unix && Sys.command "sh -c 'test \"$(uname -s)\" = Darwin'" = 0

module W = Warn.Make(O)
let target_os =
  if O.is_out then
    O.targetos
  else begin
    if is_darwin ()
    then begin
      W.warn "OS: Darwin" ;
      TargetOS.Mac
    end else begin
      W.warn "OS: Linux" ;
      TargetOS.Linux
    end
  end

let get_gcc_opts =
  let std = if O.c11 then "gnu11" else "gnu99" in
  let std_opt =
    match O.gcc,std with
    | "clang","gnu99" -> ""
    | _,_ -> sprintf "-std=%s "std in
  let std_opts = sprintf "-Wall %s" std_opt ^  O.gccopts in
  let opts =
    let open TargetOS in
    match target_os with
    | Mac|Android8 -> begin match O.word with
      | Word.W64 -> std_opts ^ " -m64"
      | Word.W32 -> std_opts ^ " -m32"
      | Word.WXX -> std_opts
    end
    | Linux|FreeBsd -> begin match O.word with
      | Word.W64 -> std_opts ^ " -m64 -pthread"
      | Word.W32 -> std_opts ^ " -m32 -pthread"
      | Word.WXX -> std_opts ^ " -pthread"
    end
    | AIX -> begin match O.word with
      | Word.W64 -> std_opts ^ " -maix64 -pthread"
      | Word.W32 -> std_opts ^ " -maix32 -pthread"
      | Word.WXX -> std_opts ^ " -pthread"
    end in

  let opts = match O.affinity with
  | Affinity.No -> opts
  | Affinity.Incr _|Affinity.Random|Affinity.Custom|Affinity.Scan ->
      (if TargetOS.is_freebsd target_os then fun s -> s
      else fun s ->  "-D_GNU_SOURCE " ^ s)
        (if O.force_affinity then "-DFORCE_AFFINITY " ^opts
        else opts ) in
  opts

let get_link_opts =
  let def_opts = "" in
  let add_opts = O.linkopt in
  match def_opts,add_opts with
  | (s,"")|("",s) -> s
  | _,_ -> def_opts ^ " " ^ add_opts

let pp_barrier_loc =
  let b = Barrier.pp O.barrier in
  match  O.barrier with
  | Barrier.TimeBase -> sprintf "%s (%i)" b O.delay
  | _ -> b

let report_parameters out =
  let pf fmt = ksprintf out fmt in
  pf "Revision %s, version %s" Version_litmus.rev Version_litmus.version;
  pf "Command line:%s"
    (String.concat ""
       (List.map (sprintf " %s") (Array.to_list Sys.argv))) ;
  out "Parameters" ;
  let module D = DumpParams.Make(O) in
  D.dump out ;
  pf "/* gcc options: %s */" get_gcc_opts ;
  begin match get_link_opts with
  | "" -> ()
  | s ->   pf "/* gcc link options: %s */" s
  end ;
  pf "/* barrier: %s */" pp_barrier_loc ;
  pf "/* launch: %s */"  (Launch.pp O.launch) ;
  pf "/* affinity: %s */" (Affinity.pp O.affinity) ;
  begin match O.logicalprocs with
  | None -> ()
  | Some xs ->
      pf "/* procs: {%s} */" (LexSplit.pp_ints xs)
  end ;
  pf "/* alloc: %s */" (Alloc.pp O.alloc) ;
  pf "/* memory: %s */" (Memory.pp O.memory) ;
  begin match O.stride with
  | Stride.No -> ()
  | Stride.Adapt | Stride.St _ ->  pf "/* stride: %s */" (Stride.pp O.stride)
  end ;
  pf "/* safer: %s */" (Safer.pp O.safer) ;
  pf "/* preload: %s */" (Preload.pp O.preload) ;
  pf "/* speedcheck: %s */" (Speedcheck.pp O.speedcheck) ;
  begin match O.avail with
  | Some n -> pf "/* proc used: %i */" n
  | None -> ()
  end ;
  if O.isync then begin
    pf "/* isync: true */"
  end ;
  ()

let report_machine chan =
(* Machine information *)
  if not O.is_out then begin
    output_string chan "Machine:"  ;
    exec "hostname" (output_line chan) ;
    begin match target_os with
    | TargetOS.Linux|TargetOS.Android8 -> cat "/proc/cpuinfo" (output_line chan)
    | TargetOS.FreeBsd -> exec "sysctl -a | grep dev.cpu" (output_line chan)
    | TargetOS.Mac ->
        exec "system_profiler SPHardwareDataType" (output_line chan)
    | TargetOS.AIX -> ()
    end
  end ;
(* Parameters of test *)
  open_quote chan ;
  report_parameters (fprintf chan "%s\n") ;
  close_quote chan ;
  ()
end
