(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

let pgm = if Array.length Sys.argv > 0 then Sys.argv.(0) else "litmus"

(* Local options are in Option module *)
let sources = ref []

open Option

module PStride = ParseTag.Make(Stride)

let opts =
  [
(* General behavior *)
   "-v", Arg.Unit (fun () -> incr verbose), " be verbose";
   "-version", Arg.Unit (fun () -> print_endline Version_litmus.version; exit 0),
   " show version number and exit";
   "-libdir", Arg.Unit (fun () -> print_endline Version_litmus.libdir; exit 0),
   " show installation directory and exit";
   "-switch", Arg.Set Misc.switch, "switch something" ;
   "-o", Arg.String set_tar,
     "<name> cross compilation to directory or tar file <name>" ;
   "-cross",  Arg.String set_cross, "<name> same as -o above, with README and Makefile";
   begin let module P = ParseTag.Make(Crossrun) in
   P.parse "-crossrun" crossrun "run tests on remote machine or simulator" end ;
   argstring "-adbdir" Option.adbdir
    "<dir> target directory on device in mode -crossrun adb";
   "-mach", Arg.String MyName.read_cfg,
   "<name> read configuration file name.cfg";
   "-index",  argstringo Option.index ,
   "<@name> save index of compiled tests in file <@name>" ;
   "-hexa", Arg.Set Option.hexa,
   " hexadecimal output";
   "-no", argstringo Option.no,
   "<name> do not handle tests whose edge cycles are listed in file <name>";

(* Test parameters *)
   "-a", arginto Option.avail,
     "<n> Run maximal number of tests concurrently for n available cores (default, run one test)";
   "-avail", arginto Option.avail, "<n> alias for -a <n>";
   argbool "-limit" Option.limit "run tests with n threads or less";
   argkm "-s" Option.size "size of test" ;
   argkm "-size_of_test" Option.size  "alias for -s";
   argkm "-r" Option.runs "number of runs" ;
   argkm "-number_of_run" Option.runs "alias for -r" ;
   argkm "-noccs" Option.noccs "upper bound on numbers of target outcomes (presi only)" ;
   argfloato "-timelimit" Option.timelimit "bound on runtime (presi only)" ;

(* Modes *)
   begin let module P = ParseTag.MakeS(Variant_litmus) in
   P.parse "-variant" Option.variant "select a variation" end ;
   begin let module P = ParseTag.Make(Barrier) in
   P.parse "-barrier" Option.barrier "set type of barriers" end ;
   "-delay", Arg.Int set_delay,
   begin let get_delay a = Option.get_delay (Option.get_default a) in
   sprintf
     "set timebase delay (default X86=%i, PPC=%i, ARM=%i, C=%i)"
     (get_delay `X86)
     (get_delay `PPC)
     (get_delay `ARM)
     (get_delay `C)
   end ;
   argbool "-vb" Option.verbose_barrier
     "show time of loop synchronisation" ;
   argboolo "-vp" Option.verbose_prelude
     "show some additional information on test as a prelude to test output" ;
   begin let module P = ParseTag.Make(Driver) in
   P.parse "-driver" Option.driver "select language of driver" end ;
   argboolfun "-detached"
     (fun b ->
       Option.threadstyle :=
         (if b then ThreadStyle.Detached else ThreadStyle.Std))
     "used detached POSIX threads" ;
   begin let module P = ParseTag.Make(ThreadStyle) in
   P.parse "-thread" Option.threadstyle "set thread nature" end ;
   begin let module P = ParseTag.Make(Launch) in   
   P.parse "-launch" Option.launch "set type of phread lauch" end ;
   begin let module P = ParseTag.Make(Memory) in
   P.parse "-mem" Option.memory "set memory mode" end ;
   PStride.parse "-st" Option.stride "stride for scanning memory" ;
   PStride.parse "-stride" Option.stride "stride for scanning memory" ;
   begin let module P = ParseTag.Make(Preload) in
   P.parse "-preload" Option.preload "set preload mode" end ;
   begin let module P = ParseTag.Make(Collect) in
   P.parse "-collect" Option.collect "time when results are collected" end ;
   begin let module P = ParseTag.Make(Safer) in
   P.parse "-safer" Option.safer "safer mode" end ;
   begin let module P = ParseTag.Make(Mode) in
   P.parse "-mode" Option.mode "output style for C files" end ;
   begin let module P = ParseTag.Make(UseArch) in
   P.parse "-usearch" Option.usearch "use generated/traditional arch description" end;
   argbool "-cautious" Option.cautious
     "insert additional memory barriers" ;
(* Affinity *)
   begin let module P = ParseTag.Make(Affinity) in
   P.parse "-affinity" Option.affinity
     "attach threads to logical processors" end ;
   "-i",
   Arg.Int (fun i -> Option.affinity := Affinity.Incr i),
   "<n> alias for -affinity incr<n>" ;
   "-procs",
   Arg.String (fun s -> set_logicalprocs s),
   "<int list> specify order of logical processors, implies -affinity incr1" ;
   "-p",Arg.String (fun s -> set_logicalprocs s),
   "<int list> alias for -procs <int list>" ;
   argbool "-force_affinity" Option.force_affinity
   "force affinity setting, useful to circumvent power management";
   begin let module P = ParseTag.Make(Smt) in
   P.parse "-smtmode" Option.smtmode
   "how logical processors from the same core are numbered" end;
   argint "-smt" Option.smt "specify <n>-ways SMT" ;
   argint "-nsockets" Option.nsockets "specify <n> sockets" ;
(* Allocation *)
  begin let module P = ParseTag.Make(Alloc) in
   P.parse"-alloc" Option.alloc
     "allocation mode"  end ;
   argbool "-doublealloc" Option.doublealloc
     "perform a malloc/free once before allocating for real" ;
   argbool "-contiguous" Option.contiguous
   "allocate shared locations as a big chunk of memory" ;
   begin let module P = ParseTag.Make(Align) in
   P.parse_opt "-noalign" Option.noalign
     "specify non alignment"  end ;
(* Premature stop *)
   begin let module P = ParseTag.Make(Speedcheck) in
   P.parse"-speedcheck" Option.speedcheck
     "stop test as soon as condition is settled"  end ;
(* C compiler *)
   "-ccopts", Arg.String set_gccopts,
   begin let get_gccopts a = Option.get_gccopts (Option.get_default a) in
   sprintf
     "<flags> set gcc compilation flags (default X86=\"%s\", PPC=\"%s\", ARM=\"%s\", C=\"%s\")"
     (get_gccopts `X86)
     (get_gccopts `PPC)
     (get_gccopts `ARM)
     (get_gccopts `C)
   end ;
   argstring_withfun "-makevar" (fun arg -> makevar := !makevar @ [arg])
     "<line> add line at beginning of Makefile" ;
   argstring "-gcc" Option.gcc "<name> name of gcc" ;
   argbool "-c11" Option.c11 "enable the C11 standard";
   argbool "-c11_fence" Option.c11_fence "enable the C11 standard";
   argboolo "-stdio" Option.stdio "use/do not use stdio";
   argbool "-ascall" Option.ascall "tested code is in a function";
   argstring "-linkopt" Option.linkopt "<flags> set gcc link option(s)" ;
   "-gas",
   Arg.Bool set_gas,
   "<bool> emit Gnu as extensions (default Linux/Mac=true, AIX=false)" ;

(* Specify target machine *)
   begin let module P = ParseTag.Make(TargetOS) in
   P.parse "-os" Option.targetos "target operating system" end ;
   begin let module P = ParseTag.Make(Word) in
   P.parse_withfun "-ws" set_word
     (let get_word a = Option.get_word (Option.get_default a) in
      sprintf "word_size (default X86=%s, PPC=%s, ARM=%s, C=%s)"
        (Word.pp (get_word `X86))
        (Word.pp (get_word `PPC))
        (Word.pp (get_word `ARM))
        (Word.pp (get_word `C))
     )
     None
   end ;
   begin let module P = ParseTag.Make(Archs.System) in
   P.parse_withfun "-carch"
     Option.set_carch "Target architechture (C arch only)" None end ;
   argbool "-pldw" Option.pldw "use pldw instruction (ARM)" ;
   argbool "-cacheflush" Option.cacheflush "use cache flush instruction (AArch64)" ;
(********)
(* Misc *)
(********)
   argint "-sleep" Option.sleep "sleep n seconds between tests" ;
   argbool "-exit" Option.exit_cond "exit status reflects final condition success or failure" ;
   argkm_withfun "-loop" set_timeloop
     "<n> insert assembly code in a loop of size <n>" ;
   argbool "-kind" Option.kind "show kind information in output" ;
(* Change input *)
   CheckName.parse_names names ;
   CheckName.parse_excl excl ;
(* Change input *)
   CheckName.parse_rename rename ;
   argstring_withfun "-kinds" set_kinds
     "<file> specify kinds of tests (can be repeated)" ;
   argstring_withfun "-conds" set_conds
     "<file> specify conditions of tests (can be repeated)" ;
   "-hints", argstringo Option.hint, "<file> read hints in <file>";
   argstring_withfun "-nstates" set_nstates
   "<file> specify number of states mapping";
(* Bizarre *)
   argbool "-isync" Option.isync "undocumented" ;
   argint "-syncmacro" Option.syncmacro "undocumented" ;
   argbool "-xy" Option.xy "undocumented";
  ]


let usage = sprintf   "Usage: %s [opts]* filename" pgm

let () = Arg.parse opts (fun s -> sources := s :: !sources) usage

let sources = !sources
let rename = !rename
let names = !names
let excl = !excl
let kinds = !Option.kinds
let conds = !Option.conds
let nstates = !Option.nstates
let verbose = !Option.verbose
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
    let module L = LexRename.Make(struct let verbose = verbose end) in
    let kinds =
      L.read_from_files kinds ConstrGen.parse_kind in
    let conds =
      L.read_from_files conds (fun s -> Some s) in
    let nstates =
      L.read_from_files nstates
        (fun s ->
          try Some (int_of_string s)
          with Failure _ -> None) in
    let outname =
      if Option.is_out () then Option.get_tar ()
      else MySys.mktmpdir () in
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
      let check_kind = TblRename.find_value_opt kinds
      let check_cond = TblRename.find_value_opt conds
      let check_nstates = TblRename.find_value_opt nstates
(* Static options *)
      let verbose = verbose
      let index = !index
      let no = !no
      let hint = !hint
      let verbose_prelude = match !verbose_prelude with
      | None ->
        (match !driver with
        | Driver.XCode|Driver.C -> true
        | Driver.Shell -> false)
      | Some b -> b
      let verbose_barrier = !verbose_barrier
      let hexa = !hexa
      let collect = !collect
      let syncmacro = if !syncmacro > 0 then Some !syncmacro else None
      let contiguous = !contiguous
      let noalign = !noalign
      let smt = !smt
      let nsockets = !nsockets
      let smtmode = !smtmode
      let force_affinity = !force_affinity
      let kind = !kind
      let numeric_labels = get_numeric_labels ()
      let syncconst = syncconst
      let morearch = !morearch
      let carch = !carch
      let xy = !xy
      let pldw = !pldw
      let cacheflush = !cacheflush
      let makevar = !makevar
      let gcc = !gcc
      let c11 = !c11
      let c11_fence =
        let b = !c11_fence in
        if b && not c11 then
          Warn.fatal "The use of C11 fence cannot be enabled without C11 enabled (use -c11 true)";
        b
      let stdio = match !stdio with
      | None ->
          begin match !mode with
          | Mode.Std|Mode.Kvm -> true
          | Mode.PreSi -> false
          end
      | Some b -> b
      let ascall = !ascall
      let variant = !variant
      let crossrun = match !mode,!crossrun with
      | Mode.Kvm,Crossrun.Qemu s -> Crossrun.Kvm s
      | Mode.Kvm,_ -> Crossrun.Kvm "./arm-run"
      | (Mode.Std|Mode.PreSi),cr -> cr
      let adbdir = !adbdir
      let driver = !driver
      let exit_cond = !exit_cond
      let sleep = !sleep
      let is_out = is_out ()
      let targetos = !targetos
      let platform = "_linux"
      let affinity = match !mode with
      | Mode.Std -> !affinity
      | Mode.PreSi -> Affinity.Scan
      | Mode.Kvm -> Affinity.No
      let logicalprocs = !logicalprocs
      let linkopt = !linkopt
      let barrier = !barrier
      let threadstyle = !threadstyle
      let launch = !launch
      let alloc = !alloc
      let doublealloc = !doublealloc
      let memory = !memory
      let preload = !preload
      let safer = !safer
      let cautious = !cautious
      let speedcheck = !speedcheck
      let isync = !isync
      let size = !size
      let runs = !runs
      let noccs = !noccs
      let timelimit = !timelimit
      let avail = !avail
      let stride =
        let open Stride in
        let st = !stride in
        match st with
        | No|Adapt -> st
        | St i ->  if i > 0 then st else No
      let timeloop = !timeloop
      let limit = !limit
(* tar stuff *)
      let cross = !Option.cross
      let tarname = Option.get_tar ()
(* Arch dependent *)
      let mkopt = Option.get_dependent ()
(* Mode *)
      let mode = !mode
      let usearch = !usearch
      let asmcomment = !asmcomment
      let asmcommentaslabel = !asmcommentaslabel
    end in
    let module T = Top_litmus.Top (Config) (Tar) in
    T.from_files sources ;
    if not (Option.is_out ()) then MySys.rmdir outname ;
    exit 0
  with
    | LexRename.Error|Misc.Exit -> exit 2
    | Misc.UserError msg ->
        eprintf "User error: %s\n%!" msg ;
        exit 2
    | Misc.Fatal msg ->
        eprintf "Fatal error: %s\n%!" msg ;
        exit 2
