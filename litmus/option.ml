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

type arg_triple =  string * Arg.spec * string

(* Helpers *)

let parse_km opt s =  match Misc.string_of_intkm s with
| Some x -> x
| None ->
    raise
      (Arg.Bad
         (sprintf
            "wrong argument '%s'; option '%s' expects an integer argument, possibly suffixed by k or m" s opt))

let argkm opt r usage =
  opt,Arg.String (fun s -> r := parse_km opt s),
  sprintf "<n[kKmM]?> %s, default %i" usage !r

let argkm_withfun opt set usage =
  opt,Arg.String (fun s -> set (parse_km opt s)),
  sprintf "<n[kKmM]?> %s" usage

let argstringo r = Arg.String (fun s -> r := Some s)

let arginto r = Arg.Int (fun s -> r := Some s)

let argstring_withfun opt f msg = opt,Arg.String f,msg

let argstring opt r msg =
  opt,Arg.String (fun b -> r := b),
  if String.length msg > 0 && msg.[0] = '<' then
    sprintf "%s, default %s" msg !r
  else
    sprintf "<s> %s, default %s" msg !r

let argint opt  r msg =
  opt,Arg.Int (fun b -> r := b),
  sprintf "<n> %s, default %i" msg !r

let argboolfun opt f msg =
  opt,Arg.Bool f,sprintf "<bool> %s" msg

let argbool opt  r msg =
  opt,Arg.Bool (fun b -> r := b),
  sprintf "<bool> %s, default %b" msg !r

let argboolo opt  r msg =
  opt,Arg.Bool (fun b -> r := Some b),
  sprintf "<bool> %s" msg

let argfloato opt r msg =
  opt,Arg.Float (fun f -> r := Some f),
  sprintf "<float> %s" msg

(* verbose *)
let verbose = ref 0

(* Special *)
let tar = ref None
let cross = ref false

let set_tar b  = cross := false ; tar := Some b
let set_cross b  = cross := true ; tar := Some b
let is_out () = match !tar with
| Some _ -> true
| None -> false
and get_tar () = match !tar with
| Some s -> s
| None -> "**useless**"

let logicalprocs = ref None

let set_logicalprocs s =
  try
    logicalprocs := Some (LexSplit.ints s) ;
  with LexSplit.Error ->
    raise (Arg.Bad ("bad logical processors mapping: " ^s))

(* Direct references *)
let crossrun = ref Crossrun.No
let adbdir = ref "/data/local/tmp"
let index = ref None
let hexa = ref false
let limit = ref true
let no = ref None
let hint = ref None
let avail = ref None
let size = ref 100000
let runs = ref 10
let noccs = ref 1
let timelimit = ref None
let barrier = ref Barrier.User
let verbose_barrier = ref false
let verbose_prelude = ref None
let driver = ref Driver.Shell
let threadstyle = ref ThreadStyle.Std
let launch = ref Launch.Changing
let memory = ref Memory.Direct
let contiguous = ref false
let stride = ref Stride.No
let preload = ref Preload.RandomPL
let collect = ref Collect.After
let safer = ref Safer.Write
let cautious = ref false
let affinity = ref Affinity.No
let force_affinity = ref false
let smtmode = ref Smt.No
let smt = ref 2
let nsockets = ref 1
let alloc = ref Alloc.Dynamic
let doublealloc = ref false
let noalign = ref None
let speedcheck = ref Speedcheck.NoSpeed
let gcc = ref "gcc"
let c11 = ref false
let c11_fence = ref false
let ascall = ref false
let stdio = ref None
let linkopt = ref ""
let targetos = ref TargetOS.Linux
let gas = ref None
let set_gas b = gas := Some b
let asmcomment = ref None
let asmcommentaslabel = ref false
let get_numeric_labels () = match !gas with
| Some b -> b
| None ->
    let open TargetOS in
    begin match !targetos with
    | AIX -> false
    | Linux|Mac|FreeBsd|Android8 -> true
    end
let timeloop = ref (-1)
let set_timeloop i = timeloop :=  i
let kind = ref true
let names = ref []
let excl = ref []
let rename = ref []
let kinds = ref []
let set_kinds s = kinds := !kinds @ [s]
let conds = ref []
let set_conds s = conds := !conds @ [s]
let nstates = ref []
let set_nstates s = nstates := !nstates @ [s]
let exit_cond = ref false
let sleep = ref 0
let isync = ref false
let syncconst = 128
let syncmacro = ref (-1)
let xy = ref false
let morearch = ref MoreArch.No
let carch = ref None
let mode = ref Mode.Std
let usearch = ref UseArch.Trad
let variant = ref (fun _ -> false)

(* Arch dependent options *)
type opt =
    { delay : int; gccopts : string ;
      word : Word.t ; line : int; }

let mod_config = ref (fun cfg -> cfg)

let x86opt =
  { delay = 2048; gccopts="-fomit-frame-pointer -O2";
    word = Word.WXX; line = 512; }
let ppcopt =
  { delay = 1024; gccopts = "-fomit-frame-pointer -O2";
    word = Word.WXX; line = 128; }
let armopt =
  { delay = 1024; gccopts = "-O2";
    word = Word.WXX; line = 64;} (* cortexa9 -> 32, cortex-a15 -> 64 *)
let mipsopt =
  { delay = 1024; gccopts = "-O2";
    word = Word.WXX; line = 1024 ;} (* cache line size cannot be wrong... *)
  
let copt =
  { delay = 2048; gccopts = ""; word = Word.WXX; line = 1024} (* maximal *)
let get_default arch = match arch with
| `X86 |`X86_64 -> x86opt
| `PPCGen
| `PPC -> ppcopt
| `AArch64
| `ARM -> armopt
| `MIPS|`RISCV -> mipsopt
| `C -> copt
| `CPP
| `LISA
| `GPU_PTX 
| `OpenCL -> assert false

let replace_config f =
  let g = !mod_config in
  mod_config := (fun cfg -> f (g cfg))

let get_dependent () = !mod_config

let set_delay i = replace_config (fun o ->  { o with delay = i; })
let get_delay opt = opt.delay

let set_gccopts opts =  replace_config (fun o ->  { o with gccopts = opts; })
let get_gccopts opt = opt.gccopts

let set_word w = replace_config (fun o ->  { o with word = w; })
let get_word opt = opt.word

let set_line w = replace_config (fun o ->  { o with line = w; })
let get_line opt = opt.line

let set_carch x = carch := Some x

(* More *)

let pldw = ref true
let cacheflush = ref true
