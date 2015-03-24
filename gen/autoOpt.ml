(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf

(* Main mode *)
type mode = Explo | Conform

let parse_mode s = match String.lowercase s with
| "explo" -> Some Explo
| "conform" -> Some Conform
| _ -> None

let pp_mode = function
  | Explo -> "explo"
  | Conform -> "conform"


(* Execution *)

type mach =
  | Local
  | Distant of string
  | Simul of string * string
  | Cross of string * string

let split_space s =
  try
    let i = String.index s ' ' in
    String.sub s 0 i,
    String.sub s (i+1) (String.length s - (i+1))
  with Not_found -> s,""
  
let parse_mach s =

  let key,v = split_space s in
  match String.lowercase key with
  | "local" -> Some Local
  | "ssh" -> Some (Distant v)
  | ("memevents"|"ppcmem"|"herd") as prog ->
      Some (Simul (prog,v))
  | "cross" ->
      let v1,v2 = split_space v in
      Some (Cross (v1,v2))
  | _ -> None
        
let sp s = if s = "" then "" else " "

let pp_mach = function
  | Local -> "local"
  | Distant addr -> sprintf "ssh %s" addr
  | Simul (prog,opts) ->
      sprintf "%s%s%s" prog (sp opts) opts
  | Cross (addr1,addr2) -> sprintf "cross %s %s" addr1 addr2 
(* Interpretation *)

type interpretation = Single | Multi

let parse_interpretation s = match String.lowercase s with
| "single" -> Some Single
| "multi" -> Some Multi
| _ -> None

let pp_interpretation = function
  | Single -> "single"
  | Multi -> "multi"

(* Checkpoint *)

let ckpt_name = "ckpt"

type t =
  {
   arch : Archs.t ;
   output : string ; (* Directory for all output *)
   testing : string option ;
   safe : string option ;
   mode : mode ;
   mach : mach ;
   interpretation : interpretation ;
   work_dir : string ;
   nprocs : int option ;
   diy_sz : int option ;
   litmus_opts : string ;
   run_opts : string list ;
   verbose : int ;
   interactive : bool ;
   force_interactive : bool ;
   build : string ;
   stabilise : int ;
   compress : bool ;
   distrm : string ;
   distaddpath : string list ;
   diy_opts : string list ;
   transitive : bool ;
  }

let pp_opt chan t =
  let pp_opt pp = function
    | None -> "-"
    | Some x -> pp x in
  let pp_oint = pp_opt string_of_int in

  let p = fprintf chan in
  p "arch = %s\n" (Archs.pp t.arch) ;
  p "nprocs = %s\n" (pp_oint t.nprocs) ;
  p "diy_sz = %s\n" (pp_oint t.diy_sz) ;
  ()

let  default =
  {
   arch = Archs.X86 ;
   output = "." ;
   testing = None ;
   safe = None ;
   mach = Local ;
   mode = Explo ;
   interpretation = Single ;
   work_dir = "/var/tmp" ;
   nprocs = None ;
   diy_sz = None ;
   litmus_opts = "" ;
   run_opts = ["";] ;
   verbose = 0 ;
   interactive = true ;
   force_interactive = false ;
   build = "sh comp.sh" ;
   stabilise = 5 ;
   compress = true ;
   distrm = "/bin/rm" ;
   distaddpath = [] ;
   diy_opts = [] ;
   transitive = false ;
  }

let incr_verbose opts = { opts with verbose = opts.verbose + 1 ; }

let set_interactive b opts = { opts with interactive = b ; }

let set_transitive b opts = { opts with transitive = b ; }

let set_compress b opts = { opts with compress = b ; }

let set_arch arg cfg = match Archs.parse arg with
| None -> raise (Arg.Bad (sprintf "unkown architecture: %s" arg))
| Some a -> { cfg with arch =a; } 

let set_mode arg cfg = match parse_mode arg with
| None -> raise (Arg.Bad (sprintf "bad mode: %s" arg))
| Some m -> { cfg with mode = m ; }

let set_nprocs i cfg = { cfg with nprocs = Some i; }

let get_nprocs a cfg = match cfg.nprocs with
| None ->
    begin
      let open Archs in
      match a with
      | PPC -> 4
      | X86 -> 2
      | ARM -> 2
      | MIPS -> 2
      | AArch64 -> 2
      | C|CPP -> 2
    end
| Some i -> i

let set_diy_opts s t = { t with diy_opts = s :: t.diy_opts; }
let get_diy_opts t = t.diy_opts
