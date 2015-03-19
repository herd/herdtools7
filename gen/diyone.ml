(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Misc
open Printf
open Archs

(* Configuration *)
let norm = ref false
let () = Config.nprocs := 1000
let () = Config.numeric := true

let opts =
  Config.common_specs @
  ("-num", Arg.Bool (fun b -> Config.numeric := b),
   sprintf "<bool> use numeric names, default %b" !Config.numeric)::
  ("-norm",Arg.Set norm," find a mormalised name for me")::
  []



module type Config = sig
  include Top.Config
  include DumpAll.Config
  val norm : bool
  val cpp : bool    
  val docheck : bool
end

module Make(O:Config) (M:Builder.S) =
  struct

    let dump_stdout es =
      let t = M.make_test "A" es in
      M.dump_test_channel stdout t ;
      None

    let litmus =
      if O.cpp then sprintf "%s.c"
      else sprintf "%s.litmus"

    let dump_file name es =
      if O.verbose > 0 then eprintf "Test name: %s\n" name ;
      let t = M.make_test name es in
      let fname = litmus name in
      Misc.output_protect
        (fun chan -> M.dump_test_channel chan t; Some fname)
        fname

    let dump =
      if O.norm then
        let module Namer = Namer.Make(M.A)(M.E) in
        let module Normer = Normaliser.Make(O)(M.E) in
        fun _name es ->
	  let es = M.E.resolve_edges es in
          let base,es = Normer.normalise_family es in
          let name = Namer.mk_name base es in
          dump_file name es
      else
        fun name  -> match name with
        | None -> fun es -> dump_stdout (M.E.resolve_edges es)
        | Some name -> fun es -> dump_file name (M.E.resolve_edges es)

    let parse_line s =
      try
        let r = String.index s ':' in
        let name  = String.sub s 0 r
        and es = String.sub s (r+1) (String.length s - (r+1)) in
        let es = LexUtil.split es in
        let es = M.R.parse_relaxs es in
        let es =
          List.fold_right
            (fun r k -> M.R.edges_of r @ k)
            es [] in
        name,es
      with
      | Not_found | Invalid_argument _ ->
          Warn.fatal "bad line: %s" s

(********)
    let do_zyva name pp_rs =
      try begin
        let pp_rs = List.map LexUtil.split pp_rs in
        let pp_rs = List.concat pp_rs in
        let rs = List.map M.R.parse_relax pp_rs in
        let es =
          List.fold_right
            (fun r k -> M.R.edges_of r @ k)
            rs [] in
        match es with
        | [] ->
            let dump_names =
              O.norm ||
              (match O.family with Some _ -> true | None -> false) in
            let module D = DumpAll.Make(O)(M) in
            let gen kont =
              let rec do_rec k0 =
                let k = 
                  try
                    let line = read_line () in
                    try
                      let name,es = parse_line line in
                      let mk_name =
                        if dump_names then  D.no_name
                        else (fun _ -> Some name) in
                      Some (kont es D.no_info mk_name k0)
                    with
                    | Fatal msg | UserError msg ->
                        Warn.warn_always "%s on line '%s'" msg line ;
                        Some k0
                  with
                  | End_of_file -> None in
                match k with
                | None -> k0
                | Some k -> do_rec k in
              do_rec in
            D.all gen
        | _ -> ignore (dump name es)
      end with Fatal msg ->
        eprintf "%s: Fatal error: %s\n" Config.prog msg ;
        exit 2

    let zyva = do_zyva O.family

  end

let pp_es = ref []

let () =
  Util.parse_cmdline
    opts
    (fun x -> pp_es := x :: !pp_es)

let pp_es = List.rev !pp_es
let cpp = match !Config.arch with
| CPP -> true
| _ -> false

let () =
  let module V = SymbConstant in
  let module Co = struct
(* Dump all *)
    let verbose = !Config.verbose
    let hout = match !Config.hout with
    | None -> Hint.none
    | Some n -> Hint.open_out n
    let family = !Config.name
    let canonical_only = !Config.canonical_only
    let fmt = !Config.fmt
    let no = match !Config.no with
    | None -> []
    | Some fname -> Config.read_no fname
    let cond = !Config.cond
    let tarfile = !Config.tarfile
    let addnum = !Config.addnum
    let numeric = !Config.numeric
    let lowercase = !Config.lowercase
    let coherence_decreasing = !Config.coherence_decreasing
    let optcoherence = !Config.optcoherence
    let optcond = !Config.optcond
    let poll = !Config.poll
    let fno = !Config.fno
    let overload = !Config.overload
    let obs_type = !Config.obs_type
    let do_observers = !Config.do_observers
    let eprocs = !Config.eprocs
    let nprocs = !Config.nprocs
    let neg = !Config.neg
(* Specific *)
    let norm = !norm
    let cpp = cpp
    let docheck = !Config.docheck
  end in
  let module Build = Make(Co) in
  let module C = struct
    let verbose = !Config.verbose
    let list_edges = !Config.list_edges
    let coherence_decreasing = !Config.coherence_decreasing
    let same_loc =
      !Config.same_loc ||
      (match Co.cond with
      | Config.Unicond -> true
      | _ -> false)
    let sta = !Config.sta
    let unrollatomic = !Config.unrollatomic
    let allow_back = true
  end in
  (match !Config.arch with
  | X86 ->
      let module T = Top.Make(Co) in
      let module M = Build(T(X86Compile.Make(V)(C))) in
      M.zyva
  | PPC ->
      let module T = Top.Make(Co) in
      let module M = Build(T(PPCCompile.Make(V)(C)(PPCArch.Config))) in
      M.zyva
  | ARM ->
      let module T = Top.Make(Co) in
      let module M = Build(T(ARMCompile.Make(V)(C))) in
      M.zyva
  | AArch64 ->
      let module T = Top.Make(Co) in
      let module M = Build(T(AArch64Compile.Make(V)(C))) in
      M.zyva
  | MIPS ->
      let module T = Top.Make(Co) in
      let module M = Build(T(MIPSCompile.Make(V)(C))) in
      M.zyva
  | C|CPP as a ->
      let module CoC = struct
        include Co
        include C
        let typ = !Config.typ
        let cpp = match a with CPP -> true | _ -> false
      end in
      let module T = CCompile.Make(CoC) in
      let module M = Build(T) in
      M.zyva
)
    pp_es

