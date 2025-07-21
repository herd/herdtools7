(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** select  cycles with at least one atomic specification *)

open Printf

(* Configuration *)
let arch = ref `PPC

let opts = [Util.arch_opt arch]

module Make (F:Fence.S)(A:Atom.S) =
    struct
      module E = Edge.Make(Edge.Config)(F)(A)

      let is_atom es =
        List.exists
          (fun e -> match e.E.a1,e.E.a2 with
          | None,None -> false
          | Some _,_
          | _,Some _ -> true)
          es &&
        List.for_all
          (fun e -> match e with
          | {E.edge=E.Po _; a1=None; a2=None; _} -> false
          | {E.edge=E.Fenced (f,_,_,_); _ } -> not (F.is_isync f)
          | _ -> true)
          es

      let parse_line s =
        try
          let r = String.index s ':' in
          let name  = String.sub s 0 r
          and es = String.sub s (r+1) (String.length s - (r+1)) in
          let es = E.parse_edges es in
          name,es
        with
        | Not_found | Invalid_argument _ ->
            Warn.fatal "bad line: %s" s

      let rec next_line () =
        let line = read_line () in
        match line with
        | "" -> next_line ()
        | _ -> begin match line.[0] with
          | '%'|'#' -> next_line ()
          | _ -> line
        end

      let zyva () =
        try while true do
          try
            let line = next_line () in
            let _,es = parse_line line in
            if is_atom es then  printf "%s\n" line
(*           else eprintf "No: '%s'\n" line *)
          with Misc.Fatal msg -> Warn.warn_always "%s" msg
        done with End_of_file -> ()
    end

let () =
  Util.parse_cmdline
    opts
    (fun _ -> raise (Arg.Bad "No argument"))

let () =
  let (module FenceImpl:Fence.S), (module AtomImpl:Atom.S) =
    match !arch with
    | `X86 -> (module X86Arch_gen),(module X86Arch_gen)
    | `X86_64 -> assert false
    | `PPC ->
        let module M = PPCArch_gen.Make(PPCArch_gen.Config) in
        (module M),(module M)
    | `ARM ->
        let module M = ARMArch_gen.Make(ARMArch_gen.Config) in
        (module M),(module M)
    | `AArch64 ->
        let module M = AArch64Arch_gen.Make(AArch64Arch_gen.Config) in
        (module M),(module M)
    | `MIPS ->
        let module M = MIPSArch_gen.Make(MIPSArch_gen.Config) in
        (module M),(module M)
    | `RISCV ->
        let module M = RISCVArch_gen.Make(RISCVArch_gen.Config) in
        (module M),(module M)
    | `LISA ->
        let module BellConfig = Config.ToLisa(Config) in
        let module M = BellArch_gen.Make(BellConfig) in
        (module M),(module M)
    | `C -> (module CArch_gen),(module CArch_gen)
    | `ASL -> Warn.fatal "ASL arch in atoms"
    | `BPF -> Warn.fatal "BPF arch in atoms"
    | `CPP -> Warn.fatal "CPP arch in atoms"
    | `JAVA -> Warn.fatal "JAVA arch in atoms" in
  let module M = Make(FenceImpl)(AtomImpl) in
  M.zyva ()
