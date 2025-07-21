(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
open Code

(* Configuration *)
let arch = ref `PPC

let opts = [Util.arch_opt arch]

module Make (F:Fence.S)(A:Atom.S) =
    struct
      module E = Edge.Make(Edge.Config)(F)(A)
      module Namer = Namer.Make(F)(A)(E)
      module Normer =
        Normaliser.Make(struct let lowercase = false end)(E)


      let is_ext e = match E.get_ie e with
      | Ext -> true
      | Int -> false
      | UnspecCom -> assert false

      let atomic = Some A.default_atom

      let atomize es =
        match es with
        | [] -> []
        | fst::_ ->
            let rec do_rec es = match es with
              | [] -> []
              | [e] ->
                  if E.is_ext fst || E.is_ext e then
                    [ { e with E.a2 = atomic ; } ]
                  else
                    es
              | e1::e2::es ->
                  if E.is_ext e1 || E.is_ext e2 then
                    let e1 = { e1 with E.a2 = atomic; } in
                    let e2 = { e2 with E.a1 = atomic; } in
                    e1::do_rec (e2::es)
                  else e1::do_rec (e2::es) in
            match do_rec es with
            | [] -> assert false
            | fst::rem as es ->
                let lst = Misc.last es in
                if is_ext fst || is_ext lst then
                  { fst with E.a1 = atomic;}::rem
                else es

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

      let pp_edges es = String.concat " " (List.map E.pp_edge es)

      let zyva_stdin () =
        try while true do
          try
            let line = read_line () in
            let _,es = parse_line line in
            let base,es,_ = Normer.normalise_family (atomize es) in
            let name = Namer.mk_name base es in
            printf "%s: %s\n" name (pp_edges es)
          with Misc.Fatal msg -> Warn.warn_always "%s" msg
        done with End_of_file -> ()

      let zyva_argv es =
        let es = List.map E.parse_edge es in
        let es = atomize es in
        printf "%s\n" (pp_edges es)

      let zyva = function
        | [] -> zyva_stdin ()
        | es ->  zyva_argv es
    end

let pp_es = ref []

let () =
  Util.parse_cmdline
    opts
    (fun x -> pp_es := x :: !pp_es)

let pp_es = List.rev !pp_es

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
    | `ASL -> Warn.fatal "ASL arch in atomize"
    | `BPF -> Warn.fatal "BPF arch in atomize"
    | `CPP -> Warn.fatal "CPP arch in atomize"
    | `JAVA -> Warn.fatal "JAVA arch in atomize" in
  let module M = Make(FenceImpl)(AtomImpl) in
  M.zyva pp_es
