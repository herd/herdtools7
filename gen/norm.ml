(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2018-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Normalise cycles and name them *)
open Printf

let arch = ref `PPC
let lowercase = ref false
let bell = ref None
let variant = ref (fun (_:Variant_gen.t) -> false)
let args = ref []

let opts =
  ("-lowercase", Arg.Bool (fun b -> lowercase := b),
   sprintf "<bool> use lowercase familly names, default %b" !lowercase)::
  ("-bell",Arg.String (fun f -> bell := Some f; arch := `LISA),
   "<name> read bell file <name>")::
  Util.parse_tag
    "-arch"
    (fun tag -> match Archs.parse tag with
    | None -> false
    | Some a -> arch := a ; true)
    Archs.tags "specify architecture"::
    Util.parse_tag
    "-variant"
    (fun tag -> match Variant_gen.parse tag with
    | None -> false
    | Some v0 ->
        let ov = !variant in variant := (fun v -> v = v0 || ov v) ;
        true)
    Variant_gen.tags
    (sprintf "specify variant")::
  []



module type Config = sig
  val lowercase : bool
  val sufname : string option
  val variant : Variant_gen.t -> bool
end

module Make(Co:Config) (A:Fence.S) = struct
  module E = Edge.Make(Co)(A)
  module N = Namer.Make(A)(E)
  module Norm = Normaliser.Make(Co)(E)


  let zyva es =
    try
      let es = List.map E.parse_edge es in
      let base,es,_ = Norm.normalise_family (E.resolve_edges es) in
      let name =  N.mk_name base ?scope:None es in
      Printf.printf "%s: %s\n" name (E.pp_edges es)
    with Misc.Fatal msg ->
      eprintf "Fatal error: %s\n" msg ;
      exit 2

end

let () =
  Util.parse_cmdline
    opts
    (fun a -> args := a :: !args)


let () =
  let args = List.rev !args in
  let module Co = struct
    let lowercase = !lowercase
    let sufname = None
    let variant = !variant
  end in
  let module Build = Make(Co) in
  (match !arch with
  | `X86 ->
      let module M = Build(X86Arch_gen) in
      M.zyva
  | `X86_64 -> assert false
  | `PPC ->
      let module M = Build(PPCArch_gen.Make(PPCArch_gen.Config)) in
      M.zyva
  | `ARM ->
      let module M = Build(ARMArch_gen.Make(ARMArch_gen.Config)) in
      M.zyva
  | `AArch64 ->
      let module A =
        AArch64Arch_gen.Make
          (struct
            include AArch64Arch_gen.Config
            let moreedges = !Config.moreedges
          end) in
      let module M = Build(A) in
      M.zyva
  | `MIPS ->
      let module M = Build(MIPSArch_gen.Make(MIPSArch_gen.Config)) in
      M.zyva
  | `RISCV ->
      let module M = Build(RISCVArch_gen.Make(RISCVArch_gen.Config)) in
      M.zyva
  | `LISA ->
      let module BellConfig =
	struct
	  let debug = !Config.debug
	  let verbose = !Config.verbose
	  let libdir = Version_gen.libdir
	  let prog = Config.prog
	  let bell = !bell
	  let varatom = []
	end in
      let module M = Build(BellArch_gen.Make(BellConfig)) in
      M.zyva
  | `C | `CPP ->
      let module M = Build(CArch_gen) in
      M.zyva)
    args
