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
let typ = ref TypBase.default
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
        Util.parse_tag
    "-type"
    (fun tag -> match TypBase.parse tag with
    | None -> false
    | Some a -> typ := a ; true)
    TypBase.tags
    (sprintf "specify base type, default %s" (TypBase.pp !typ))::
  []



module type Config = sig
  val lowercase : bool
  val variant : Variant_gen.t -> bool
  val naturalsize : MachSize.sz
  val wildcard : bool
end

module Make(Co:Config)(F:Fence.S)(A:Atom.S) = struct
  module E = Edge.Make(Co)(F)(A)
  module R = Relax.Make(F)(E)
  module N = Namer.Make(F)(A)(E)
  module Norm = Normaliser.Make(Co)(E)


  let zyva relaxs =
    try
      let rs =
        relaxs
        |> Util.List.concat_map LexUtil.split
        |> List.map R.parse_relax
      in
      let es = Util.List.concat_map R.edges_of rs in
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
    let variant = !variant
    let naturalsize = TypBase.get_size !typ
    let wildcard = false
  end in
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
      let module M =
        AArch64Arch_gen.Make
          (struct
            include AArch64Arch_gen.Config
            let moreedges = !Config.moreedges
          end) in
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
    | `C | `CPP -> (module CArch_gen),(module CArch_gen)
    | `JAVA | `ASL | `BPF -> assert false in
  let module M = Make(Co)(FenceImpl)(AtomImpl) in
  M.zyva args
